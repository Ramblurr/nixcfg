'use strict';

const assert = require('node:assert/strict');
const { createHash } = require('node:crypto');
const { once } = require('node:events');
const { mkdtemp, readFile, readdir, rm, writeFile } = require('node:fs/promises');
const { createServer } = require('node:http');
const { tmpdir } = require('node:os');
const { join } = require('node:path');
const { test } = require('node:test');
const { setImmediate, setTimeout } = require('node:timers/promises');

const MiB = 1024 ** 2;
const image = Buffer.alloc(256 * 1024, 42);
const digest = (data) => createHash('sha256').update(data).digest('hex');

async function collect() {
  assert.equal(typeof global.gc, 'function', 'Run the local regression with --expose-gc');
  for (let i = 0; i < 5; i++) {
    await setImmediate();
    global.gc();
  }
}

async function readBlob(mode) {
  const blob = new Blob([image]);
  if (mode === 'arrayBuffer') {
    assert.equal((await blob.arrayBuffer()).byteLength, image.length);
  } else if (mode === 'abandoned') {
    blob.stream();
  } else if (mode === 'cancelled') {
    const reader = blob.stream().getReader();
    assert.equal((await reader.read()).done, false);
    await reader.cancel();
    reader.releaseLock();
  } else {
    let size = 0;
    for await (const chunk of blob.stream()) size += chunk.byteLength;
    assert.equal(size, image.length);
  }
}

for (const mode of ['arrayBuffer', 'consumed', 'cancelled', 'abandoned']) {
  test(`Blob reader releases ${mode} backing storage`, async (t) => {
    await collect();
    const before = process.memoryUsage();
    for (let i = 0; i < 128; i++) await readBlob(mode);
    await collect();
    const after = process.memoryUsage();
    t.diagnostic(JSON.stringify({ mode, before, after }));
    // The old native callback keeps all 32MiB alive even after explicit GC.
    assert.ok(after.arrayBuffers - before.arrayBuffers < 4 * MiB, 'Blob backing storage retained');
  });
}

test('Immich ML uploads preserve content, results and retry bodies without retention', { timeout: 300_000 }, async (t) => {
  assert.ok(process.env.IMMICH_PACKAGE, 'Set IMMICH_PACKAGE to the built Immich package');
  const iterations = Number(process.env.IMMICH_UPLOAD_ITERATIONS || 128);
  assert.ok(Number.isSafeInteger(iterations) && iterations > 0 && iterations <= 10_000);
  // Execute the packaged application, not a copy of its upload implementation.
  const { MachineLearningRepository } = require(join(
    process.env.IMMICH_PACKAGE, 'lib/node_modules/immich/dist/repositories/machine-learning.repository.js',
  ));
  const directory = await mkdtemp(join(tmpdir(), 'immich-blob-test-'));
  const imagePath = join(directory, 'preview.bin');
  const errors = [];
  const received = [];
  let captureRequests = true;
  let requestCount = 0;
  let mutateOnFailure = false;
  const server = createServer(async (request, response) => {
    try {
      const body = await new Request(`http://localhost${request.url}`, {
        method: request.method, headers: request.headers, body: request, duplex: 'half',
      }).formData();
      const entries = JSON.parse(body.get('entries'));
      requestCount++;
      if (captureRequests) received.push({ path: request.url, entries });
      if (body.has('image')) {
        const file = body.get('image');
        assert.deepEqual({ name: file.name, type: file.type, size: file.size }, {
          name: 'blob', type: 'application/octet-stream', size: image.length,
        });
        assert.equal(digest(Buffer.from(await file.arrayBuffer())), digest(image));
      } else {
        assert.equal(body.get('text'), 'transport regression');
      }
      if (request.url === '/failure/predict') {
        if (mutateOnFailure) await writeFile(imagePath, Buffer.alloc(image.length, 99));
        response.writeHead(503).end('{}');
      } else {
        response.setHeader('content-type', 'application/json');
        response.end(JSON.stringify({
          clip: 'embedding', 'facial-recognition': [], ocr: { text: [], box: [], boxScore: [], textScore: [] },
          imageHeight: 8, imageWidth: 16,
        }));
      }
    } catch (error) {
      errors.push(error.message);
      response.writeHead(500).end('{}');
    }
  });
  const repository = new MachineLearningRepository({ setContext() {}, log() {}, warn() {} });
  try {
    await writeFile(imagePath, image);
    server.listen(0, '127.0.0.1');
    await once(server, 'listening');
    const url = `http://127.0.0.1:${server.address().port}/`;
    const configure = (urls) => repository.setup({
      enabled: true, urls, availabilityChecks: { enabled: false },
    });
    configure([url]);
    assert.equal(await repository.encodeImage(imagePath, { modelName: 'clip-test' }), 'embedding');
    assert.deepEqual(await repository.detectFaces(imagePath, { modelName: 'face-test', minScore: 0.7 }), {
      imageHeight: 8, imageWidth: 16, faces: [],
    });
    assert.deepEqual(await repository.ocr(imagePath, {
      modelName: 'ocr-test', minDetectionScore: 0.5, minRecognitionScore: 0.6, maxResolution: 736,
    }), { text: [], box: [], boxScore: [], textScore: [] });
    assert.equal(await repository.encodeText('transport regression', { modelName: 'clip-test', language: 'en' }), 'embedding');
    assert.deepEqual(received.map((row) => row.entries), [
      { clip: { visual: { modelName: 'clip-test' } } },
      { 'facial-recognition': { detection: { modelName: 'face-test', options: { minScore: 0.7 } }, recognition: { modelName: 'face-test' } } },
      { ocr: { detection: { modelName: 'ocr-test', options: { minScore: 0.5, maxResolution: 736 } }, recognition: { modelName: 'ocr-test', options: { minScore: 0.6 } } } },
      { clip: { textual: { modelName: 'clip-test', options: { language: 'en' } } } },
    ]);

    const attemptsBeforeMissing = received.length;
    await assert.rejects(repository.encodeImage(join(directory, 'missing'), { modelName: 'clip-test' }), { code: 'ENOENT' });
    assert.equal(received.length, attemptsBeforeMissing);
    configure([`${url}failure/`, url]);
    mutateOnFailure = true;
    assert.equal(await repository.encodeImage(imagePath, { modelName: 'clip-test' }), 'embedding');
    assert.deepEqual(received.slice(-2).map((row) => row.path), ['/failure/predict', '/predict']);
    // The retry must reuse the bytes already read, even when the source changed.
    assert.equal((await readFile(imagePath))[0], 99);
    mutateOnFailure = false;
    await writeFile(imagePath, image);
    configure([url]);

    captureRequests = false;
    await collect();
    const before = process.memoryUsage();
    const beforeFd = (await readdir('/proc/self/fd')).length;
    await Promise.all(Array.from({ length: 3 }, async () => {
      for (let i = 0; i < iterations; i++) {
        assert.equal(await repository.encodeImage(imagePath, { modelName: 'clip-test' }), 'embedding');
        if (i % 512 === 0) {
          const memory = process.memoryUsage();
          assert.ok(memory.rss - before.rss < 384 * MiB, 'Bounded test RSS guard exceeded');
          t.diagnostic(JSON.stringify({ phase: 'active', requests: requestCount, memory }));
        }
      }
    }));
    await collect();
    const after = process.memoryUsage();
    const afterFd = (await readdir('/proc/self/fd')).length;
    assert.deepEqual(errors, []);
    assert.equal(requestCount, attemptsBeforeMissing + 2 + 3 * iterations);
    t.diagnostic(JSON.stringify({ phase: 'packaged-immich', before, after, beforeFd, afterFd, requests: requestCount }));
    assert.ok(after.arrayBuffers - before.arrayBuffers < 8 * MiB, 'ML payload buffers retained');
    assert.ok(after.heapUsed - before.heapUsed < 16 * MiB, 'ML request objects retained');
    // Three simultaneous clients can add sockets; there must not be per-job FDs.
    assert.ok(afterFd - beforeFd <= 12, 'File descriptors accumulated');
  } finally {
    repository.teardown();
    server.closeAllConnections();
    await new Promise((resolve) => server.close(resolve));
    await rm(directory, { recursive: true, force: true });
    await setTimeout(50);
  }
});
