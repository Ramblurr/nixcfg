// Synthetic credentials only. Runs the installed native libsignal binary, not Signal.
// Usage: node tests/signal-auth-salt.cjs /absolute/path/to/libsignal-client.node /path/to/Signal-Desktop
const assert = require('node:assert/strict');
const Native = require(process.argv[2]);
const random = new Uint8Array(32).fill(1);
const server = { _nativeHandle: Native.ServerSecretParams_GenerateDeterministic(random) };
const publicParams = { _nativeHandle: Native.ServerSecretParams_GetPublicParams(server) };
const aci = new Uint8Array(17);
aci[16] = 1;
const salt = Uint8Array.from({ length: 16 }, (_, i) => i);
const redemptionTime = 123456 * 86400;
const response = Native.ServerSecretParams_IssueAuthCredentialZkcWithoutPniDeterministic(
  server, random, aci, salt, redemptionTime
);
const receive = value => Native.ServerPublicParams_ReceiveAuthCredentialZkcWithoutPni(
  publicParams, aci, value, redemptionTime, response
);
const expected = receive(salt);
assert.ok(expected instanceof Uint8Array);
console.log('PASS: valid synthetic credential accepted with original Uint8Array salt');

// Signal 8.26.0 Client.preload.ts has no ITEM_SPECS entry for authCredentialSalt.
// Server.node.ts persists items through JSON.stringify/JSON.parse (util.std.ts).
const persisted = JSON.stringify({ id: 'authCredentialSalt', value: salt });
const loaded = JSON.parse(persisted).value;
assert.equal(loaded.constructor, Object);
assert.throws(() => receive(loaded), /failed to downcast any to Uint8Array/);
console.log('REPRODUCED: JSON-loaded salt causes the exact native Uint8Array downcast error');
assert.deepEqual(receive(Uint8Array.from(Object.values(loaded))), expected);
console.log('PASS: restoring the original bytes yields the identical valid credential');

// Apply the candidate patch to a disposable source copy and execute its actual helper.
// Third argument: local Signal-Desktop checkout at v8.26.0.
const fs = require('node:fs');
const path = require('node:path');
const os = require('node:os');
const { execFileSync } = require('node:child_process');
const { stripTypeScriptTypes } = require('node:module');
const source = process.argv[3];
assert.ok(source, 'Supply a local Signal-Desktop v8.26.0 checkout as the third argument');
const client = fs.readFileSync(path.join(source, 'ts/sql/Client.preload.ts'), 'utf8');
const specs = client.match(/const ITEM_SPECS:[^=]+=(\s*\{[\s\S]*?\n\});/)[1];
assert.ok(!specs.includes('authCredentialSalt'), 'Reassess: upstream storage mapping changed');
const temporary = fs.mkdtempSync(path.join(os.tmpdir(), 'signal-auth-salt-'));
try {
  const relative = 'ts/services/groupCredentialFetcher.preload.ts';
  fs.mkdirSync(path.dirname(path.join(temporary, relative)), { recursive: true });
  fs.copyFileSync(path.join(source, relative), path.join(temporary, relative));
  execFileSync('patch', ['--batch', '--fuzz=0', '-p1', '-i',
    path.resolve(__dirname, '../patches/signal-auth-salt.patch')], { cwd: temporary });
  const patched = fs.readFileSync(path.join(temporary, relative), 'utf8');
  assert.ok(patched.includes("const salt = restoreAuthCredentialSalt(\n        itemStorage.get('authCredentialSalt')"));
  const helper = patched.slice(patched.indexOf('function restoreAuthCredentialSalt('));
  assert.ok(!patched.includes('[DEBUG-signal-salt-shape]'));
  const restore = Function('strictAssert',
    stripTypeScriptTypes(helper) + '\nreturn restoreAuthCredentialSalt;')(assert.ok);
  assert.equal(restore(salt), salt);
  assert.deepEqual(restore(loaded), salt);
  assert.deepEqual(receive(restore(loaded)), expected);
  assert.deepEqual(receive(restore(JSON.parse(JSON.stringify(restore(loaded))))), expected);
  console.log('PASS: actual patched helper accepts native credentials after repeated reloads');
  for (const invalid of [undefined, null, [], {}, 'invalid', new Uint8Array(15),
    { ...loaded, 0: -1 }, { ...loaded, 0: 256 }, { ...loaded, 0: 1.5 },
    { ...loaded, 0: '1' }, { ...loaded, extra: 0 },
    Object.fromEntries(Object.entries(loaded).filter(([key]) => key !== '0'))]) {
    assert.throws(() => restore(invalid));
  }
  const bufferJson = JSON.parse(JSON.stringify(Buffer.from(salt)));
  assert.throws(() => receive(bufferJson), /failed to downcast any to Uint8Array/);
  assert.deepEqual(receive(restore(bufferJson)), expected);
  assert.deepEqual(receive(restore(JSON.parse(JSON.stringify(restore(bufferJson))))), expected);
  console.log('PASS: observed Buffer JSON representation restores valid native credentials');
  for (const invalid of [
    { type: 'Buffer', data: Array(15).fill(0) },
    { type: 'Buffer', data: Array(17).fill(0) },
    { type: 'Buffer', data: [...Array(15).fill(0), 256] },
    { type: 'Buffer', data: [...Array(15).fill(0), -1] },
    { type: 'Buffer', data: [...Array(15).fill(0), 0.5] },
    { type: 'Buffer', data: [...Array(15).fill(0), '1'] },
    { type: 'Buffer', data: new Array(16) },
    { type: 'Other', data: Array(16).fill(0) },
    { type: 'Buffer', data: Array(16).fill(0), extra: true },
  ]) assert.throws(() => restore(invalid));
  console.log('PASS: malformed salts rejected; no profile or network access');
} finally {
  fs.rmSync(temporary, { recursive: true });
}
