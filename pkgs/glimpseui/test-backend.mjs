import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { accessSync, constants } from 'node:fs';

assert.equal(process.env.GLIMPSE_BACKEND, 'chromium');
assert.equal(process.env.GLIMPSE_CHROME_PATH, process.env.GLIMPSE_EXPECTED_CHROME_PATH);
assert.equal(process.env.GLIMPSE_BINARY_PATH, undefined);
assert.equal(process.env.GLIMPSE_HOST_PATH, undefined);
accessSync(process.env.GLIMPSE_CHROME_PATH, constants.X_OK);

const runtimeCheck = spawnSync(
  'python3',
  [
    '-c',
    `
import ctypes
import gi
import sys

gi.require_version('Gtk', '3.0')
gi.require_version('AyatanaAppIndicator3', '0.1')
from gi.repository import AyatanaAppIndicator3, Gtk

for library in sys.argv[1:]:
    ctypes.cdll.LoadLibrary(library)
`,
    process.env.GLIMPSE_EXPECTED_LIBX11_PATH,
    process.env.GLIMPSE_EXPECTED_LIBXFIXES_PATH,
  ],
  { encoding: 'utf8' },
);
assert.equal(runtimeCheck.status, 0, runtimeCheck.stderr);

const { getNativeHostInfo } = await import(process.env.GLIMPSE_MODULE_PATH);
const host = getNativeHostInfo();

assert.equal(host.platform, 'linux-chromium');
assert.equal(host.path, process.execPath);
assert.match(host.extraArgs?.[0] ?? '', /chromium-backend\.mjs$/);
