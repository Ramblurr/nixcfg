// Run before Electron acquires its instance lock or creates a browser session.
const { app } = require("electron");
const { mkdirSync } = require("node:fs");
const { homedir } = require("node:os");
const path = require("node:path");

function directory(variable, fallback) {
  const value = process.env[variable];
  const base = value && path.isAbsolute(value) ? value : path.join(homedir(), fallback);
  const result = path.join(base, "keybr-standalone");
  mkdirSync(result, { recursive: true, mode: 0o700 });
  return result;
}

const config = directory("XDG_CONFIG_HOME", ".config");
const data = directory("XDG_DATA_HOME", ".local/share");
const cache = directory("XDG_CACHE_HOME", ".cache");
const state = directory("XDG_STATE_HOME", ".local/state");
app.setPath("userData", config);
// Browser storage includes persistent settings, not just disposable caches.
const browserData = path.join(data, "browser");
mkdirSync(browserData, { recursive: true, mode: 0o700 });
app.setPath("sessionData", browserData);
app.commandLine.appendSwitch("disk-cache-dir", cache);
app.setPath("crashDumps", path.join(state, "crashes"));
app.setAppLogsPath(path.join(state, "logs"));
// Never probe .env files in the directory from which the app was launched.
process.chdir(config);
module.exports = { data };
