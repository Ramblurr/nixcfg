// Start upstream's offline server directly; Electron is not loaded.
const { mkdirSync } = require("node:fs");
const { homedir } = require("node:os");
const path = require("node:path");
const { parseArgs } = require("node:util");

let values;
try {
  ({ values } = parseArgs({
    options: {
      port: { type: "string", default: "8080" },
      help: { type: "boolean", short: "h" },
    },
  }));
  if (!/^[0-9]+$/.test(values.port) || Number(values.port) < 1 || Number(values.port) > 65535) {
    throw new Error("--port must be an integer between 1 and 65535");
  }
} catch (error) {
  console.error(`keybr: ${error.message}\nUsage: keybr [--port PORT]`);
  process.exit(1);
}

if (values.help) {
  process.stdout.write("Usage: keybr [--port PORT]\n\nServe offline Keybr on localhost (default port: 8080).\nUses the same XDG data as keybr-standalone. Stop with Ctrl-C.\n");
  process.exit(0);
}

function directory(variable, fallback) {
  const value = process.env[variable];
  const base = value && path.isAbsolute(value) ? value : path.join(homedir(), fallback);
  const result = path.join(base, "keybr-standalone");
  mkdirSync(result, { recursive: true, mode: 0o700 });
  return result;
}

const config = directory("XDG_CONFIG_HOME", ".config");
const data = directory("XDG_DATA_HOME", ".local/share");
const port = String(Number(values.port));
Object.assign(process.env, {
  NODE_ENV: "production",
  DESKTOP_MODE: "true",
  SERVER_PORT: port,
  APP_URL: `http://localhost:${port}/`,
  PUBLIC_DIR: path.join(__dirname, "root/public"),
  DATA_DIR: path.join(data, "data"),
  DATABASE_CLIENT: "sqlite",
  DATABASE_FILENAME: path.join(data, "database.sqlite"),
  COOKIE_SECURE: "false",
  COOKIE_DOMAIN: "",
});
// Like the desktop launcher, only probe dotenv files in the app's config directory.
process.chdir(config);
process.stdout.write(`Keybr: http://localhost:${port}/ (Ctrl-C to stop)\n`);
require("./root/lib/desktop.js");
