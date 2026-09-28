// Exercise the installed app, its native SQLite server, and real Electron paths.
const assert = require("node:assert/strict");
const { existsSync, readdirSync } = require("node:fs");
const path = require("node:path");
const { app } = require("electron");

const timeout = setTimeout(() => {
  console.error("Keybr did not load within 60 seconds");
  app.once("will-quit", () => app.exit(1));
  app.quit();
}, 60000);

app.on("browser-window-created", (_event, window) => {
  window.webContents.once("did-finish-load", async () => {
    try {
      const base = process.env.TEST_XDG_BASE;
      const expected = {
        userData: path.join(base ? path.join(base, "config") : path.join(process.env.HOME, ".config"), "keybr-standalone"),
        sessionData: path.join(base ? path.join(base, "data") : path.join(process.env.HOME, ".local/share"), "keybr-standalone/browser"),
        logs: path.join(base ? path.join(base, "state") : path.join(process.env.HOME, ".local/state"), "keybr-standalone/logs"),
        crashDumps: path.join(base ? path.join(base, "state") : path.join(process.env.HOME, ".local/state"), "keybr-standalone/crashes"),
      };
      assert.deepEqual(Object.fromEntries(Object.keys(expected).map(key => [key, app.getPath(key)])), expected);
      assert.equal(app.commandLine.getSwitchValue("disk-cache-dir"), path.join(base ? path.join(base, "cache") : path.join(process.env.HOME, ".cache"), "keybr-standalone"));
      const data = path.join(base ? path.join(base, "data") : path.join(process.env.HOME, ".local/share"), "keybr-standalone");
      assert.ok(existsSync(path.join(data, "database.sqlite")), "SQLite database missing from XDG data directory");
      assert.equal(process.cwd(), expected.userData);
      const text = await window.webContents.executeJavaScript(`new Promise((resolve, reject) => {
        const deadline = Date.now() + 15000;
        const poll = () => {
          const text = document.body.innerText;
          if (/keybr|typing/i.test(text)) return resolve(text);
          if (Date.now() > deadline) return reject(new Error(document.body.innerHTML));
          setTimeout(poll, 100);
        };
        poll();
      })`);
      assert.match(text, /keybr|typing/i);
      assert.deepEqual(readdirSync(process.env.HOME).filter(name => ![".config", ".cache", ".local"].includes(name)), []);
      clearTimeout(timeout);
      app.quit();
    } catch (error) {
      console.error(error);
      app.once("will-quit", () => app.exit(1));
      app.quit();
    }
  });
});

require(path.join(process.env.APP_ROOT, "desktop/main.js"));
