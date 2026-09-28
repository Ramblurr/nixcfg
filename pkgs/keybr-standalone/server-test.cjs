const assert = require("node:assert/strict");
const { spawn, spawnSync } = require("node:child_process");
const { once } = require("node:events");
const { mkdtempSync, mkdirSync, existsSync, readdirSync, readFileSync } = require("node:fs");
const net = require("node:net");
const os = require("node:os");
const path = require("node:path");
const { setTimeout: delay } = require("node:timers/promises");

const executable = process.argv[2];

async function freePort() {
  const socket = net.createServer();
  socket.listen(0, "127.0.0.1");
  await once(socket, "listening");
  const { port } = socket.address();
  await new Promise(resolve => socket.close(resolve));
  return port;
}

async function withServer(args, port, env, check) {
  const child = spawn(executable, args, { env, stdio: ["ignore", "pipe", "pipe"] });
  const exited = once(child, "exit");
  let output = "";
  child.stdout.on("data", chunk => { output += chunk; });
  child.stderr.on("data", chunk => { output += chunk; });
  const origin = `http://localhost:${port}`;
  try {
    let response;
    for (let attempt = 0; attempt < 150; attempt++) {
      assert.equal(child.exitCode, null, output);
      try {
        response = await fetch(origin, { signal: AbortSignal.timeout(1000) });
        break;
      } catch {
        await delay(100);
      }
    }
    assert.ok(response, `Server did not start: ${output}`);
    assert.equal(response.status, 200);
    const html = await response.text();
    // Check that the initial application scripts are served by the offline server.
    const scripts = [...html.matchAll(/<script[^>]+src="([^"]+)"/g)];
    assert.ok(scripts.length > 0, "No application scripts in page");
    for (const [, src] of scripts) {
      const url = new URL(src, origin);
      assert.equal(url.origin, origin, "Unexpected external script dependency");
      const asset = await fetch(url);
      assert.equal(asset.status, 200);
      assert.ok((await asset.text()).length > 0);
    }
    await check(origin);
  } finally {
    child.kill("SIGINT");
    const force = setTimeout(() => child.kill("SIGKILL"), 5000);
    const [code] = await exited;
    clearTimeout(force);
    assert.equal(code, 0, output);
  }
}

async function main() {
  for (const mode of ["default", "custom", "relative"]) {
    const root = mkdtempSync(path.join(os.tmpdir(), `keybr-server-${mode}-`));
    const home = path.join(root, "home");
    mkdirSync(home);
    const env = { ...process.env, HOME: home };
    for (const key of ["DISPLAY", "WAYLAND_DISPLAY", "XDG_CONFIG_HOME", "XDG_DATA_HOME", "XDG_CACHE_HOME", "XDG_STATE_HOME"]) {
      delete env[key];
    }
    if (mode === "custom") {
      env.XDG_CONFIG_HOME = path.join(root, "custom config");
      env.XDG_DATA_HOME = path.join(root, "custom data");
    } else if (mode === "relative") {
      env.XDG_CONFIG_HOME = "relative";
      env.XDG_DATA_HOME = "relative";
    }
    assert.equal(spawnSync(executable, ["--help"], { env }).status, 0);
    for (const args of [["--port", "0"], ["--port", "65536"], ["--port", "abc"], ["--port"], ["--unknown"], ["unexpected"]]) {
      assert.equal(spawnSync(executable, args, { env }).status, 1);
    }
    assert.deepEqual(readdirSync(home), [], "CLI validation must not create app data");
    const data = path.join(mode === "custom" ? env.XDG_DATA_HOME : path.join(home, ".local/share"), "keybr-standalone");
    const settings = { "keyboard.layout": "us" };
    // Exercise the default port as well as the explicit port option.
    const port = mode === "default" ? 8080 : await freePort();
    const args = mode === "default" ? [] : ["--port", String(port)];
    await withServer(args, port, env, async origin => {
      const response = await fetch(`${origin}/_/sync/settings`, {
        method: "PUT",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(settings),
      });
      assert.equal(response.status, 204);
      assert.ok(existsSync(path.join(data, "database.sqlite")));
      assert.deepEqual(JSON.parse(readFileSync(path.join(data, "data/user_settings/000/000/000000001"), "utf8")), settings);
    });
    await withServer(args, port, env, async origin => {
      const response = await fetch(`${origin}/_/sync/settings`);
      assert.deepEqual(await response.json(), settings, "Settings must survive a server restart");
    });
    assert.deepEqual(readdirSync(home).sort(), mode === "custom" ? [] : [".config", ".local"]);
  }
}

main().catch(error => {
  console.error(error);
  process.exitCode = 1;
});
