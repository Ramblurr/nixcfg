import asyncio
from contextlib import asynccontextmanager
import copy
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import json
import math
import os
from pathlib import Path
import sys
from tempfile import TemporaryDirectory
from threading import Thread
from urllib.parse import urlsplit


MISTRAL = "fixture-mistral"
SAVANT = "subq-savant-1.4"
RESPONSES = "gpt-6.2-sol"
ANTHROPIC = "claude-opus-5-5"
OPENROUTER = "google/gemini-4-carbon"

# Snap classifies directly against SubQ; the fixture fetch keeps it offline.
SNAP_EXTENSION = """
export default function (pi) {
  pi.registerCommand("fixture-snap", {
    async handler(_args, ctx) {
      const model = ctx.modelRegistry.getModelOfType("classifier", "subq", "subq-snap-1.2");
      if (!model || model.api !== "typesafe-system-one" || model.baseUrl !== "https://api.subq.ai/v1") {
        throw new Error("Unexpected Snap model: " + JSON.stringify(model));
      }
      const result = await ctx.modelRegistry.classify(model, {
        state: { text: "Fixture" },
        questions: {
          match: { type: "bool", instructions: "Is this a fixture?", criteria: { true: "Yes", false: "No" } },
        },
      }, {
        async fetch(url, init) {
          const payload = JSON.parse(init.body);
          if (String(url) !== "https://api.subq.ai/v1/systemone" ||
              payload.model !== model.id ||
              new Headers(init.headers).get("authorization") !== "Bearer fixture-token") {
            throw new Error("Unexpected direct classifier request");
          }
          return Response.json({ answers: { match: { type: "noul", noul: 0.95 } } });
        },
      });
      if (result.stopReason !== "stop" || result.answers.match.probability !== 0.95) {
        throw new Error("Unexpected classifier result: " + JSON.stringify(result));
      }
    },
  });
}
"""


class Proxy(BaseHTTPRequestHandler):
    def log_message(self, *_args):
        pass

    def send_body(self, status, content_type, body):
        body = body.encode()
        self.send_response(status)
        self.send_header("Content-Type", content_type)
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def do_GET(self):
        self.server.requests.append(("GET", urlsplit(self.path).path))
        self.send_body(400, "text/plain", "unexpected discovery request")

    def do_POST(self):
        payload = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
        model_id = payload["model"]
        path = urlsplit(self.path).path
        self.server.requests.append((model_id, path))
        if self.headers.get("Authorization") != "Bearer fixture-token" and self.headers.get("x-api-key") != "fixture-token":
            self.send_body(401, "text/plain", "missing fixture credential")
            return
        if path == "/v1/responses":
            events = [{"type": "response.completed", "response": {"status": "completed"}}]
            body = "".join(f"data: {json.dumps(event)}\n\n" for event in events)
        elif path == "/v1/messages":
            events = [
                {"type": "message_start", "message": {"id": "fixture", "usage": {"input_tokens": 1, "output_tokens": 0}}},
                {"type": "message_delta", "delta": {"stop_reason": "end_turn"}, "usage": {"output_tokens": 1}},
                {"type": "message_stop"},
            ]
            body = "".join(f"event: {event['type']}\ndata: {json.dumps(event)}\n\n" for event in events)
        elif path in ("/v1/chat/completions", "/configured/v1/chat/completions"):
            event = {
                "id": "fixture",
                "model": model_id,
                "choices": [{"index": 0, "delta": {"content": "fixture-ok"}, "finish_reason": "stop"}],
                "usage": {
                    "prompt_tokens": 1000,
                    "completion_tokens": 100,
                    "prompt_tokens_details": {"cached_tokens": 800},
                },
            }
            body = f"data: {json.dumps(event)}\n\ndata: [DONE]\n\n"
        else:
            self.send_body(400, "text/plain", f"unexpected stream path {path}")
            return
        self.send_body(200, "text/event-stream", body)


class PiRpc:
    def __init__(self, process):
        self.process = process

    async def receive(self, match):
        async with asyncio.timeout(12):
            while line := await self.process.stdout.readline():
                message = json.loads(line)
                assert message["type"] != "extension_error", message
                if match(message):
                    return message
        raise RuntimeError((await self.process.stderr.read()).decode())

    async def command(self, command, **fields):
        self.process.stdin.write(json.dumps({"type": command, "id": command, **fields}).encode() + b"\n")
        await self.process.stdin.drain()
        response = await self.receive(lambda msg: msg.get("id") == command and msg["type"] == "response")
        assert response["success"], response
        return response.get("data")

    async def select(self, model_id):
        return await self.command("set_model", provider="cliproxy", modelId=model_id)

    async def available(self):
        response = await self.command("get_available_models")
        return {model["id"]: model for model in response["models"] if model["provider"] == "cliproxy"}

    async def classify(self):
        disposition = await self.command("prompt", message="/fixture-snap")
        assert disposition["disposition"] == "handled", disposition

    async def prompt(self, server, model_id, path):
        await self.select(model_id)
        await self.command("prompt", message="fixture")
        end = await self.receive(lambda msg: msg["type"] == "agent_end")
        message = end["messages"][-1]
        assert message["stopReason"] == "stop", message
        assert server.requests[-1] == (model_id, path), server.requests
        return message


@asynccontextmanager
async def pi_rpc(pi, directory, *, extensions=True):
    args = [pi, "--mode", "rpc", "--no-session", "--no-skills"]
    if extensions:
        args += ["-e", str(directory / "snap.ts")]
    else:
        args.append("--no-extensions")
    process = await asyncio.create_subprocess_exec(
        *args,
        env={
            "PATH": os.environ["PATH"],
            "HOME": str(directory),
            "PI_CODING_AGENT_DIR": str(directory),
            "PI_SKIP_VERSION_CHECK": "1",
            "PI_TELEMETRY": "0",
            "PI_OFFLINE": "1",
        },
        cwd=directory,
        stdin=asyncio.subprocess.PIPE,
        stdout=asyncio.subprocess.PIPE,
        stderr=asyncio.subprocess.PIPE,
        limit=2**22,
    )
    try:
        yield PiRpc(process)
    finally:
        if process.returncode is None:
            process.terminate()
            await asyncio.wait_for(process.wait(), 5)


def setup(directory, profile, proxy_url, stale_url):
    directory.mkdir()
    # A discovery cache from the removed extension, naming another proxy.
    stale = [
        {"id": RESPONSES, "api": "openai-responses", "provider": "cliproxy", "baseUrl": f"{stale_url}/v1"},
        {"id": "stale-only", "api": "openai-responses", "provider": "cliproxy", "baseUrl": f"{stale_url}/v1"},
    ]
    (directory / "models-store.json").write_text(json.dumps({"cliproxy": {"models": stale, "checkedAt": 1}}))
    config = copy.deepcopy(profile["models"])
    config["providers"]["subq"]["apiKey"] = "!printf fixture-token"
    provider = config["providers"]["cliproxy"]
    provider["baseUrl"] = proxy_url
    for model in provider["models"]:
        model["baseUrl"] = model["baseUrl"].replace("http://proxy.invalid", proxy_url)
    (directory / "models.json").write_text(json.dumps(config))
    (directory / "settings.json").write_text(json.dumps(profile["settings"]))
    (directory / "snap.ts").write_text(SNAP_EXTENSION)
    return {model["id"]: model for model in provider["models"]}


async def check_profiles(pi, profiles, directory, server, stale):
    proxy_url = f"http://127.0.0.1:{server.server_port}"
    stale_url = f"http://127.0.0.1:{stale.server_port}"
    for name, profile in profiles.items():
        assert len(profile["settings"]["packages"]) == 1, name
        agent_dir = directory / name
        configured = setup(agent_dir, profile, proxy_url, stale_url)
        assert {MISTRAL, SAVANT, RESPONSES, ANTHROPIC, OPENROUTER} <= configured.keys(), name

        async with pi_rpc(pi, agent_dir) as rpc:
            assert (await rpc.select(MISTRAL))["api"] == "mistral-conversations"
            models = await rpc.available()
            assert models.keys() == configured.keys(), models.keys() ^ configured.keys()
            for model_id, model in configured.items():
                assert models[model_id]["baseUrl"] == model["baseUrl"], model_id
            await rpc.prompt(server, RESPONSES, "/v1/responses")
            await rpc.prompt(server, ANTHROPIC, "/v1/messages")
            await rpc.prompt(server, MISTRAL, "/v1/chat/completions")
            message = await rpc.prompt(server, OPENROUTER, "/v1/chat/completions")
            cost = message["usage"]["cost"]
            assert math.isclose(cost["input"], 200 * 2e-6), message
            assert math.isclose(cost["cacheRead"], 800 * 0.1e-6), message
            assert math.isclose(cost["output"], 100 * 10e-6), message
            message = await rpc.prompt(server, SAVANT, "/configured/v1/chat/completions")
            assert message["usage"]["cacheRead"] == 800, message
            assert math.isclose(message["usage"]["cost"]["cacheRead"], 800 * 0.01e-6), message
            await rpc.classify()
        print(f"{name}: first RPC, request paths, pricing, and Snap OK", flush=True)

        async with pi_rpc(pi, agent_dir, extensions=False) as rpc:
            assert (await rpc.select(MISTRAL))["api"] == "mistral-conversations"
            assert (await rpc.available()).keys() == configured.keys()
            await rpc.prompt(server, OPENROUTER, "/v1/chat/completions")
        print(f"{name} --no-extensions: first RPC and stream OK", flush=True)

    assert not any(method == "GET" for method, _path in server.requests), server.requests
    assert not stale.requests, stale.requests


if __name__ == "__main__":
    pi, profiles_file = sys.argv[1:]
    profiles = json.loads(Path(profiles_file).read_text())
    assert {"pi", "pi-t3"} <= profiles.keys()
    with (
        TemporaryDirectory() as directory,
        ThreadingHTTPServer(("127.0.0.1", 0), Proxy) as server,
        ThreadingHTTPServer(("127.0.0.1", 0), Proxy) as stale,
    ):
        threads = []
        for proxy in (server, stale):
            proxy.requests = []
            threads.append(Thread(target=proxy.serve_forever, daemon=True))
            threads[-1].start()
        try:
            asyncio.run(check_profiles(pi, profiles, Path(directory), server, stale))
        finally:
            for proxy, thread in zip((server, stale), threads):
                proxy.shutdown()
                thread.join()
