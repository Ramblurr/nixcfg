"""Check the ten MCP tools against a real indexed Git fixture."""
import json
import sys
import urllib.error
import urllib.request

base, repository, revision = sys.argv[1:]
headers = {"Content-Type": "application/json", "Accept": "application/json, text/event-stream"}


def request(message=None, method="POST", extra=None):
    req = urllib.request.Request(
        base + "/mcp",
        data=json.dumps(message).encode() if message is not None else None,
        method=method,
        headers=headers | (extra or {}),
    )
    with urllib.request.urlopen(req, timeout=15) as response:
        events = [line[6:] for line in response.read().decode().splitlines() if line.startswith("data: ")]
        return response.status, response.headers, json.loads(events[0]) if events else None


_, initial_headers, initialized = request({
    "jsonrpc": "2.0", "id": 1, "method": "initialize",
    "params": {"protocolVersion": "2025-11-25", "capabilities": {},
               "clientInfo": {"name": "nixos-integration", "version": "1"}},
})
assert initialized["result"]["capabilities"] == {"tools": {}}, initialized
headers |= {"Mcp-Session-Id": initial_headers["Mcp-Session-Id"], "Mcp-Protocol-Version": "2025-11-25"}
assert request({"jsonrpc": "2.0", "method": "notifications/initialized"})[0] == 202
listed = request({"jsonrpc": "2.0", "id": 2, "method": "tools/list"})[2]
assert len(listed["result"]["tools"]) == 10, listed


def tool(name, arguments):
    response = request({"jsonrpc": "2.0", "id": name, "method": "tools/call",
                        "params": {"name": name, "arguments": arguments}})[2]
    assert "error" not in response, response
    assert not response["result"].get("isError", False), response
    return json.loads(response["result"]["content"][0]["text"])


repos = tool("list_repos", {})
assert any(repo["id"] == repository and repo["head"] == revision for repo in repos["repositories"]), repos
search = tool("grep", {"query": "sym:DemoSymbol"})
assert search["files"] and search["files"][0]["revision"] == revision, search
common = {"repository": repository}
read = tool("read_file", common | {"revision": search["files"][0]["revision"], "path": "main.go"})
assert read["revision"] == revision and "func DemoSymbol() {}" in read["lines"], read
assert tool("glob", common | {"pattern": "**/*.go"})["matches"] == ["main.go"]
assert any(entry["path"] == "main.go" for entry in tool("list_tree", common)["entries"])
assert any(branch["revision"] == revision for branch in tool("list_branches", common)["branches"])
assert tool("list_commits", common)["commits"][0]["revision"] == revision
assert tool("get_diff", common | {"from": "HEAD", "to": "HEAD"})["patch"] == ""
for name, kind in [("find_symbol_definitions", "ctags-definitions"),
                   ("find_symbol_references", "textual-occurrences-not-semantic")]:
    result = tool(name, common | {"symbol": "DemoSymbol"})
    assert result["kind"] == kind and result["results"]["files"], result
    assert result["results"]["files"][0]["revision"] == revision, result
try:
    request({"jsonrpc": "2.0", "id": 3, "method": "ping"}, extra={"Origin": "https://untrusted.example"})
    raise AssertionError("Untrusted Origin was accepted")
except urllib.error.HTTPError as error:
    assert error.code == 403, error.code
assert request(method="DELETE")[0] == 204
print("All ten tools passed through Caddy against real Git, Neogrok and Zoekt.")
