"""Check every MCP tool on both endpoints against a real indexed Git fixture."""
import json
import sys
import urllib.error
import urllib.request

base, repository, revision = sys.argv[1:]
CORE = {"grep", "read_file", "list_files", "list_repos", "commit_search", "add_repos", "sync_repos"}
ALL = CORE | {"glob", "list_refs", "compare_revisions", "find_symbol_definitions", "find_symbol_references"}


def request(endpoint, headers, message=None, method="POST", extra=None):
    req = urllib.request.Request(
        base + endpoint,
        data=json.dumps(message).encode() if message is not None else None,
        method=method,
        headers=headers | (extra or {}),
    )
    with urllib.request.urlopen(req, timeout=15) as response:
        events = [line[6:] for line in response.read().decode().splitlines() if line.startswith("data: ")]
        return response.status, response.headers, json.loads(events[0]) if events else None


def open_session(endpoint):
    headers = {"Content-Type": "application/json", "Accept": "application/json, text/event-stream"}
    _, initial_headers, initialized = request(endpoint, headers, {
        "jsonrpc": "2.0", "id": 1, "method": "initialize",
        "params": {"protocolVersion": "2025-11-25", "capabilities": {},
                   "clientInfo": {"name": "nixos-integration", "version": "1"}},
    })
    assert initialized["result"]["capabilities"] == {"tools": {}}, initialized
    assert "list_repos" in initialized["result"]["instructions"], initialized
    headers |= {"Mcp-Session-Id": initial_headers["Mcp-Session-Id"], "Mcp-Protocol-Version": "2025-11-25"}
    assert request(endpoint, headers, {"jsonrpc": "2.0", "method": "notifications/initialized"})[0] == 202
    return headers


def tool_names(endpoint, headers):
    listed = request(endpoint, headers, {"jsonrpc": "2.0", "id": 2, "method": "tools/list"})[2]
    return {entry["name"] for entry in listed["result"]["tools"]}


def call(endpoint, headers, name, arguments):
    response = request(endpoint, headers, {"jsonrpc": "2.0", "id": name, "method": "tools/call",
                                           "params": {"name": name, "arguments": arguments}})[2]
    assert "error" not in response, response
    assert not response["result"].get("isError", False), response
    return response["result"]["content"][0]["text"]


core = open_session("/mcp")
full = open_session("/mcp/all")
assert tool_names("/mcp", core) == CORE
assert tool_names("/mcp/all", full) == ALL


def tool(name, arguments):
    return json.loads(call("/mcp/all", full, name, arguments | {"format": "json"}))


# EDN is the default format.
edn = call("/mcp", core, "list_repos", {"query": "DEMO"})
assert edn.startswith("{") and f':id "{repository}"' in edn, edn
repos = tool("list_repos", {"query": "demo"})
assert [row["id"] for row in repos["results"]] == [repository] and "warnings" not in repos["results"][0], repos
search = tool("grep", {"query": "sym:DemoSymbol"})
hit = search["results"][0]
assert hit["path"] == "main.go" and f"@{revision}:" in hit["cite"], search
read = tool("read_file", {"cite": hit["cite"]})
assert read["rev"] == revision and "func DemoSymbol() {}" in read["lines"], read
common = {"repository": repository}
assert tool("glob", common | {"pattern": "**/*.go"})["results"] == ["main.go"]
assert "main.go" in tool("list_files", common)["entries"]
refs = tool("list_refs", common)["results"]
assert {(ref["name"], ref["type"], ref["rev"]) for ref in refs} == {("main", "branch", revision), ("v1.0.0", "tag", revision)}, refs
assert [ref["name"] for ref in tool("list_refs", common | {"tagsOnly": True})["results"]] == ["v1.0.0"]
commits = tool("commit_search", {"repos": [repository], "messageTerms": ["INITIAL"]})
assert [commit["rev"] for commit in commits["results"]] == [revision], commits
assert tool("compare_revisions", {"repo": repository, "base": "v1.0.0", "head": "HEAD"})["filesChanged"] == 0
for name, kind in [("find_symbol_definitions", "ctags-definitions"),
                   ("find_symbol_references", "textual-occurrences-not-semantic")]:
    result = tool(name, common | {"symbol": "DemoSymbol"})
    assert result["kind"] == kind and f"@{revision}:" in result["results"][0]["cite"], result

# Inventory changes go through the real editor's agent API.
added = tool("add_repos", {"repos": [{"url": "https://github.com/Example/Added.git"}]})
assert added["added"] == ["https://github.com/example/added"], added
with urllib.request.urlopen(base + "/edit/api/repos", timeout=15) as response:
    assert "https://github.com/example/added" in json.load(response)
assert tool("add_repos", {"repos": [{"url": "https://github.com/example/added"}]})["existing"] == ["https://github.com/example/added"]
assert tool("sync_repos", {"url": "https://github.com/example/added"})["status"] == "accepted"

# Sessions are bound to their endpoint; untrusted origins are rejected.
for endpoint, headers in [("/mcp/all", core), ("/mcp", full)]:
    try:
        request(endpoint, headers, {"jsonrpc": "2.0", "id": 3, "method": "tools/list"})
        raise AssertionError("A session was accepted on another endpoint")
    except urllib.error.HTTPError as error:
        assert error.code == 404, error.code
try:
    request("/mcp", core, {"jsonrpc": "2.0", "id": 3, "method": "ping"}, extra={"Origin": "https://untrusted.example"})
    raise AssertionError("Untrusted Origin was accepted")
except urllib.error.HTTPError as error:
    assert error.code == 403, error.code
assert request("/mcp", core, method="DELETE")[0] == 204
assert request("/mcp/all", full, method="DELETE")[0] == 204
print("Every tool passed on both endpoints through Caddy against real Git, Neogrok, Zoekt and the editor.")
