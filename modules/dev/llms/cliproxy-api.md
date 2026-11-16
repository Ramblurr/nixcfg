# CLIProxyAPI

Quine runs the `cliproxy-api` user service on loopback port 8317, with HTTPS
through Tailscale Serve on the same port. Host settings live in
`hosts/quine/llm.nix`; the service module is `cliproxy-api.nix`.

At each service start, `cliproxy-config.py` resolves exact `op://` strings in the
Nix-generated template and writes a mode-0600 config under
`$XDG_RUNTIME_DIR/cliproxyapi/config.yaml`. JSON is accepted as YAML by the proxy.
Secret values never enter the Nix template or store. Resolution failure stops
startup rather than starting with missing credentials. The old hand-edited
`~/.config/cliproxyapi/config.yaml` is no longer the service input after deployment.

The LLM vault is read through `op-read-llm` and the configured Connect token.
The management key uses `op://LLM/CLIProxyAPI/management-secret-unhashed`; the
proxy hashes it in its runtime config. Client keys are
`op://LLM/CLIProxyAPI/credential` and
`op://moot/cliproxyapi hirde/credential`.

Connect cannot read the moot vault. The hirde reference therefore uses the
owner-only runtime source `/home/ramblurr/.local/state/cliproxyapi/hirde-key` via
`secretFiles`. When rotating this key, update 1Password, this file, and its
clients before restarting the service. Other references resolve directly through
Connect. This file must be restored when recovering the host.

OpenRouter uses `api-keys.openai-compatibility`, with its upstream key read from
`op://LLM/OpenRouter/credential`. Model IDs must be listed explicitly in the host
settings. Quine exposes `google/gemini-4-carbon`, `z-ai/glm-5.3-flash`,
`deepseek/deepseek-v4.1-flash-1017`, `moonshotai/kimi-k3.1-pro`, and `tencent/hy4`.
Pi discovers them under `cliproxy`, using its OpenRouter catalog for
protocol, limits, and metered prices. ECA's separate nono OpenRouter route remains
configured independently.

Claude Code and Codex currently keep proxy client keys in their local settings.
Rotating only the server key breaks them: update clients in the same operation.
Test `/v1/models` and an actual model request after changes. Never print config
contents or headers; they contain secret values. A 429 `model_cooldown` indicates
an upstream usage limit.
