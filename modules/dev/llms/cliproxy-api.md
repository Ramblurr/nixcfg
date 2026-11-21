# CLIProxyAPI

Quine runs the `cliproxy-api` user service on loopback port 8317, with HTTPS
through Tailscale Serve on the same port. Host settings live in
`hosts/quine/llm.nix`; the service module is `cliproxy-api.nix`.

At each service start, `scripts/render-op-config.clj` resolves exact `op://` strings in the
Nix-generated template and writes a mode-0600 config under
`$XDG_RUNTIME_DIR/cliproxyapi/config.yaml`. JSON is accepted as YAML by the proxy.
Secret values never enter the Nix template or store. Resolution failure stops
startup rather than starting with missing credentials. Run
`bb tests/render-op-config_test.clj` after changing the renderer. The old hand-edited
`~/.config/cliproxyapi/config.yaml` is no longer the service input after deployment.

The LLM vault is read through `op-read-llm` and the configured Connect token.
The management key uses `op://LLM/CLIProxyAPI/management-secret-unhashed`; the
proxy hashes it in its runtime config. Client keys are
`op://LLM/CLIProxyAPI/credential` and
`op://moot/cliproxyapi hirde/credential`.

Every reference resolves through Connect when the service starts, so after
rotating a key in 1Password, restart `cliproxy-api`. A reference that fails to
resolve is skipped with a warning; the service refuses to start only when no
client key resolves.

OpenRouter uses `api-keys.openai-compatibility`, with its upstream key read from
`op://LLM/OpenRouter/credential`. Model IDs must be listed explicitly in the host
settings. Quine exposes `google/gemini-4-carbon`, `z-ai/glm-5.3-flash`,
`deepseek/deepseek-v4.1-flash-1017`, `moonshotai/kimi-k3.1-pro`, `tencent/hy4`, and
`tencent/hy4-flash`.
Pi discovers them under `cliproxy`, using its OpenRouter catalog for
protocol, limits, and metered prices. ECA's `openrouter` provider uses the same six
models through `nono-eca`. Nono forwards its OpenRouter route to CLIProxyAPI with
the proxy client credential, so ECA never retrieves the upstream OpenRouter key.
The wrapper preserves unrelated `ECA_CONFIG` settings and configures the provider
to use nono's local URL and temporary token. Restart an existing ECA server after
changing this wrapper. Global or project ECA provider settings can override its
environment defaults.

Claude Code and Codex currently keep proxy client keys in their local settings.
Rotating only the server key breaks them: update clients in the same operation.
Test `/v1/models` and an actual model request after changes. Never print config
contents or headers; they contain secret values. A 429 `model_cooldown` indicates
an upstream usage limit.
