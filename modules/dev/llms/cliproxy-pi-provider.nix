# Pi provider for CLIProxyAPI with the full static model list. Pi does not
# discover proxy models at runtime, so every model is here.
{
  url,
  apiKey ? "tailnet",
  extraModels ? [ ],
}:
let
  catalog = (import ./models.nix).cliproxy;
  cliproxyRoot = url;
  cliproxyOpenaiModel = model: {
    inherit (model) id name;
    api = "openai-responses";
    reasoning = true;
    input = [
      "text"
      "image"
    ];
    contextWindow = 272000;
    maxTokens = 128000;
    cost = {
      input = 0;
      output = 0;
      cacheRead = 0;
      cacheWrite = 0;
    };
    thinkingLevelMap = {
      off = model.offThinkingLevel;
      minimal = null;
      low = "low";
      medium = "medium";
      high = "high";
      xhigh = "xhigh";
      max = "max";
    };
    inputLimits = {
      maxRequestBytes = 536870912;
      images = {
        maxPerRequest = 1500;
        resize = {
          maxWidth = 2000;
          maxHeight = 2000;
          maxBytes = 4718592;
          jpegQuality = 80;
        };
      };
    };
    compat = {
      supportsStrictMode = true;
      supportsOpenAIGrammarTools = true;
      supportsAdditionalTools = true;
      supportsToolSearch = true;
      supportsMidConvoSystemMessages = true;
      supportsExplicitPromptCacheMode = true;
      supportsMidConvoEffort = true;
    };
  };
  cliproxyMuseModel = model: {
    inherit (model) id name;
    api = "openai-responses";
    reasoning = true;
    input = [
      "text"
      "image"
    ];
    contextWindow = 1048576;
    maxTokens = 131072;
    cost = {
      input = 0;
      output = 0;
      cacheRead = 0;
      cacheWrite = 0;
    };
    thinkingLevelMap = {
      off = null;
      minimal = "minimal";
      low = "low";
      medium = "medium";
      high = "high";
      xhigh = "xhigh";
      max = "max";
    };
    inputLimits.images.resize = {
      maxWidth = 2000;
      maxHeight = 2000;
      maxBytes = 4718592;
      jpegQuality = 80;
    };
  };
in
{
  baseUrl = cliproxyRoot;
  inherit apiKey;
  models =
    let
      extraIds = map (model: model.id) extraModels;
      routed = builtins.filter (model: !(builtins.elem model.id extraIds)) catalog.routed;
      declared =
        map cliproxyOpenaiModel catalog.openai
        ++ map cliproxyMuseModel catalog.muse
        ++ routed
        ++ extraModels;
      declaredIds = map (model: model.id) declared;
      advertised = builtins.filter (model: !(builtins.elem model.id declaredIds)) catalog.advertised;
      # These clients append /v1 themselves; the others take it in the base URL.
      withBaseUrl =
        model:
        {
          baseUrl =
            if
              builtins.elem model.api [
                "anthropic-messages"
                "mistral-conversations"
              ]
            then
              cliproxyRoot
            else
              "${cliproxyRoot}/v1";
        }
        // model;
    in
    map withBaseUrl (declared ++ advertised);
}
