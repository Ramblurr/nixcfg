# Pi provider for CLIProxyAPI with a static model list, so the models exist the
# moment Pi starts, before any extension can discover them.
{ url, apiKey }:
let
  catalog = (import ./models.nix).cliproxy;
  cliproxyRoot = url;
  cliproxyOpenaiModel = model: {
    inherit (model) id name;
    api = "openai-responses";
    baseUrl = "${cliproxyRoot}/v1";
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
    baseUrl = "${cliproxyRoot}/v1";
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
  models = map cliproxyOpenaiModel catalog.openai ++ map cliproxyMuseModel catalog.muse;
}
