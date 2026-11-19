# Pi provider for CLIProxyAPI with a static model list, so the models exist the
# moment Pi starts, before any extension can discover them.
{ url, apiKey }:
let
  cliproxyRoot = url;
  cliproxyOpenaiModel = id: name: offLevel: {
    inherit id name;
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
      off = offLevel;
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
  cliproxyMuseModel = id: name: {
    inherit id name;
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
  models = [
    (cliproxyOpenaiModel "gpt-6.2-sol" "GPT-6.2 Sol" null)
    (cliproxyOpenaiModel "gpt-6.1-terra" "GPT-6.1 Terra" "none")
    (cliproxyOpenaiModel "gpt-6.2-astra" "GPT-6.2 Astra" null)
    (cliproxyOpenaiModel "gpt-6-luna" "GPT-6 Luna" "none")
    (cliproxyMuseModel "muse-wonder-1.0" "Muse Wonder")
    (cliproxyMuseModel "muse-wonder-1.0-contributor" "Muse Wonder (Contributor)")
    (cliproxyMuseModel "muse-wonder-full-1.0" "Muse Wonder Full")
    (cliproxyMuseModel "muse-wonder-full-1.0-contributor" "Muse Wonder Full (Contributor)")
  ];
}
