rec {
  defaults = {
    pi = "gpt-6.2-sol";
    codex = "gpt-6-luna";
    claude = "opus";
  };

  cliproxy = {
    openai = [
      {
        id = "gpt-6.2-sol";
        name = "GPT-6.2 Sol";
        offThinkingLevel = null;
      }
      {
        id = "gpt-6.1-terra";
        name = "GPT-6.1 Terra";
        offThinkingLevel = "none";
      }
      {
        id = "gpt-6.2-astra";
        name = "GPT-6.2 Astra";
        offThinkingLevel = null;
      }
      {
        id = "gpt-6-luna";
        name = "GPT-6 Luna";
        offThinkingLevel = "none";
      }
    ];
    muse = [
      {
        id = "muse-wonder-1.0";
        name = "Muse Wonder";
      }
      {
        id = "muse-wonder-1.0-contributor";
        name = "Muse Wonder (Contributor)";
      }
      {
        id = "muse-wonder-full-1.0";
        name = "Muse Wonder Full";
      }
      {
        id = "muse-wonder-full-1.0-contributor";
        name = "Muse Wonder Full (Contributor)";
      }
    ];
    # Every other model the proxy advertised on 2026-11-20, as Pi's discovery
    # recorded them. OpenRouter prices are that day's OpenRouter list prices.
    advertised = builtins.fromJSON (builtins.readFile ./cliproxy-advertised.json);
    routed = [
      subq.savant
      mistral.large4
    ];
  };

  subq.savant = {
    id = "subq-savant-1.4";
    name = "SubQ Savant 1.4";
    api = "openai-completions";
    contextWindow = 4000000;
    maxTokens = 32768;
    reasoning = true;
    input = [
      "text"
      "image"
    ];
    cost = {
      input = 0.1;
      output = 0.5;
      cacheRead = 0.01;
      cacheWrite = 0;
    };
    compat = {
      maxTokensField = "max_tokens";
      supportsDeveloperRole = false;
      supportsReasoningEffort = true;
      thinkingFormat = "openai";
    };
    thinkingLevelMap = {
      off = null;
      minimal = null;
      low = "low";
      medium = null;
      high = "high";
      xhigh = null;
      max = "max";
    };
  };

  mistral.large4 = {
    id = "mistral-large-4";
    name = "Mistral Large 4";
    api = "mistral-conversations";
    reasoning = true;
    input = [
      "text"
      "image"
    ];
    contextWindow = 1048576;
    maxTokens = 262144;
    cost = {
      input = 0.7;
      output = 2.1;
      cacheRead = 0.07;
      cacheWrite = 0;
    };
    thinkingLevelMap = {
      off = null;
      minimal = null;
      low = "low";
      medium = "medium";
      high = "high";
      xhigh = null;
      max = null;
    };
    inputLimits.images.resize = {
      maxWidth = 2000;
      maxHeight = 2000;
      maxBytes = 4718592;
      jpegQuality = 80;
    };
  };

  openrouter = [
    "google/gemini-4-carbon"
    "z-ai/glm-5.3-flash"
    "deepseek/deepseek-v4.1-flash-1017"
    "moonshotai/kimi-k3.1-pro"
    "tencent/hy4"
  ];
}
