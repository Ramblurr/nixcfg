{
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
  };

  openrouter = [
    "google/gemini-4-carbon"
    "z-ai/glm-5.3-flash"
    "deepseek/deepseek-v4.1-flash-1017"
    "moonshotai/kimi-k3.1-pro"
    "tencent/hy4"
  ];
}
