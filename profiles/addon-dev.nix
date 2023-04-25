{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ../mixins/docker.nix
    ../mixins/podman.nix
  ];
  config = {
    home-manager.users.ramblurr = {pkgs, ...}: {
      home.packages = with pkgs; [
        go
        terraform
        httpie
        openjdk11
        openjdk17
        openjdk19
        jetbrains.idea-ultimate
        jetbrains.datagrip
        jetbrains.gateway
        tcpdump
        docker-compose
        dig
        dogdns
        whois
        nmap
        yq
        k9s
        yamlfmt
        kustomize
        cilium-cli
        fluxcd
        esphome
      ];

      home.file.".ideavimrc" = {
        source = ../configs/ideavimrc;
      };
      home.file.".config/ideavim" = {
        source = ../configs/ideavim;
        recursive = true;
      };
      home.persistence."/persist/home/ramblurr" = {
        directories = [
          ".config/JetBrains"
          ".cache/JetBrains"
          ".local/share/JetBrains"
          ".java/.userPrefs/jetbrains"
          ".vscode/extensions"
          ".config/Code"
        ];
      };
      programs.vscode = {
        enable = true;
        enableExtensionUpdateCheck = true;
        keybindings = [
          {
            key = "ctrl+e";
            command = "-extension.vim_ctrl+e";
            when = "editorTextFocus && vim.active && vim.use<C-e> && !inDebugRepl";
          }
          {
            key = "ctrl+e";
            command = "-workbench.action.quickOpen";
          }
          {
            key = "ctrl+e";
            command = "-workbench.action.quickOpenNavigateNextInFilePicker";
            when = "inFilesPicker && inQuickOpen";
          }
          {
            key = "ctrl+e";
            command = "workbench.action.quickOpenPreviousRecentlyUsedEditorInGroup";
            when = "!activeEditorGroupEmpty";
          }
          {
            key = "ctrl+tab";
            command = "-workbench.action.quickOpenPreviousRecentlyUsedEditorInGroup";
            when = "!activeEditorGroupEmpty";
          }
          {
            key = "ctrl+e";
            command = "workbench.action.quickOpenNavigateNext";
            when = "inQuickOpen";
          }
          {
            key = "ctrl+p";
            command = "workbench.action.quickOpenNavigatePrevious";
            when = "inQuickOpen";
          }
          {
            key = "ctrl+p";
            command = "-workbench.action.quickOpen";
          }
          {
            key = "ctrl+n";
            command = "workbench.action.showAllSymbols";
          }
          {
            key = "ctrl+t";
            command = "-workbench.action.showAllSymbols";
          }
          {
            key = "ctrl+n";
            command = "-workbench.action.files.newUntitledFile";
          }
          {
            key = "ctrl+n";
            command = "-extension.vim_ctrl+n";
            when = "editorTextFocus && vim.active && vim.use<C-n> && !inDebugRepl || vim.active && vim.use<C-n> && !inDebugRepl && vim.mode == 'CommandlineInProgress' || vim.active && vim.use<C-n> && !inDebugRepl && vim.mode == 'SearchInProgressMode'";
          }
          {
            key = "alt+x";
            command = "workbench.action.showCommands";
          }
          {
            key = "ctrl+shift+p";
            command = "-workbench.action.showCommands";
          }
          {
            key = "ctrl+alt+l";
            command = "editor.action.formatDocument";
            when = "editorHasDocumentFormattingProvider && editorTextFocus && !editorReadonly && !inCompositeEditor";
          }
          {
            key = "ctrl+shift+i";
            command = "-editor.action.formatDocument";
            when = "editorHasDocumentFormattingProvider && editorTextFocus && !editorReadonly && !inCompositeEditor";
          }
          {
            key = "ctrl+alt+left";
            command = "workbench.action.navigateBack";
          }
          {
            key = "ctrl+alt+-";
            command = "-workbench.action.navigateBack";
          }
          {
            key = "ctrl+alt+left";
            command = "-workbench.action.moveEditorToPreviousGroup";
          }
          {
            key = "ctrl+alt+right";
            command = "-workbench.action.moveEditorToNextGroup";
          }
          {
            key = "ctrl+alt+right";
            command = "workbench.action.navigateForward";
          }
          {
            key = "ctrl+shift+-";
            command = "-workbench.action.navigateForward";
          }
          {
            key = "shift+f6";
            command = "editor.action.rename";
            when = "editorHasRenameProvider && editorTextFocus && !editorReadonly";
          }
          {
            key = "f2";
            command = "-editor.action.rename";
            when = "editorHasRenameProvider && editorTextFocus && !editorReadonly";
          }
          {
            key = "shift+f6";
            command = "-workbench.action.focusPreviousPart";
          }
          {
            key = "f2";
            command = "editor.action.marker.nextInFiles";
            when = "editorFocus";
          }
          {
            key = "f8";
            command = "-editor.action.marker.nextInFiles";
            when = "editorFocus";
          }
          {
            key = "ctrl+p";
            command = "editor.action.triggerParameterHints";
            when = "editorHasSignatureHelpProvider && editorTextFocus";
          }
          {
            key = "ctrl+shift+space";
            command = "-editor.action.triggerParameterHints";
            when = "editorHasSignatureHelpProvider && editorTextFocus";
          }
          {
            key = "ctrl+k";
            command = "terminal.focus";
          }
          {
            key = "ctrl+k";
            command = "-extension.vim_ctrl+k";
            when = "editorTextFocus && vim.active && vim.use<C-k> && !inDebugRepl";
          }
          {
            key = "ctrl+k";
            command = "workbench.action.focusActiveEditorGroup";
            when = "!editorTextFocus";
          }
          {
            key = "escape";
            command = "-calva.clearInlineResults";
          }
          {
            key = "shift+escape";
            command = "calva.clearInlineResults";
            when = "editorTextFocus && !editorHasMultipleSelections && !editorReadOnly && !hasOtherSuggestions && !suggestWidgetVisible && editorLangId == 'clojure'";
          }
          {
            key = "shift+,";
            command = "paredit.barfSexpForward";
            when = "vim.active && vim.mode == 'Normal' && calva:keybindingsEnabled && editorTextFocus && editorLangId == 'clojure' && paredit:keyMap =~ /original|strict/";
          }
          {
            key = "shift+.";
            command = "paredit.slurpSexpForward";
            when = "vim.active && vim.mode == 'Normal' && calva:keybindingsEnabled && editorTextFocus && editorLangId == 'clojure' && paredit:keyMap =~ /original|strict/";
          }
        ];

        extensions = with pkgs.vscode-extensions; [
          redhat.ansible
          Boto3typed.boto3-ide
          bungcip.better-toml
          blinkshellinc.blink-fs
          ms-vscode-remote.remote-containers
          GitHub.copilot
          gitpod.gitpod-desktop
          golang.go
          casualjim.gotemplate
          andrejunges.Handlebars
          hashicorp.hcl
          hashicorp.terraform
          VisualStudioExptTeam.vscodeintellicode
          ms-python.isort
          wholroyd.jinja
          darkriszty.markdown-table-prettify
          matangover.mypy
          esbenp.prettier-vscode
          ms-python.vscode-pylance
          ms-python.python
          ms-vscode-remote.remote-ssh
          ms-vscode-remote.remote-ssh-edit
          ms-vscode.remote-explorer
          coolbear.systemd-unit-file
          bradlc.vscode-tailwindcss
          vscodevim.vim
          redhat.vscode-yaml
          jdinhlife.gruvbox
        ];
        userSettings = {
          "html.format.enable" = true;
          "html.format.indentHandlebars" = true;
          "html.format.maxPreserveNewLines" = 0;
          "python.venvFolders" = [
            "~/.pyenv/versions/"
          ];
          "vim.handleKeys" = {
            "<C-n>" = false;
            "<C-p>" = false;
            "<C-a>" = false;
            "<C-e>" = false;
          };
          "vim.useSystemClipboard" = true;
          "vim.easymotion" = false;
          "vim.leader" = "<space>";
          "vim.normalModeKeyBindingsNonRecursive" = [
            {
              "before" = [
                "g"
                "r"
              ];
              "commands" = [
                {
                  "command" = "editor.action.referenceSearch.trigger";
                  "when" = "editorHasReferenceProvider && editorTextFocus && !inReferenceSearchEditor && !isInEmbeddedEditor";
                }
              ];
            }
            {
              "before" = [
                "<leader>"
                "f"
                "f"
              ];
              "after" = [];
              "commands" = [
                {
                  "command" = "workbench.action.files.openFile";
                  "args" = [];
                }
              ];
            }
            {
              "before" = [
                "<leader>"
                "f"
                "D"
              ];
              "after" = [];
              "commands" = [
                {
                  "command" = "fileutils.removeFile";
                  "args" = [];
                }
              ];
            }
            {
              "before" = [
                "<leader>"
                "p"
                "f"
              ];
              "after" = [];
              "commands" = [
                {
                  "command" = "workbench.action.quickOpen";
                  "args" = [];
                }
              ];
            }
            {
              "before" = [
                "<leader>"
                "p"
                "p"
              ];
              "after" = [];
              "commands" = [
                {
                  "command" = "workbench.action.openRecent";
                  "args" = [];
                }
              ];
            }
            {
              "before" = [
                "<leader>"
                "f"
                "R"
              ];
              "after" = [];
              "commands" = [
                {
                  "command" = "fileutils.move";
                  "args" = [];
                }
              ];
            }
            {
              "before" = [
                "<leader>"
                "v"
              ];
              "after" = [];
              "commands" = [
                {
                  "command" = "editor.action.smartSelect.expand";
                  "when" = "editorTextFocus";
                  "args" = [];
                }
              ];
            }
            {
              "before" = [
                "<leader>"
                "V"
              ];
              "after" = [];
              "commands" = [
                {
                  "command" = "editor.action.smartSelect.shrink";
                  "args" = [];
                }
              ];
            }
            {
              "before" = [
                "<leader>"
                "b"
                "d"
              ];
              "after" = [];
              "commands" = [
                {
                  "command" = "workbench.action.closeActiveEditor";
                  "args" = [];
                }
              ];
            }
            {
              "before" = [
                "<leader>"
                "b"
                "n"
              ];
              "after" = [];
              "commands" = [
                {
                  "command" = "workbench.action.nextEditor";
                  "args" = [];
                }
              ];
            }
            {
              "before" = [
                "<leader>"
                "b"
                "p"
              ];
              "after" = [];
              "commands" = [
                {
                  "command" = "workbench.action.previousEditor";
                  "args" = [];
                }
              ];
            }
            {
              "before" = ["<leader>" "m" "e" "e"];
              "after" = [];
              "commands" = [
                {
                  "command" = "calva.evaluateEnclosingForm";
                  "args" = [];
                }
              ];
            }
            {
              "before" = ["<leader>" "m" "e" "d"];
              "after" = [];
              "commands" = [
                {
                  "command" = "calva.evaluateCurrentTopLevelForm";
                  "args" = [];
                }
              ];
            }
            {
              "before" = ["<leader>" "k" "d" "x"];
              "after" = [];
              "commands" = [
                {
                  "command" = "paredit.killSexpForward";
                  "args" = [];
                }
              ];
            }
          ];
          "remote.SSH.defaultExtensions" = [
            "gitpod.gitpod-remote-ssh"
          ];
          "telemetry.telemetryLevel" = "off";
          "redhat.telemetry.enabled" = false;
          "files.autoSave" = "afterDelay";
          "files.autoSaveDelay" = 500;
          "terraform.languageServer.args" = [
            "serve"
          ];
          "python.languageServer" = "Pylance";
          "files.exclude" = {
            "/workspace/.**" = true;
          };
          "search.exclude" = {
            "/workspace/.**" = true;
          };
          "[typescript]" = {
            "editor.defaultFormatter" = "esbenp.prettier-vscode";
          };
          "calva.paredit.defaultKeyMap" = "strict";
          "editor.inlineSuggest.enabled" = true;
          "[json]" = {
            "editor.defaultFormatter" = "vscode.json-language-features";
          };
          "settingsSync.ignoredSettings" = [
            "configurationSync.store"
          ];
          "configurationSync.store" = {
            "url" = "https=//gitpod.io/code-sync";
            "stableUrl" = "https=//gitpod.io/code-sync";
            "insidersUrl" = "https=//gitpod.io/code-sync";
            "canSwitch" = false;
            "authenticationProviders" = {
              "gitpod" = {
                "scopes" = [
                  "function=accessCodeSyncStorage"
                  "function=getLoggedInUser"
                  "resource=default"
                ];
              };
            };
          };
          "[html]" = {
            "editor.defaultFormatter" = "vscode.html-language-features";
          };
          "[javascript]" = {
            "editor.defaultFormatter" = "esbenp.prettier-vscode";
          };
          "javascript.updateImportsOnFileMove.enabled" = "always";
          "explorer.confirmDragAndDrop" = false;
          "typescript.updateImportsOnFileMove.enabled" = "always";
        };
      };
    };
  };
}
