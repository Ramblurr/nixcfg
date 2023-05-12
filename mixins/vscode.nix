{
  config,
  lib,
  pkgs,
  ...
}: {
  home-manager.users.ramblurr = {pkgs, ...}: {
    home.packages = with pkgs; [
      vscode.fhs
    ];
    home.persistence."/persist/home/ramblurr" = {
      directories = [
        ".vscode/extensions"
        ".config/Code"
        ".config/code-work"
        ".config/code-personal"
      ];
    };

    home.file.".local/share/applications/code-personal.desktop" = {
      text = ''
        [Desktop Entry]
        Name=Personal Visual Studio Code
        Comment=Code Editing. Redefined.
        GenericName=Text Editor
        Exec=/etc/profiles/per-user/ramblurr/bin/code --user-data-dir /home/ramblurr/.config/code-personal --unity-launch %F
        Icon=com.visualstudio.code
        Type=Application
        StartupNotify=false
        StartupWMClass=vscode-personal
        Categories=Utility;TextEditor;Development;IDE;
        MimeType=text/plain;inode/directory;application/x-code-workspace;
        Keywords=vscode-personal;
      '';
    };
    home.file.".local/share/applications/code-personal-url-handler.desktop" = {
      text = ''
        [Desktop Entry]
        Name=Personal Visual Studio Code - URL Handler
        Comment=Code Editing. Redefined.
        GenericName=Text Editor
        Exec=/etc/profiles/per-user/ramblurr/bin/code --user-data-dir /home/ramblurr/.config/code-personal --open-url %U
        Icon=com.visualstudio.code
        Type=Application
        NoDisplay=true
        StartupNotify=true
        StartupWMClass=vscode-personal
        Categories=Utility;TextEditor;Development;IDE;
        MimeType=x-scheme-handler/vscode;x-scheme-handler/vscode-insiders;
        Keywords=vscode-personal;
      '';
    };
    home.file.".local/share/applications/code-work-url-handler.desktop" = {
      text = ''
        [Desktop Entry]
        Name=Work Visual Studio Code - URL Handler
        Comment=Code Editing. Redefined.
        GenericName=Text Editor
        Exec=/etc/profiles/per-user/ramblurr/bin/code --user-data-dir /home/ramblurr/.config/code-work --open-url %U
        Icon=com.visualstudio.code
        Type=Application
        NoDisplay=true
        StartupNotify=true
        StartupWMClass=vscode-work
        Categories=Utility;TextEditor;Development;IDE;
        MimeType=x-scheme-handler/vscode;x-scheme-handler/vscode-insiders;
        Keywords=vscode-work;
      '';
    };
    home.file.".local/share/applications/code-work.desktop" = {
      text = ''
        [Desktop Entry]
        Name=Work Visual Studio Code
        Comment=Code Editing. Redefined.
        GenericName=Text Editor
        Exec=/etc/profiles/per-user/ramblurr/bin/code --user-data-dir /home/ramblurr/.config/code-work --unity-launch %F
        Icon=com.visualstudio.code
        Type=Application
        StartupNotify=false
        StartupWMClass=vscode-work
        Categories=Utility;TextEditor;Development;IDE;
        MimeType=text/plain;inode/directory;application/x-code-workspace;
        Keywords=vscode-work;
      '';
    };
  };
}
