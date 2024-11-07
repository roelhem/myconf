{ ... }:

{
  config.programs.git = {
    includes = [ ];
    ignores = [
      # scratch files
      "/__*"
      # nix
      "/.direnv/"
      "/.devenv/"
      "/result"
      # org
      "scratch.org"
      "project.org"
      "todo.org"
      "notes.org"
      "changelog.org"
      # Auth info
      "/.authinfo"
      "/.authinfo.gpg"
      # mac
      ".DS_Store"
      ".AppleDouble"
      ".LSOverride"
      "Icon"
      "._*"
      ".DocumentRevisions-V100"
      ".fseventsd"
      ".Spotlight-V100"
      ".TemporaryItems"
      ".Trashes"
      ".VolumeIcon.icns"
      ".com.apple.timemachine.donotpresent"
      ".AppleDB"
      ".AppleDesktop"
      "Network Trash Folder"
      "Temporary Items"
      ".apdisk"
      "*.icloud"
      # linux
      "*~"
      ".fuse_hidden*"
      ".directory"
      ".Trash-*"
      ".nfs*"
      # windows
      "Thumbs.db"
      "Thumbs.db:encryptable"
      "ehthumbs.db"
      "ehthumbs_vista.db"
      "*.stackdump"
      "[Dd]esktop.ini"
      "$RECYCLE.BIN/"
      # vscode
      ".vscode/*"
      "!.vscode/settings.json"
      "!.vscode/tasks.json"
      "!.vscode/launch.json"
      "!.vscode/extensions.json"
      "!.vscode/*.code-snippets"
    ];
  };
}
