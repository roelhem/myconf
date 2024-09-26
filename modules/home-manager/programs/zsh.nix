{
  config,
  pkgs,
  lib,
  ...
}:

with lib;

let

  cfg = config.programs.zsh;

in
{
  options.programs.zsh = {
    vi-mode.enable = mkEnableOption "{command} `zsh-vi-mode`";
  };

  config.programs.zsh = {
    plugins = optional cfg.vi-mode.enable {
      name = "vi-mode";
      src = pkgs.zsh-vi-mode;
      file = "share/zsh-vi-mode/zsh-vi-mode.plugin.zsh";
    };

    initExtra = ''

    '';
  };
}
