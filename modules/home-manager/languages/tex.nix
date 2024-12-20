{
  config,
  lib,
  pkgs,
  myconf-literate-config,
  ...
}:

with lib;

let

  cfg = config.languages.tex;

  texlive = config.programs.texlive.package;
  texlab = cfg.texlab.package;

in
{

  options.languages.tex = {
    enable = mkEnableOption "{command} tex for editors.";

    texlab.enable = mkOption {
      type = types.bool;
      default = false;
      description = "{command}`texlab`";
    };

    texlab.package = mkOption {
      type = types.package;
      default = pkgs.texlab;
    };
  };

  config = {

    home.packages = optional cfg.texlab.enable texlab;

    programs.texlive = mkIf cfg.enable {
      enable = true;
      extraPackages =
        tpkgs:
        {
          inherit (tpkgs)
            collection-basic
            collection-latex
            ;
        }
        // myconf-literate-config.results.texlive.extraPackages tpkgs;
    };

    programs.emacs.setq =
      mkIf cfg.enable { lsp-clients-digestif-executable = "${texlive}/bin/digestif"; }
      // mkIf cfg.texlab.enable { lsp-clients-texlab-executable = "${texlab}/bin/texlab"; };

    programs.emacs.doomConfig.init.lang.latex = {
      enable = mkDefault cfg.enable;
      lsp = mkDefault cfg.texlab.enable;
    };
  };

}
