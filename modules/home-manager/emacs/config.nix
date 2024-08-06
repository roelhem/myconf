{ config, lib, pkgs, ... }:

with lib;

let emacs = config.programs.emacs.finalPackage;

    # elisp = pkgs.writeShellScriptBin "elisp" ''
    #   [ -t 0 ] && sexp="($*)" || sexp="$(cat)"
    #   exec ${emacs}/bin/emacsclient -e "$sexp"
    #   '';

    # e = pkgs.writeShellScriptBin "e" ''
    #   if [ -t 0 ]; then
    #     ${emacs}/bin/emacsclient -n "$@"
    #   else
    #     tmpf=$(${pkgs.mktemp}/bin/mktemp --tmpdir emacs-edit.XXXXXXXXX)
    #     cat > $tmpf
    #     trap 'rm -rf $tmpf; trap -- EXIT; exit' EXIT INT HUP
    #     ${elisp}/bin/elisp <<EOF
    #       (progn
    #         (let ((dir default-directory))
    #           (find-file "$tmpf")
    #           (setq default-directory dir)
    #           (set-visited-file-name nil)
    #           (rename-buffer "*stdin*" t)))
    #   EOF
    #   fi
    #   '';

    cfg = config.programs;


in {

  config = {
    home.sessionVariables = {
      EDITOR = "${emacs}/bin/emacsclient";
    };

    home.file.".doom.d/nix-deps.el" = mkIf cfg.doomemacs.enable {
      text = ''
        ${pkgs.lib.elisp.toSetq {
          dhall-command = "${pkgs.dhall}/bin/dhall";
          nix-nixfmt-bin = "${pkgs.nixfmt}/bin/nixfmt";
          sh-shellcheck-program = "${pkgs.shellcheck}/bin/shellcheck";
          doom-projectile-fd-binary = "${pkgs.fd}/bin/fd";
          lsp-csharp-omnisharp-roslyn-binary-path = "${pkgs.omnisharp-roslyn}/bin/OmniSharp";
          lsp-phpactor-path = "${pkgs.phpactor}/bin/phpactor";
          myconf-nix-exec-path = [
            "${pkgs.phpactor}/bin"
            "${pkgs.nodePackages.typescript}/bin"
            "${pkgs.nodePackages.typescript-language-server}/bin"
          ];
        }}


        (setq exec-path (append myconf-nix-exec-path exec-path))
        '';
    };

    programs.zsh = {
      # shellAliases = {
      #   emacs = "${e}/bin/e";
      # };
      # shellAliases = mkMerge [
      #   (mkIf cfg.emacs.enable {
      #     emacs = "${emacs}/Applications/Emacs.app/Contents/MacOS/Emacs";
      #   })
      #   (mkIf cfg.doomemacs.enable {
      #     doomemacs = "${doomemacs}/Applications/DoomEmacs.app/Contents/MacOS/Emacs";
      #   })
      # ];
    };

    programs.emacs = {
      package = pkgs.emacs-plus;
    };

    launchd.agents.emacs = {
      enable = false;
      config = {
        Label = "emacs";
        ProgramArguments = ["${emacs}/Applications/Emacs.app/Contents/MacOS/Emacs" "--fg-daemon"];
        ProcessType = "Interactive";
        # EnvironmentVariables = {};
        KeepAlive = {
          SuccessfulExit = true;
        };
      };
    };
  };
}
