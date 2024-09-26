{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  emacs = config.programs.emacs.finalPackage;

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

  emacs-zsh-plugin = pkgs.emacs-zsh-plugin;

  emacs-launch-editor = pkgs.emacs-launch-editor.override { inherit emacs; };

  # dhall-lsp-server = "${pkgs.dhall-lsp-server}/bin/dhall-lsp-server";

in
{

  options = {
    programs.emacs = {
      binaries = mkOption {
        type = with types; attrsOf string;
        description = "Sets the values for the `rh-nix-programs` variable.";
      };

      setq = mkOption {
        type = with types; attrsOf anything;
        description = "Set variables at the beginning of the config.el file. Values can be normal nix values.";
        default = { };
      };

      enableZshIntegration = mkOption {
        type = types.bool;
        description = "Enable zsh integration.";
        default = cfg.emacs.enable;
      };
    };
  };

  config = {
    home.sessionVariables = {
      EDITOR = "${emacs}/bin/emacsclient";
      LAUNCH_EDITOR = "${emacs-launch-editor}";
    };

    home.file.".doom.d/nix-deps.el" = mkIf cfg.doomemacs.enable {
      text = ''
        ${pkgs.lib.elisp.toSetq cfg.emacs.setq}
      '';
    };

    programs.zsh = mkIf cfg.emacs.enableZshIntegration {
      initExtra = emacs-zsh-plugin.activationScript;
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
      setq =
        mkIf config.programs.fd.enable {
          doom-projectile-fd-binary = "${config.programs.fd.package}/bin/fd";
          projectile-fd-executable = "${config.programs.fd.package}/bin/fd";
        }
        // mkIf config.programs.direnv.enable {
          envrc-direnv-executable = "${config.programs.direnv.package}/bin/direnv";
        }
        // {
          # lsp-clojure-custom-server-command = "${pkgs.clojure-lsp}/bin/clojure-lsp";
          lsp-pwsh-exe = "${pkgs.powershell}/bin/pwsh";
          # lsp-sourcekit-executable = "${pkgs.sourcekit-lsp}/bin/sourcekit-lsp";
          lsp-rust-rustfmt-path = "${pkgs.rustfmt}/bin/rustfmt";
        };
      extraPackages =
        epkgs:
        let
          org-cv = pkgs.callPackage ../../../../pkgs/emacs/org-cv.nix {
            inherit (epkgs) trivialBuild ox-hugo dash;
          };

        in
        [ org-cv ];
    };

    launchd.agents.emacs = {
      enable = false;
      config = {
        Label = "emacs";
        ProgramArguments = [
          "${emacs}/Applications/Emacs.app/Contents/MacOS/Emacs"
          "--fg-daemon"
        ];
        ProcessType = "Interactive";
        # EnvironmentVariables = {};
        KeepAlive = {
          SuccessfulExit = true;
        };
      };
    };
  };
}
