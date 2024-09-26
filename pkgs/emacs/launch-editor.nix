{ writeShellScript, emacs }:

writeShellScript "emacs-launch-editor" ''
  ${emacs}/bin/emacsclient -n -u "+$2:$3" "$1"
''
