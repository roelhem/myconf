{
  writeShellScript,
  emacs,
}:

writeShellScript "emacs-launch-editor" ''
  ${emacs}/bin/emacsclient "+$2:$3" "$1"
''
