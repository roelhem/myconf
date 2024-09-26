{ ... }:

{
  config.programs.fd = {
    ignores = [
      ".git/"
      "*.bak"
    ];
  };
}
