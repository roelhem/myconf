{ pkgs, ... }:

{
  config = {
    environment.systemPackages = with pkgs; [
      moreutils
      curl
      wget
      git
      niv
      gnugrep
      openssl
    ];
  };
}
