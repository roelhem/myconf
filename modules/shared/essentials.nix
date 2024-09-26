{ pkgs, ... }:

{
  config = {
    environment.systemPackages = with pkgs; [
      curl
      wget
      git
      gnugrep
      openssl_3_2
    ];
  };
}
