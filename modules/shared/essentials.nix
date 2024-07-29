{ config, lib, pkgs, ... }:

{
  config = {
    environment.systemPackages = with pkgs; [
      curl
      wget
      git
      openssl_3_2
    ];
  };
}
