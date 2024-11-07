{ ... }:

{
  config = {
    services.yabai = {
      config = {
        layout = "bsp";
      };
      extraConfig = ''
        yabai -m rule --add app="^System Settings$" manage=off
        yabai -m rule --add app="^System Information$" manage=off
        yabai -m rule --add app="^System Preferences$" manage=off
        yabai -m rule --add app="Preferences$" manage=off
        yabai -m rule --add app="Systeeminstellingen" manage=off
        yabai -m rule --add app="Finder" manage=off
      '';
    };
  };
}
