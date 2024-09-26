{
  description = "Nix utilities I use in my configurations.";
  inputs = { };
  output =
    { }:
    let

      # My supported linux systems.
      linuxSystems = [
        "x86_64-linux"
        "aarch64-linux"
      ];

      # My supported darwin systems.
      darwinSystems = [
        "x86_64-darwin"
        "aarch64-darwin"
      ];

    in
    {
      lib = {
        inherit linuxSystems darwinSystems;
      };
    };
}
