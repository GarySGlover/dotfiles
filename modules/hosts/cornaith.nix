{ inputs, ... }:
{
  flake.nixosConfigurations.cornaith = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules = [
      inputs.sops-nix.nixosModules.sops
      # Still using the legacy modules for now:
      (import ../../legacy-modules/hosts/cornaith/extra.nix)
      (import ../../legacy-modules/hosts/cornaith/hardware-configuration.nix)
      inputs.home-manager.nixosModules.home-manager
      {
        nixpkgs.pkgs = import inputs.nixpkgs {
          system = "x86_64-linux";
          config.allowUnfree = true;
          config.permittedInsecurePackages = [
            "python3.13-ecdsa-0.19.1"
          ];
        };
        home-manager = {
          extraSpecialArgs = {
            self = inputs.self;
            inputs = inputs;
          };
          useGlobalPkgs = true;
          useUserPackages = true;
          backupFileExtension = "hm-backup";
          users.clover = {
            home.username = "clover";
            home.homeDirectory = "/home/clover";
            wolf.secretsPath = ../../secrets;
            imports = import ../../legacy-modules/users/clover;
          };
          sharedModules = (import ../../legacy-modules/users/global);
        };
      }
      # Add more as needed, or migrate incrementally.
    ]
    ++ (import ../../legacy-modules/hosts/global/default.nix);
    specialArgs = {
      host = "cornaith";
      users = [ "clover" ];
      self = inputs.self;
    };
  };
}
