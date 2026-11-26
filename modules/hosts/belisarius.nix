{ inputs, ... }:
{
  flake.nixosConfigurations.belisarius = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules = [
      inputs.self.modules.nixos.core
      inputs.self.modules.nixos.entertainment
      inputs.self.modules.nixos.guix
      inputs.self.modules.nixos.tilingWindowManager
      inputs.sops-nix.nixosModules.sops
      # Still using the legacy modules for now:
      (import ../../legacy-modules/hosts/belisarius/extra.nix)
      (import ../../legacy-modules/hosts/belisarius/hardware-configuration.nix)
      inputs.home-manager.nixosModules.home-manager
      {
        nixpkgs.pkgs = import inputs.nixpkgs {
          system = "x86_64-linux";
          config.allowUnfree = true;
          config.permittedInsecurePackages = [
            "python3.13-ecdsa-0.19.1"
          ];
          overlays = [
            # Upstream only adds `mkEmacsWith`; it deliberately does not replace
            # `emacs` (doing so broke nixpkgs' `emacsPackages = emacs.pkgs`,
            # which `trivialBuild`/treesit-grammars rely on), so no stock-`emacs`
            # shim is needed anymore.
            inputs.fourleafclover-nix-config.overlays.default
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
            imports = with inputs.self.modules.homeManager; [
              core
              devops
              editor
              environment
              options
              programming
              style
              terminal
              tilingWindowManager
            ];
          };
          sharedModules = (import ../../legacy-modules/users/global);
        };
      }
      # Add more as needed, or migrate incrementally.
    ]
    ++ (import ../../legacy-modules/hosts/global/default.nix);
    specialArgs = {
      host = "belisarius";
      users = [ "clover" ];
      self = inputs.self;
    };
  };
}
