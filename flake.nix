{
  description = "Start of the system configuration flake";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ {nixpkgs, ...}: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
    };
    lib = nixpkgs.lib;
  in {
    nixosConfigurations = {
      auberon = lib.nixosSystem {
        inherit system;
        modules = [
          ./hosts/auberon
          inputs.sops-nix.nixosModules.sops
        ];
      };
      belisarius = lib.nixosSystem {
        inherit system;
        modules = [
          ./hosts/belisarius
          inputs.sops-nix.nixosModules.sops
        ];
      };
    };

    homeConfigurations.clover = inputs.home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      modules = [
        ./users/clover/home.nix
      ];
    };
  };
}
