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
    ags.url = "github:Aylur/ags";
  };

  outputs = inputs @ {nixpkgs, ...}: let
    system = "x86_64-linux";
    pkgs =
      import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      }
      // {
        ags = inputs.ags.packages.${system}.default;
      };
    lib = nixpkgs.lib;
  in {
    nixosConfigurations = {
      auberon = lib.nixosSystem {
        inherit system;
        inherit pkgs;
        modules = [
          ./hosts/auberon
          inputs.sops-nix.nixosModules.sops
        ];
      };
      belisarius = lib.nixosSystem {
        inherit system;
        inherit pkgs;
        modules = [
          ./hosts/belisarius
          inputs.sops-nix.nixosModules.sops
        ];
      };
    };

    homeConfigurations."clover@belisarius" = inputs.home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = {inherit inputs pkgs;};
      modules = [
        ./hosts/belisarius/clover_belisarius.nix
      ];
    };
    homeConfigurations."clover@auberon" = inputs.home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = {inherit inputs pkgs;};
      modules = [
        ./hosts/auberon/clover_auberon.nix
      ];
    };
  };
}
