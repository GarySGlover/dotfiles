{
  description = "Start of the system configuration flake";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-23.05";
    home-manager = {
      url = "github:nix-community/home-manager/release-23.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, home-manager, sops-nix, ... }:
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      config = { allowUnfree = true; };
    };
    lib = nixpkgs.lib;
  in {
    nixosConfigurations = {
      auberon = lib.nixosSystem {
        inherit system;
	modules = [
	  ./hosts/auberon
	  sops-nix.nixosModules.sops
	  home-manager.nixosModules.home-manager {
	    home-manager.useGlobalPkgs = true;
	    home-manager.users.clover = import ./users/clover/home.nix;
	  }
	];
      };
    };

    homeConfigurations.clover = home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      modules = [
        ./users/clover/home.nix
      ];
    };
  };
}
