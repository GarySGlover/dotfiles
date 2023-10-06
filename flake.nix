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

  outputs = {
    self,
    nixpkgs,
    home-manager,
    ags,
    sops-nix,
    ...
  }: let
    system =
      if builtins ? currentSystem
      then builtins.currentSystem
      else "x86_64-linux";

    pkgs =
      import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      }
      // {
        ags = ags.packages.${system}.default;
      };

    lib = nixpkgs.lib;

    mkHomeCfg = name: let
      username = "${builtins.head (builtins.match "(.+)@.+" name)}";
      hostname = "${builtins.head (builtins.match ".+@(.+)" name)}";
      homeDir = "/home/${username}";
    in {
      inherit name;
      value = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          ({...}: {
            home.username = username;
            home.homeDirectory = homeDir;
            clover.hostname = hostname;
          })
          (./users + "/${username}")
        ];
        extraSpecialArgs = {inherit homeDir pkgs;};
      };
    };

    mkNixOsCfg = {
      host,
      users,
    }: let
      userHome = builtins.listToAttrs (
        lib.lists.forEach users (
          user: {
            name = "${user}";
            value = {pkgs, ...}: {
              home.username = "${user}";
              home.homeDirectory = "/home/${user}";
              imports = [
                ./users/clover
              ];
            };
          }
        )
      );
    in
      lib.nixosSystem {
        inherit system;
        inherit pkgs;
        modules = [
          ./hosts/${host}
          sops-nix.nixosModules.sops
          home-manager.nixosModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              users = userHome;
            };
          }
        ];
      };

    hostCfgs = {
      belisarius = ["clover"];
      auberon = ["clover" "work"];
      clover-z270pd3 = ["clover"];
    };

    homeCfgs = lib.lists.flatten (
      builtins.attrValues (
        builtins.mapAttrs (host: users:
          lib.lists.forEach users (
            user: "${user}@${host}"
          ))
        hostCfgs
      )
    );
  in {
    nixosConfigurations = builtins.mapAttrs (host: users:
      mkNixOsCfg {inherit host users;})
    hostCfgs;

    homeConfigurations = builtins.listToAttrs (
      lib.lists.forEach homeCfgs (
        name: mkHomeCfg name
      )
    );
  };
}
