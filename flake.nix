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
      }
      // {
        ags = ags.packages.${system}.default;
      };

    lib = nixpkgs.lib;

    userModules = builtins.attrNames (
      lib.filterAttrs (n: v: v == "directory")
      (builtins.readDir modules/users)
    );

    mkHomeCfg = name: let
      user = "${builtins.head (builtins.match "(.+)@.+" name)}";
      host = "${builtins.head (builtins.match ".+@(.+)" name)}";
      userLegacyModule = ./users + "/${user}";
    in {
      inherit name;
      value = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules =
          [
            ({...}: {
              home.username = user;
              home.homeDirectory = "/home/${user}";
              wolf.host = host;
            })
            ./modules/users
          ]
          ++ userModules
          ++ (
            if builtins.pathExists userLegacyModule
            then [userLegacyModule]
            else []
          );
        extraSpecialArgs = {inherit pkgs;};
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
                ./users/${user}
              ];
            };
          }
        )
      );
      hostLegacyModule = (
        if builtins.pathExists ./hosts/${host}
        then [./hosts/${host}]
        else []
      );
    in
      lib.nixosSystem {
        inherit system;
        inherit pkgs;
        modules =
          [
            ./modules/hosts
            sops-nix.nixosModules.sops
            home-manager.nixosModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                users = userHome;
                sharedModules =
                  [
                    ({...}: {
                      wolf.host = host;
                    })
                    ./modules/users
                  ]
                  ++ userModules;
              };
            }
          ]
          ++ hostLegacyModule;
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
