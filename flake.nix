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

    listNixFilesRecursive = dir:
      lib.flatten (lib.mapAttrsToList (name: type: let
        path = dir + "/${name}";
      in
        if type == "directory"
        then
          if lib.pathExists (dir + "/${name}/default.nix")
          then path
          else listNixFilesRecursive path
        else if lib.hasSuffix ".nix" name
        then path
        else []) (builtins.readDir dir));

    pkgs =
      import nixpkgs {
        inherit system;
        config.allowUnfreePredicate = pkg:
          builtins.elem (lib.getName pkg) [
            "steam"
            "steam-original"
            "steam-run"
          ];
      }
      // {
        ags = ags.packages.${system}.default;
      };

    lib = nixpkgs.lib;

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
              wolf.secretsPath = ./secrets;
            })
          ]
          ++ (listNixFilesRecursive ./modules/users)
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
              wolf.secretsPath = ./secrets;
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
                  ]
                  ++ (listNixFilesRecursive ./modules/users);
              };
            }
          ]
          ++ (listNixFilesRecursive ./modules/hosts)
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
