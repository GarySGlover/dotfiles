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

    listNixFilesRecursive = with builtins;
    with lib;
      dir:
        flatten (mapAttrsToList (name: type: let
          path = dir + "/${name}";
        in
          if type == "directory"
          then
            if pathExists (dir + "/${name}/default.nix")
            then path
            else listNixFilesRecursive path
          else if hasSuffix ".nix" name
          then path
          else []) (readDir dir));

    pkgs =
      import nixpkgs {
        inherit system;
        config = import ./config.nix {inherit lib;};
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
        modules = with builtins;
        with lib.lists;
          [
            ({...}: {
              home.username = user;
              home.homeDirectory = lib.mkForce "/home/${user}";
              wolf.host = host;
              wolf.secretsPath = ./secrets;
            })
          ]
          ++ listNixFilesRecursive ./modules/users
          ++ optionals (pathExists userLegacyModule) [userLegacyModule];
        extraSpecialArgs = {inherit pkgs;};
      };
    };

    mkNixOsCfg = {
      host,
      users,
    }: let
      userHome = with builtins;
      with lib.lists;
        listToAttrs (
          forEach users (
            user: {
              name = "${user}";
              value = {pkgs, ...}: {
                home.username = "${user}";
                home.homeDirectory = lib.mkForce "/home/${user}";
                wolf.secretsPath = ./secrets;
                imports = [
                  ./users/${user}
                ];
              };
            }
          )
        );
      hostLegacyModule = with builtins;
      with lib.lists; optionals (pathExists ./hosts/${host}) [./hosts/${host}];
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
      belisarius = {
        nixos = true;
        users = ["clover"];
      };
      auberon = {
        nixos = true;
        users = ["clover"];
      };
      clover-z270pd3 = {
        nixos = false;
        users = ["clover"];
      };
    };

    homeCfgs = with builtins;
    with lib.lists;
      flatten (
        attrValues (
          mapAttrs (host: v:
            forEach v.users (
              user: "${user}@${host}"
            ))
          hostCfgs
        )
      );
  in {
    nixosConfigurations = with builtins;
    with lib;
      mapAttrs (host: v:
        mkNixOsCfg {
          inherit host;
          users = v.users;
        })
      (filterAttrs (n: v: v.nixos) hostCfgs);

    homeConfigurations = with builtins;
    with lib.lists;
      listToAttrs (
        forEach homeCfgs (
          name: mkHomeCfg name
        )
      );
  };
}
