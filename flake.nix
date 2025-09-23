{
  description = "Clover Nix Configuration";

  nixConfig = {
    extra-experimental-features = "nix-command flakes";
    extra-substituters = "https://nix-community.cachix.org";
    extra-trusted-public-keys = "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=";
  };

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-wsl = {
      url = "github:nix-community/NixOS-WSL";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay/master";
    ags.url = "github:aylur/ags";
    nur.url = "github:nix-community/NUR";
    qutebrowser-src = {
      url = "github:qutebrowser/qutebrowser";
      flake = false;
    };

    # Emacs Packages
    transient-compile = {
      url = "github:gavv/transient-compile";
      flake = false;
    };
    kbd-mode = {
      url = "github:kmonad/kbd-mode";
      flake = false;
    };
    eglot-booster = {
      url = "github:jdtsmith/eglot-booster";
      flake = false;
    };
    ws-butler = {
      url = "github:lewang/ws-butler";
      flake = false;
    };
    org-menu = {
      url = "github:sheijk/org-menu";
      flake = false;
    };

    # MCP Servers
    modelcontextprotocol-servers = {
      url = "github:modelcontextprotocol/servers";
      flake = false;
    };
  };

  outputs =
    { self, ... }@inputs:
    let
      wolfLib = import ./functions.nix { lib = inputs.nixpkgs.lib; };
      lib = inputs.nixpkgs.lib;
      inherit (lib)
        pathExists
        hasSuffix
        mapAttrsToList
        forEach
        flatten
        filterAttrs
        mkForce
        nixosSystem
        ;
      inherit (builtins)
        readDir
        head
        match
        mapAttrs
        listToAttrs
        attrValues
        ;

      system = if builtins ? currentSystem then builtins.currentSystem else "x86_64-linux";
      listNixFilesRecursive =
        dir:
        flatten (
          mapAttrsToList (
            name: type:
            let
              path = dir + "/${name}";
            in
            if type == "directory" then
              if pathExists (dir + "/${name}/default.nix") then path else listNixFilesRecursive path
            else if hasSuffix ".nix" name then
              path
            else
              [ ]
          ) (readDir dir)
        );
      pkgs = import inputs.nixpkgs {
        inherit system;
        config = import ./config.nix { inherit lib; };
        overlays =
          let
            overlayFiles = builtins.filter (file: builtins.match ".*\\.nix$" file != null) (
              builtins.attrNames (builtins.readDir ./modules/overlays)
            );
            importedOverlays = map (file: import (./modules/overlays + "/${file}") inputs) overlayFiles;
          in
          [
            inputs.emacs-overlay.overlay
            inputs.nur.overlays.default
          ]
          ++ importedOverlays;
      };

      extraSpecialArgs = {
        inherit
          pkgs
          wolfLib
          self
          inputs
          ;
      };

      mkHomeCfg =
        name:
        let
          user = "${head (match "(.+)@.+" name)}";
          host = "${head (match ".+@(.+)" name)}";
        in
        {
          inherit name;
          value = inputs.home-manager.lib.homeManagerConfiguration {
            inherit pkgs;
            inherit extraSpecialArgs;
            modules = [
              (
                { ... }:
                {
                  home.username = user;
                  home.homeDirectory = mkForce "/home/${user}";
                  wolf.host = host;
                  wolf.secretsPath = ./secrets;
                }
              )
            ]
            ++ listNixFilesRecursive ./modules/users/global
            ++ listNixFilesRecursive ./modules/users/${user};
          };
        };

      mkNixOsCfg =
        {
          host,
          users,
        }:
        let
          inherit (lib) forEach listToAttrs optionals;
          userHome = listToAttrs (
            forEach users (user: {
              name = "${user}";
              value = {
                home.username = "${user}";
                home.homeDirectory = mkForce "/home/${user}";
                wolf.secretsPath = ./secrets;
                imports = listNixFilesRecursive ./modules/users/${user};
              };
            })
          );
          hostLegacyModule = optionals (pathExists ./hosts/${host}) [ ./hosts/${host} ];
          specialArgs = {
            inherit
              host
              users
              self
              inputs
              ;
          };
        in
        nixosSystem {
          inherit system specialArgs;
          inherit pkgs;
          modules = [
            inputs.sops-nix.nixosModules.sops
            inputs.home-manager.nixosModules.home-manager
            {
              home-manager = {
                inherit extraSpecialArgs;
                useGlobalPkgs = true;
                useUserPackages = true;
                backupFileExtension = "hm-backup";
                users = userHome;
                sharedModules = [
                  (
                    { ... }:
                    {
                      wolf.host = host;
                    }
                  )
                ]
                ++ (listNixFilesRecursive ./modules/users/global);
              };
            }
          ]
          ++ (listNixFilesRecursive ./modules/hosts/global)
          ++ (listNixFilesRecursive ./modules/hosts/${host})
          ++ hostLegacyModule;
        };

      hostCfgs = {
        auberon = {
          nixos = true;
          users = [
            "clover"
            "work"
          ];
        };
        belisarius = {
          nixos = true;
          users = [ "clover" ];
        };
        cornaith = {
          nixos = true;
          users = [ "clover" ];
        };
        clover-z270pd3 = {
          nixos = false;
          users = [ "clover" ];
        };
        MW-RSY-GPRG8C3 = {
          nixos = false;
          users = [
            "gary"
            "clover"
          ];
        };
      };

      homeCfgs = flatten (
        attrValues (mapAttrs (host: v: forEach v.users (user: "${user}@${host}")) hostCfgs)
      );
    in
    {
      nixosConfigurations =
        mapAttrs (
          host: v:
          mkNixOsCfg {
            inherit host;
            users = v.users;
          }
        ) (filterAttrs (n: v: v.nixos) hostCfgs)
        // {
          live = nixosSystem {
            inherit system;
            inherit pkgs;
            modules = [
              (inputs.nixpkgs + "/nixos/modules/installer/cd-dvd/installation-cd-graphical-gnome.nix")
              ./modules/hosts/live/live.nix
            ];
          };
        };

      homeConfigurations = listToAttrs (forEach homeCfgs (name: mkHomeCfg name));
      devShells.${system}.default = pkgs.mkShell {
        packages = with pkgs; [

          # Pre-commit
          (pre-commit.overrideAttrs (oldAttrs: {
            makeWrapperArgs = ''
              --set PYTHONPATH $PYTHONPATH
              --suffix PYTHONPATH : ${
                python3.withPackages (ps: [
                  ps.gitpython
                  ps.click
                ])
              }/lib/python3.13/site-packages
            '';
          }))
          yamlfmt
          yamllint

          # Shell
          shfmt
          argbash

          # Nix
          nixfmt-rfc-style
          nixd
          nix

          # TypeScript
          typescript-language-server

          # Formatter for various languages
          nodePackages.prettier

          # Python
          python313
          python313Packages.black
          python313Packages.flake8
          python313Packages.pipx
          python313Packages.pip
          pyright

          # Emacs init development
          glib
        ];
      };
    };
}
