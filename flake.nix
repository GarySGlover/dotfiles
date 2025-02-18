{
  description = "Clover Nix Configuration";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:booxter/home-manager/revert-6421-native-messenger";
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
    ags.url = "github:aylur/ags/main";
    nur.url = "github:nix-community/NUR";

    # Emacs Packages
    transient-compile = {
      url = "github:gavv/transient-compile";
      flake = false;
    };
    kbd-mode = {
      url = "github:kmonad/kbd-mode";
      flake = false;
    };
    org-modern-indent = {
      url = "github:jdtsmith/org-modern-indent";
      flake = false;
    };
    eglot-booster = {
      url = "github:jdtsmith/eglot-booster";
      flake = false;
    };
  };

  outputs =
    {
      emacs-overlay,
      home-manager,
      nixpkgs,
      sops-nix,
      ags,
      nur,
      transient-compile,
      kbd-mode,
      org-modern-indent,
      eglot-booster,
      self,
      ...
    }:
    let
      lib = nixpkgs.lib;
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

      pkgs = import nixpkgs {
        inherit system;
        config = import ./config.nix { inherit lib; };
        overlays = [
          emacs-overlay.overlay
          (final: prev: {
            inherit
              transient-compile
              kbd-mode
              org-modern-indent
              eglot-booster
              ;
          })
          (import ./modules/overlays/tree-sitter-grammars.nix)
          (import ./modules/overlays/codeium.nix)
          nur.overlay
        ];
      };

      extraSpecialArgs = {
        inherit
          pkgs
          ags
          self
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
          value = home-manager.lib.homeManagerConfiguration {
            inherit pkgs;
            inherit extraSpecialArgs;
            modules =
              [
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
              ;
          };
        in
        nixosSystem {
          inherit system specialArgs;
          inherit pkgs;
          modules =
            [
              sops-nix.nixosModules.sops
              home-manager.nixosModules.home-manager
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
                  ] ++ (listNixFilesRecursive ./modules/users/global);
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
          users = [ "gary" ];
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
              (nixpkgs + "/nixos/modules/installer/cd-dvd/installation-cd-graphical-gnome.nix")
              ./modules/hosts/live/live.nix
            ];
          };
        };

      homeConfigurations = listToAttrs (forEach homeCfgs (name: mkHomeCfg name));
      devShells.${system}.default = pkgs.mkShell {
        packages = with pkgs; [
          # Pre-commit
          pre-commit
          yamlfmt
          yamllint

          # Shell
          shfmt
          argbash

          # Nix
          nixfmt-rfc-style
          nixd

          # Formatter for various languages
          nodePackages.prettier

          # Python
          python312
          python312Packages.black
          python312Packages.flake8
          python312Packages.pipx
          python312Packages.pip
          pyright
        ];
      };
    };
}
