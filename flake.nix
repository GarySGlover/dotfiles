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
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
    };
    import-tree = {
      url = "github:vic/import-tree";
    };
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-aspects = {
      url = "github:vic/flake-aspects";
    };
    wrappers = {
      url = "github:lassulus/wrappers";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    stylix = {
      url = "github:nix-community/stylix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-parts.follows = "flake-parts";
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
    gptel-quick = {
      url = "github:karthink/gptel-quick";
      flake = false;
    };

    # MCP Servers
    modelcontextprotocol-servers = {
      url = "github:modelcontextprotocol/servers";
      flake = false;
    };
    azure-devops-mcp = {
      url = "github:microsoft/azure-devops-mcp";
      flake = false;
    };
  };

  outputs =
    inputs@{
      flake-parts,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } (
      {
        # top@{
        # config,
        # withSystem,
        # moduleWithSystem,
        ...
      }:
      {
        imports = [
          inputs.flake-aspects.flakeModule
          (inputs.import-tree ./modules)
        ];
        flake =
          let
            system = if builtins ? currentSystem then builtins.currentSystem else "x86_64-linux";
            lib = pkgs.lib;
            pkgs = import inputs.nixpkgs {
              inherit system;
              config = import ./config.nix { inherit lib; };
              overlays =
                let
                  overlayFiles = builtins.filter (file: builtins.match ".*\\.nix$" file != null) (
                    builtins.attrNames (builtins.readDir ./legacy-modules/overlays)
                  );
                  importedOverlays = map (file: import (./legacy-modules/overlays + "/${file}") inputs) overlayFiles;
                in
                importedOverlays;
            };

            extraSpecialArgs = {
              inherit pkgs inputs;
              self = inputs.self;
            };

          in
          {
            homeConfigurations = {
              "gary_glover@next.co.uk" = inputs.home-manager.lib.homeManagerConfiguration {
                inherit pkgs extraSpecialArgs;
                modules = [
                  (
                    { ... }:
                    {
                      home.username = "gary_glover";
                      home.homeDirectory = "/home/gary_glover";
                      wolf.secretsPath = ./secrets;
                    }
                  )
                ]
                ++ (import ./legacy-modules/users/global)
                ++ (import ./legacy-modules/users/clover);
              };
            };

            devShells.${system}.default = pkgs.mkShell {
              packages = with pkgs; [

                # Pre-commit
                (pre-commit.overrideAttrs (_: {
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

                # Task
                go-task

                # Nix
                nixfmt
                nixd

                # Emacs init development
                glib
              ];
            };
          };
        systems = [
          "x86_64-linux"
        ];
      }
    );
}
