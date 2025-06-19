{
  description = "Meta dev environment: all combos, snake-case, double underscores";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    azure.url = "path:../azure";
    bash.url = "path:../bash";
    docker.url = "path:../docker";
    dotnet.url = "path:../dotnet";
    kubernetes.url = "path:../kubernetes";
    nix.url = "path:../nix";
    precommit.url = "path:../pre-commit";
    yaml.url = "path:../yaml";
  };

  outputs =
    {
      nixpkgs,
      flake-utils,
      ...
    }@inputs:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };

        envNames = builtins.sort builtins.lessThan (
          builtins.filter (
            name:
            !(builtins.elem name [
              "self"
              "nixpkgs"
              "flake-utils"
            ])
          ) (builtins.attrNames inputs)
        );

        getPkgs =
          name:
          let
            flake = inputs.${name};
          in
          flake.devShells.${system}.default.packages or [ ];

        concatMap = f: xs: builtins.concatLists (map f xs);

        combos =
          let
            n = builtins.length envNames;
            combosK =
              k: xs:
              if k == 0 then
                [ [ ] ]
              else if builtins.length xs == 0 then
                [ ]
              else
                (map (ys: [ (builtins.head xs) ] ++ ys) (combosK (k - 1) (builtins.tail xs)))
                ++ (combosK k (builtins.tail xs));
            allCombos = concatMap (k: combosK k envNames) (builtins.genList (i: i + 1) n);
          in
          allCombos;

        comboName = combo: builtins.concatStringsSep "__" combo;

        allComboShells = builtins.listToAttrs (
          map (combo: {
            name = comboName combo;
            value = pkgs.mkShell {
              name = comboName combo;
              packages = builtins.concatLists (map getPkgs combo);
            };
          }) combos
        );

      in
      {
        devShells = allComboShells // {
          default = allComboShells.${builtins.concatStringsSep "__" envNames};
        };
      }
    );
}
