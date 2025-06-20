{
  description = "Per-env devShells and all combos (auto combo-list). Env shells as {packages, shell}";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        envs-dir = toString ./envs;
        env-names = builtins.filter (d: d != "meta" && builtins.pathExists (envs-dir + "/${d}/shell.nix")) (
          builtins.attrNames (builtins.readDir envs-dir)
        );
        # Compute all non-empty combos with 2 or more envs
        concatMap = f: xs: builtins.concatLists (map f xs);
        combosK =
          k: xs:
          if k == 0 then
            [ [ ] ]
          else if xs == [ ] then
            [ ]
          else
            (map (ys: [ (builtins.head xs) ] ++ ys) (combosK (k - 1) (builtins.tail xs)))
            ++ (combosK k (builtins.tail xs));
        n = builtins.length env-names;
        combo-list = concatMap (k: combosK k env-names) (lib.range 2 n); # k from 2 to n
        combo-name = combo: builtins.concatStringsSep "__" combo;
        # Reuse Nixpkgs' lib.range for 2..n
        lib = import (nixpkgs + "/lib");
        getEnv = name: import (envs-dir + "/${name}/shell.nix") { inherit pkgs; };
        getPackages = name: (getEnv name).packages or [ ];
        getShell = name: (getEnv name).shell;
        comboShell =
          combo:
          let
            packages = builtins.concatLists (map getPackages combo);
          in
          pkgs.mkShell {
            name = combo-name combo;
            packages = packages;
          };
        per-env-shells = builtins.listToAttrs (
          map (name: {
            name = name;
            value = getShell name;
          }) env-names
        );
        combo-shells = builtins.listToAttrs (
          map (combo: {
            name = combo-name combo;
            value = comboShell combo;
          }) combo-list
        );
        all-shells = per-env-shells // combo-shells;
      in
      {
        devShells = all-shells // {
          default = combo-shells.${combo-name env-names};
        };
      }
    );
}
