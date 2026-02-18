{
  flake.aspects = {
    config = {

      options = {
        homeManager =
          { lib, ... }:
          {
            options = {
              editor.vimInit = lib.mkOption {
                type = lib.types.lines;
                default = "";
                description = "Vim config fragments to concatenate from other aspects.";
                apply = x: lib.concatStringsSep "\n" (lib.toList x);
              };
            };
          };
      };
      editor = {
        homeManager =
          {
            config,
            pkgs,
            ...
          }:
          {
            config = {
              home.packages = with pkgs; [
                vim
              ];
              editor.vimInit = ''
                vim aspect
              '';
              home.file."__editor".text = config.editor.vimInit;
            };
          };
      };
    };
  };
}
