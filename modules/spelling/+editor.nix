{
  flake.aspects.spelling = {
    homeManager =
      { config, lib, ... }:
      {
        config = {
          editor.vimInit = ''
            set spell
            set spelllang=en_us
          '';
        };
      };
  };
}
