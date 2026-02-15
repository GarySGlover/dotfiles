{
  flake.aspects.spelling = {
    homeManager = {
      config = {
        home.file."__spelling".text = ''
          spelling aspect
        '';
      };
    };
  };
}
