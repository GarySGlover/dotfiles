{config, ...}: let
  opt = config.wolf;
in {
  wolf = {
    roles = {
      devops = true;
      gaming = true;
      editing = true;
      electrical = true;
      programming = true;
      internet =
        if opt.host == "clover-z270pd3"
        then false
        else true;
    };

    languages = {
      json = true;
      nix = true;
      python = true;
    };
    user.interactive = true;
  };
}
