{
  config,
  lib,
  ...
}:
with lib; let
  opt = config.wolf;
  gaming_hosts = [];
  internet_hosts = ["auberon" "belisarius"];
in {
  wolf = {
    roles = {
      devops = true;
      gaming = false;
      editing = true;
      electrical = false;
      programming = true;
      internet =
        if lists.any (i: opt.host == i) internet_hosts
        then true
        else false;
      work = true;
    };

    languages = {
      json = true;
      nix = true;
      python = true;
    };
    user.interactive = true;
  };
}
