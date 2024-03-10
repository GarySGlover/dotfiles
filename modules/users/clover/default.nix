{
  config,
  lib,
  ...
}:
with lib; let
  opt = config.wolf;
  gaming_hosts = ["belisarius"];
  internet_hosts = ["auberon" "belisarius"];
  cad_hosts = ["belisarius"];
in {
  wolf = {
    roles = {
      devops = true;
      gaming =
        if lists.any (i: opt.host == i) gaming_hosts
        then true
        else false;
      editing = true;
      cad = true;
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
