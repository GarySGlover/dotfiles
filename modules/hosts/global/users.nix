{
  config,
  lib,
  pkgs,
  users,
  ...
}:
with lib;
with builtins; let
  adminUser = head users;
  createUsers = mapAttrs (n: v:
    v
    // {
      extraGroups =
        (
          if (hasAttr "extraGroups" v)
          then v.extraGroups
          else []
        )
        ++ (
          if (n == adminUser)
          then ["wheel"]
          else []
        );
    }) {
    clover = {
      isNormalUser = true;
      home = "/home/clover";
      uid = 1000;
      hashedPasswordFile = config.sops.secrets.clover-password.path;
      shell = pkgs.fish;
      extraGroups = ["realtime"];
    };
    work = {
      isNormalUser = true;
      home = "/home/work";
      uid = 1001;
      hashedPasswordFile = config.sops.secrets.work-password.path;
      shell = pkgs.fish;
    };
  };
in {
  sops.secrets = with builtins;
    listToAttrs (
      map (x: {
        name = "${x}-password";
        value = {
          "neededForUsers" = true;
        };
      })
      users
    );

  programs.fish.enable = true;

  users.users = filterAttrs (n: v: lists.any (i: i == n) users) createUsers;
}
