{ lib }:
let
  none = "";
  catchall = "catchall";
  mod = {
    super = "SUPER";
    alt = "ALT";
    ctrl = "CONTROL";
    control = "CONTROL";
    shift = "SHIFT";
  };

  direction = {
    left = "l";
    right = "r";
    up = "u";
    down = "d";
  };

  resize = {
    left = "-10 0";
    right = "10 0";
    up = "0 -10";
    down = "0 10";
    leftBy = x: "-${x} 0";
    rightBy = x: "${x} 0";
    upBy = x: "0 -${x}";
    downBy = x: "0 ${x}";
  };

  group = {
    forward = "f";
    backward = "b";
  };

  createDispatch =
    dispatcher: args:
    "${dispatcher}, ${if builtins.isList args then (lib.concatStringsSep "," args) else args}";
  dispatch =
    let
      create = createDispatch;
    in
    {
      window = {
        focus = dir: create "movefocus" dir;
        move = dir: create "movewindow" dir;
        moveWindowOrGroup = dir: create "movewindoworgroup" dir;
        swap = dir: create "swapwindow" dir;
        resize = spec: create "resizeactive" spec;
        toWorkspace =
          workspace: window:
          create "movetoworkspace" (
            lib.lists.filter (x: x != none) [
              workspace
              window
            ]
          );
        toWorkspaceSilent =
          workspace: window:
          create "movetoworkspacesilent" [
            workspace
            window
          ];
        kill = "killactive";
        fullscreen = mode: create "fullscreen" mode;
        toggleFloating = "togglefloating";
      };
      group = {
        toggle = "togglegroup";
        changeActive = dir: create "changegroupactive" dir;
      };
      workspace = {
        select = n: "workspace, ${n}";
        toCurrentMonitor = workspace: create "focusworkspaceoncurrentmonitor" workspace;
      };
      exec = rules: command: create "exec" (if rules == none then command else "[${rules}] ${command}");
      execr = command: create "exec" command;
      submap = map: create "submap" map;
      exit = "exit";
    };
  bind = {
    basic =
      mods: key: dispatcher:
      "bind = ${mods}, ${key}, ${dispatcher}";
    multi =
      mods: key: dispatchers:
      (lib.strings.concatMapStringsSep "\n" (d: (bind.basic mods key d)) dispatchers);
    repeat =
      mods: key: dispatcher:
      "binde = ${mods}, ${key}, ${dispatcher}";
    when =
      cond: mods: key: dispatcher:
      if cond then bind.basic mods key dispatcher else "";
    mouse =
      mods: button: dispatcher:
      "bindm = ${mods}, ${button}, ${dispatcher}";
  };
in
{
  inherit
    bind
    catchall
    createDispatch
    direction
    dispatch
    group
    mod
    none
    resize
    ;
}
