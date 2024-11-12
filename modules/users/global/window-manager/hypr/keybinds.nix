{
  roles,
  lib,
  pkgs,
  ...
}:
let
  inherit (import ./hypr-binder.nix { inherit lib; })
    bind
    catchall
    direction
    dispatch
    group
    mod
    none
    resize
    ;

  helpers = {
    combinations =
      items:
      let
        combHelper =
          list:
          if list == [ ] then
            [ ]
          else
            let
              hd = builtins.head list;
              rest = combHelper (builtins.tail list);
            in
            [ hd ] ++ map (d: "${hd} ${d}") rest ++ rest;
      in
      (combHelper items);
  };

  # Custom dispatchers
  noOp = dispatch.execr "${pkgs.hyprland}/bin/hyprctl version";
  workSpaceAndSubmap = workspace: map: [
    (dispatch.workspace.select workspace)
    (dispatch.submap map)
  ];
  workSpaceToCurrentAndSubmap = workspace: map: [
    (dispatch.workspace.toCurrentMonitor workspace)
    (dispatch.submap map)
  ];

  # Custom binders
  noOpCatchall =
    "${(bind.basic none catchall noOp)}\n"
    + (lib.strings.concatMapStringsSep "\n" (m: (bind.basic m catchall noOp)) (
      with mod;
      helpers.combinations [
        super
        alt
        ctrl
        shift
      ]
    ));

  defaultBinds = ''
    ${(bind.basic none "i" (dispatch.submap "insert"))}
    ${(bind.basic mod.super "q" (dispatch.submap "command"))}
    ${(bind.basic none "q" (dispatch.submap "command"))}
  '';

  workSpaces =
    lib.lists.imap0
      (i: v: {
        key = toString v;
        id = toString i;
        name = toString v;
      })
      (
        builtins.map (i: "${toString i}") (lib.lists.range 0 9)
        ++ builtins.map (i: "F${toString i}") (lib.lists.range 1 12)
      );

  submaps = {
    command = [
      (bind.basic none "a" (dispatch.submap "apps"))
      (bind.basic none "i" (dispatch.submap "insert"))
      (bind.basic none "f" (dispatch.submap "window"))
      (bind.multi none "l" [
        (dispatch.execr "${pkgs.fuzzel}/bin/fuzzel")
        (dispatch.submap "insert")
      ])
      (bind.basic none "m" (dispatch.submap "media"))
      (bind.basic none "w" (dispatch.submap "workspace"))
      noOpCatchall
    ];

    insert = [ (bind.basic mod.super "q" (dispatch.submap "command")) ];
    apps = [
      (bind.when roles.internet none "b" (dispatch.execr "${pkgs.brave}/bin/brave"))
      (bind.when (roles.internet && roles.work) none "c" (dispatch.execr "${pkgs.chromium}/bin/chromium"))
      (bind.basic none "e" (dispatch.execr "$EDITOR"))
      (bind.when roles.internet none "f" (dispatch.execr "${pkgs.firefox}/bin/firefox"))
      (bind.basic none "p" (
        dispatch.execr "${pkgs.hyprshot}/bin/hyprshot -z -m region -s --clipboard-only"
      ))
      (bind.basic mod.shift "p" (
        dispatch.execr "${pkgs.hyprshot}/bin/hyprshot -z -m window -s --clipboard-only"
      ))
      (bind.when roles.gaming none "s" (dispatch.execr "${pkgs.steam}/bin/steam"))
      (bind.basic none "t" (dispatch.execr "${pkgs.kitty}/bin/kitty"))
      (bind.basic mod.super "x" (dispatch.exit))
      defaultBinds
      noOpCatchall
    ];
    workspace =
      lib.lists.map (w: (bind.multi none w.key (workSpaceAndSubmap w.id "command"))) workSpaces
      ++ lib.lists.map (
        w: (bind.multi mod.shift w.key (workSpaceToCurrentAndSubmap w.id "command"))
      ) workSpaces
      ++ [
        defaultBinds
        noOpCatchall
      ];
    window =
      [
        (bind.basic none "h" (dispatch.window.focus direction.left))
        (bind.basic none "j" (dispatch.window.focus direction.down))
        (bind.basic none "k" (dispatch.window.focus direction.up))
        (bind.basic none "l" (dispatch.window.focus direction.right))
        (bind.basic mod.shift "h" (dispatch.window.moveWindowOrGroup direction.left))
        (bind.basic mod.shift "j" (dispatch.window.moveWindowOrGroup direction.down))
        (bind.basic mod.shift "k" (dispatch.window.moveWindowOrGroup direction.up))
        (bind.basic mod.shift "l" (dispatch.window.moveWindowOrGroup direction.right))
        (bind.basic none "n" (dispatch.group.changeActive group.forward))
        (bind.basic none "p" (dispatch.group.changeActive group.backward))
        (bind.basic mod.shift "x" dispatch.window.kill)
        (bind.basic none "r" (dispatch.submap "resize"))
        (bind.basic none "f" (dispatch.window.fullscreen "1"))
        (bind.basic mod.shift "f" (dispatch.window.fullscreen "0"))
        (bind.basic none "v" (dispatch.window.toggleFloating))
        (bind.basic none "t" (dispatch.group.toggle))
        (bind.mouse none "mouse:272" "movewindow")
        (bind.mouse none "mouse:273" "resizewindow")
      ]
      ++ lib.lists.map (w: (bind.basic none w.key (dispatch.window.toWorkspace w.id none))) workSpaces
      ++ lib.lists.map (
        w: (bind.basic mod.shift w.key (dispatch.window.toWorkspaceSilent w.id none))
      ) workSpaces
      ++ [
        defaultBinds
        noOpCatchall
      ];
    resize =
      [
        (bind.repeat none "h" (dispatch.window.resize resize.left))
        (bind.repeat none "j" (dispatch.window.resize resize.down))
        (bind.repeat none "k" (dispatch.window.resize resize.up))
        (bind.repeat none "l" (dispatch.window.resize resize.right))
        (bind.repeat mod.shift "h" (dispatch.window.resize (resize.leftBy "10%")))
        (bind.repeat mod.shift "j" (dispatch.window.resize (resize.downBy "10%")))
        (bind.repeat mod.shift "k" (dispatch.window.resize (resize.upBy "10%")))
        (bind.repeat mod.shift "l" (dispatch.window.resize (resize.rightBy "10%")))
      ]
      ++ [
        defaultBinds
        noOpCatchall
      ];
    media =
      [
        (bind.repeat none "m" (
          dispatch.execr "${pkgs.wireplumber}/bin/wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"
        ))
        (bind.repeat none "h" (
          dispatch.execr "${pkgs.wireplumber}/bin/wpctl set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 5%-"
        ))
        (bind.repeat none "l" (
          dispatch.execr "${pkgs.wireplumber}/bin/wpctl set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 5%+"
        ))
        (bind.repeat none "j" (dispatch.execr "${pkgs.brightnessctl}/bin/brightnessctl set 10%-"))
        (bind.repeat none "k" (dispatch.execr "${pkgs.brightnessctl}/bin/brightnessctl set 10%+"))
      ]
      ++ [
        defaultBinds
        noOpCatchall
      ];
  };
in
with lib;
{
  binds = builtins.concatStringsSep "\n" (
    lib.attrsets.mapAttrsToList (
      name: value: "submap = ${name}\n${builtins.concatStringsSep "\n" (lists.remove "" value)}"
    ) submaps
  );
  inherit workSpaces;
}
