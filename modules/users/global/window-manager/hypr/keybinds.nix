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
    mod
    none
    resize
    ;

  hy3 = (import ./hy3.nix { inherit lib; });

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
      (bind.basic none "g" (dispatch.submap "group"))
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
      (bind.when roles.gaming none "s" (dispatch.execr "${pkgs.steam}/bin/steam"))
      (bind.basic none "t" (dispatch.execr "${pkgs.kitty}/bin/kitty"))
      (bind.basic mod.super "x" (dispatch.exit))
      defaultBinds
      noOpCatchall
    ];
    workspace =
      lib.lists.map (w: (bind.multi none w.key (workSpaceAndSubmap w.id "command"))) workSpaces
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
        (bind.basic mod.shift "h" (hy3.dispatch.window.move direction.left))
        (bind.basic mod.shift "j" (hy3.dispatch.window.move direction.down))
        (bind.basic mod.shift "k" (hy3.dispatch.window.move direction.up))
        (bind.basic mod.shift "l" (hy3.dispatch.window.move direction.right))
        (bind.basic mod.shift "x" dispatch.window.kill)
        (bind.basic none "r" (dispatch.submap "resize"))
        (bind.basic none "f" (dispatch.window.fullscreen "1"))
        (bind.basic mod.shift "f" (dispatch.window.fullscreen "0"))
      ]
      ++ lib.lists.map (w: (bind.basic none w.key (dispatch.window.toWorkspace w.id none))) workSpaces
      ++ lib.lists.map (
        w: (bind.basic mod.shift w.key (dispatch.window.toWorkspaceSilent w.id none))
      ) workSpaces
      ++ [
        defaultBinds
        noOpCatchall
      ];
    group =
      [
        (bind.basic none "b" (hy3.dispatch.group.make hy3.group.type.horizontal))
        (bind.basic none "v" (hy3.dispatch.group.make hy3.group.type.vertical))
        (bind.basic none "t" (hy3.dispatch.group.make hy3.group.type.tab))
        (bind.basic none "o" (hy3.dispatch.group.make hy3.group.type.opposite))
        (bind.basic mod.shift "b" (hy3.dispatch.group.change hy3.group.change.horizontal))
        (bind.basic mod.shift "v" (hy3.dispatch.group.change hy3.group.change.vertical))
        (bind.basic mod.shift "t" (hy3.dispatch.group.change hy3.group.change.toggletab))
        (bind.basic mod.shift "u" (hy3.dispatch.group.change hy3.group.change.untab))
        (bind.basic mod.shift "o" (hy3.dispatch.group.change hy3.group.change.opposite))
        (bind.basic mod.shift "x" hy3.dispatch.group.kill)
        (bind.basic none "h" (hy3.dispatch.group.focusTab direction.left))
        (bind.basic none "j" (hy3.dispatch.group.focusTab direction.down))
        (bind.basic none "k" (hy3.dispatch.group.focusTab direction.up))
        (bind.basic none "l" (hy3.dispatch.group.focusTab direction.right))
        (bind.basic mod.shift "h" (hy3.dispatch.window.move direction.left))
        (bind.basic mod.shift "j" (hy3.dispatch.window.move direction.down))
        (bind.basic mod.shift "k" (hy3.dispatch.window.move direction.up))
        (bind.basic mod.shift "l" (hy3.dispatch.window.move direction.right))
        (bind.basic none "p" (hy3.dispatch.group.focus hy3.group.focus.top))
        (bind.basic none "m" (hy3.dispatch.group.focus hy3.group.focus.bottom))
        (bind.basic none "r" (hy3.dispatch.group.focus hy3.group.focus.raise))
        (bind.basic none "w" (hy3.dispatch.group.focus hy3.group.focus.lower))
        (bind.basic none "a" (hy3.dispatch.group.focus hy3.group.focus.tab))
        (bind.basic none "n" (hy3.dispatch.group.focus hy3.group.focus.tabnode))
      ]
      ++ lib.lists.map (w: (bind.basic none w.key (hy3.dispatch.group.toWorkspace w.id))) workSpaces
      ++ lib.lists.map (
        w: (bind.basic mod.shift w.key (hy3.dispatch.group.toWorkspaceSilent w.id))
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
