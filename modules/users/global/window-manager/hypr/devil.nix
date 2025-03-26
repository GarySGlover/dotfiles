{
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
    widths
    ;
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
  leader = "semicolon";

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

  bindWithResetTimer = delay: mods: key: dispatcher: ''
    ${(bind.basic mods key (dispatch.execr "systemctl --user stop hyprctl-submap.timer"))}
    ${
      (bind.basic mods key (
        dispatch.execr "systemd-run --user --on-active=${toString delay} --timer-property=AccuracySec=1us --unit=hyprctl-submap.service -- hyprctl dispatch submap insert"
      ))
    }
    ${(bind.basic mods key dispatcher)}
  '';

  bindWithReset =
    mods: key: dispatcher:
    "${
      (bind.basic mods key (
        dispatch.execr "systemctl --user stop hyprctl-submap.timer; hyprctl dispatch submap insert; hyprctl dispatch ${
          (
            let
              xs = lib.strings.splitString "," dispatcher;
              command = builtins.elemAt xs 0;
              args = builtins.concatStringsSep "," (lib.lists.drop 1 xs);
            in
            "${command} ${args}"
          )
        }"
      ))
    }";

  resetCatchall =
    "${(bindWithReset none catchall (dispatch.window.pass "activewindow\n"))}"
    + (lib.strings.concatMapStringsSep "\n"
      (m: (bindWithReset m catchall (dispatch.window.pass "activewindow")))
      (
        with mod;
        combinations [
          super
          alt
          ctrl
          shift
        ]
      )
    );

  toSubmap = key: name: (bindWithResetTimer 2 none key (dispatch.submap name));

  submaps = {
    insert = [
      (bindWithResetTimer 2 none leader (dispatch.submap "leader"))
      (bind.mouse mod.shift "mouse:272" "movewindow")
      (bind.mouse mod.shift "mouse:273" "resizewindow")
      (bind.repeat none "XF86AudioMute" (dispatch.execr "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"))
      (bind.repeat none "XF86AudioLowerVolume" (
        dispatch.execr "wpctl set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 5%-"
      ))
      (bind.repeat none "XF86AudioRaiseVolume" (
        dispatch.execr "wpctl set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 5%+"
      ))
    ];
    leader =
      lib.lists.map (w: (bindWithReset none w.key (dispatch.workspace.select w.id))) workSpaces
      ++ [
        (bind.basic none "return" (dispatch.window.sendShortcut none leader "activewindow"))
        (bindWithReset none "return" (dispatch.window.sendShortcut none "return" "activewindow"))
        (bind.basic none "space" (dispatch.window.sendShortcut none leader "activewindow"))
        (bindWithReset none "space" (dispatch.window.sendShortcut none "space" "activewindow"))
        (toSubmap "a" "apps")
        (toSubmap "f" "windowFocus")
        (toSubmap "h" "hereWorkspace")
        (toSubmap "k" "kill")
        (bindWithReset none "l" (dispatch.execr "fuzzel"))
        (toSubmap "m" "media")
        (toSubmap "t" "windowMod")
        (toSubmap "w" "windowMove")
        (toSubmap "g" "insert")
        resetCatchall
      ];
    apps = [
      (bindWithReset none "b" (dispatch.execr "brave"))
      (bindWithReset none "c" (dispatch.execr "chromium"))
      (bindWithReset none "e" (
        dispatch.execr "\"emacsclient --create-frame -no-wait --alternate-editor \'\'\""
      ))
      (bindWithReset none "f" (dispatch.execr "firefox"))
      (bindWithReset none "r" (dispatch.execr "\"hyprshot -z -m region -s --clipboard-only\""))
      (bindWithReset none "p" (dispatch.execr "\"hyprshot -z -m window -s --clipboard-only\""))
      (bindWithReset none "l" (dispatch.execr "hyprlock"))
      (bindWithReset none "s" (dispatch.execr "steam"))
      (bindWithReset none "t" (dispatch.execr "kitty"))
      (bindWithReset none "w" (dispatch.execr "floorp"))
      (toSubmap leader "leader")
      resetCatchall
    ];
    hereWorkspace =
      lib.lists.map (w: (bindWithReset none w.key (dispatch.workspace.toCurrentMonitor w.id))) workSpaces
      ++ [
        (toSubmap leader "leader")
        resetCatchall
      ];
    kill = [
      (bindWithReset none "h" (dispatch.exit))
      (bindWithReset none "w" (dispatch.window.kill))
      (bindWithReset none "s" (dispatch.execr "shutdown now"))
      (toSubmap leader "leader")
      resetCatchall
    ];
    windowFocus = [
      (bindWithResetTimer 2 none "b" (dispatch.scroller.focus direction.left))
      (bindWithResetTimer 2 none "n" (dispatch.scroller.focus direction.down))
      (bindWithResetTimer 2 none "p" (dispatch.scroller.focus direction.up))
      (bindWithResetTimer 2 none "f" (dispatch.scroller.focus direction.right))
      (bindWithResetTimer 2 none "o" (dispatch.scroller.overview))
      (bindWithResetTimer 2 none "j" (dispatch.scroller.jump))
      (toSubmap leader "leader")
      resetCatchall
    ];
    windowMod = [
      (bindWithReset none "f" (dispatch.window.fullscreen "1"))
      (bindWithReset none "m" (dispatch.window.fullscreen "0"))
      (bindWithReset none "v" (dispatch.window.toggleFloating))
      (bindWithResetTimer 2 none "i" (dispatch.scroller.increaseSize))
      (bindWithResetTimer 2 none "d" (dispatch.scroller.decreaseSize))
      (bindWithResetTimer 2 none "p" (dispatch.scroller.pin))
      (bindWithReset none "c" (dispatch.scroller.align "center"))
      (bindWithReset none "l" (dispatch.scroller.align "left"))
      (bindWithReset none "r" (dispatch.scroller.align "right"))
      (toSubmap leader "leader")
      resetCatchall
    ];
    windowMove =
      lib.lists.map (w: (bindWithReset none w.key (dispatch.window.toWorkspace w.id none))) workSpaces
      ++ [
        (bindWithResetTimer 2 none "b" (dispatch.scroller.move direction.left))
        (bindWithResetTimer 2 none "n" (dispatch.scroller.move direction.down))
        (bindWithResetTimer 2 none "p" (dispatch.scroller.move direction.up))
        (bindWithResetTimer 2 none "f" (dispatch.scroller.move direction.right))
        (toSubmap leader "leader")
        resetCatchall
      ];
    media = [
      (bindWithResetTimer 2 none "m" (dispatch.execr "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"))
      (bindWithResetTimer 2 none "p" (dispatch.execr "wpctl set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 5%-"))
      (bindWithResetTimer 2 none "n" (dispatch.execr "wpctl set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 5%+"))
      (bindWithResetTimer 2 none "b" (dispatch.execr "brightnessctl set 10%-"))
      (bindWithResetTimer 2 none "f" (dispatch.execr "brightnessctl set 10%+"))
      (bindWithReset none "t" (dispatch.execr "kitty ${pkgs.bluetuith}/bin/bluetuith"))
      (toSubmap leader "leader")
      resetCatchall
    ];
  };
in
with lib;
{
  inherit defaultMap;
  binds = builtins.concatStringsSep "\n" (
    lib.attrsets.mapAttrsToList (
      name: value: "submap = ${name}\n${builtins.concatStringsSep "\n" (lists.remove "" value)}"
    ) submaps
  );
  inherit workSpaces;
}
