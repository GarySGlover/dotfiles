{ lib, ... }:
let
  inherit (import ./hypr-binder.nix { inherit lib; })
    createDispatch
    ;

  dispatch =
    let
      create = createDispatch;
    in
    {
      window = {
        focus = dir: create "hy3:movefocus" dir;
        move = dir: create "hy3:movewindow" dir;
      };
      group = {
        make =
          type:
          create "hy3:makegroup" [
            type
            "force_ephemeral"
          ];
        change = change: create "hy3:changegroup" change;
        kill = "hy3:killactive";
        toWorkspace =
          w:
          create "hy3:movetoworkspace" [
            w
            "follow"
          ];
        toWorkspaceSilent = w: create "hy3:movetoworkspace" w;
        focus = focus: create "hy3:changefocus" focus;
        focusTab = dir: create "hy3:focustab" dir;
      };
    };
  group = {
    type = {
      horizontal = "h";
      vertical = "v";
      opposite = "opposite";
      tab = "tab";
    };
    change = {
      horizontal = "h";
      vertical = "v";
      opposite = "opposite";
      tab = "tab";
      untab = "untab";
      toggletab = "toggletab";
    };
    focus = {
      top = "top";
      bottom = "bottom";
      raise = "raise";
      lower = "lower";
      tab = "tab";
      tabnode = "tabnode";
    };
  };
in
{
  inherit dispatch;
  inherit group;
}
