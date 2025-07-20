{
  config,
  lib,
  wolfLib,
  ...
}:
let
  colorBases = [
    "red"
    "orange"
    "yellow"
    "lime"
    "green"
    "springGreen"
    "cyan"
    "azure"
    "blue"
    "violet"
    "magenta"
    "rose"
    "gray"
    "brown"
  ];
  mkColorOption =
    base: kind:
    lib.mkOption {
      type = lib.types.str;
      default =
        config.wolf.theme.defaultColors."${
          if kind == "" then base else "${kind}${wolfLib.capitalize base}"
        }";
    };
in
with lib;
with types;
{
  options.wolf = {
    host = mkOption { type = str; };
    secretsPath = mkOption { type = path; };
    user.interactive = mkOption { type = bool; };
  };

  options.wolf.theme = {
    font = mkOption { type = hm.types.fontType; };
    border = mkOption { type = ints.unsigned; };
    gaps = mkOption { type = ints.unsigned; };
    radius = mkOption {
      type = ints.unsigned;
      default = config.wolf.theme.font.size;
    };
    defaultColors = mkOption {
      type = types.attrs;
    };
    colors_2 = {
      background = mkOption { type = str; };
      bg = mkOption { type = str; };
      bg_alt = mkOption { type = str; };
      bg_dim = mkOption { type = str; };
      blue = mkOption { type = str; };
      border_active = mkOption { type = str; };
      border_inactive = mkOption { type = str; };
      brown = mkOption { type = str; };
      cyan = mkOption { type = str; };
      fg = mkOption { type = str; };
      fg_alt = mkOption { type = str; };
      fg_bright = mkOption { type = str; };
      fg_dim = mkOption { type = str; };
      fg_max = mkOption { type = str; };
      foreground = mkOption { type = str; };
      green = mkOption { type = str; };
      orange = mkOption { type = str; };
      purple = mkOption { type = str; };
      red = mkOption { type = str; };
      selection_bg = mkOption { type = str; };
      selection_fg = mkOption { type = str; };
      yellow = mkOption { type = str; };
    };

    colors =
      {
        background = mkOption { type = str; };
        foreground = mkOption {
          type = str;
          default = config.wolf.theme.defaultColors.foreground;
        };
        light = mkOption {
          type = str;
          default = config.wolf.theme.defaultColors.light;
        };
        dark = mkOption {
          type = str;
          default = config.wolf.theme.defaultColors.dark;
        };
        weak = mkOption {
          type = str;
          default = config.wolf.theme.defaultColors.weak;
        };
        strong = mkOption {
          type = str;
          default = config.wolf.theme.defaultColors.strong;
        };
      }
      // builtins.listToAttrs (
        builtins.concatMap (
          base:
          let
            baseCapitalized = wolfLib.capitalize base;
          in
          [
            {
              name = base;
              value = mkColorOption base "";
            }
            {
              name = "light${baseCapitalized}";
              value = mkColorOption base "light";
            }
            {
              name = "dark${baseCapitalized}";
              value = mkColorOption base "dark";
            }
            {
              name = "weak${baseCapitalized}";
              value = mkColorOption base "weak";
            }
            {
              name = "strong${baseCapitalized}";
              value = mkColorOption base "strong";
            }
            {
              name = "background${baseCapitalized}";
              value = mkColorOption base "background";
            }
          ]
        ) colorBases
      );
    name = mkOption { type = str; };
    type = mkOption {
      type = enum [
        "dark"
        "light"
      ];
      default = "dark";
    };
  };

  options.wolf.languages = {
    go = mkOption { type = bool; };
    json = mkOption { type = bool; };
    lisp = mkOption { type = bool; };
    nim = mkOption { type = bool; };
    nix = mkOption { type = bool; };
    powershell = mkOption { type = bool; };
    python = mkOption { type = bool; };
    zig = mkOption { type = bool; };
  };

  options.wolf.roles = {
    cad = mkOption { type = bool; };
    desktop = mkOption { type = bool; };
    devops = mkOption { type = bool; };
    editing = mkOption { type = bool; };
    gaming = mkOption { type = bool; };
    internet = mkOption { type = bool; };
    programming = mkOption { type = bool; };
    work = mkOption { type = bool; };
    wayland = mkOption { type = bool; };
  };
}
