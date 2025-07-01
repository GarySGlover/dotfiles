{ config, lib, ... }:
let
  hslToRgb =
    {
      h,
      s,
      l,
    }:
    let
      h' = h / 360.0;
      s' = s / 100.0;
      l' = l / 100.0;
      toHex =
        n:
        let
          v = lib.trivial.toHexString (builtins.floor (n * 255.0 + 0.5));
          padded = if builtins.stringLength v == 1 then "0${v}" else v;
        in
        padded;
      hue2rgb =
        a: b: t:
        let
          t' =
            if t < 0.0 then
              t + 1.0
            else if t > 1.0 then
              t - 1.0
            else
              t;
        in
        if t' < 1.0 / 6.0 then
          a + (b - a) * 6.0 * t'
        else if t' < 1.0 / 2.0 then
          b
        else if t' < 2.0 / 3.0 then
          a + (b - a) * (2.0 / 3.0 - t') * 6.0
        else
          a;
      q = if l' < 0.5 then l' * (1.0 + s') else l' + s' - l' * s';
      p = 2.0 * l' - q;
      r = hue2rgb p q (h' + 1.0 / 3.0);
      g = hue2rgb p q h';
      b = hue2rgb p q (h' - 1.0 / 3.0);
    in
    "#${toHex r}${toHex g}${toHex b}";

  hexToInt =
    hex:
    let
      h = builtins.substring 0 1 hex;
      t = builtins.substring 1 1 hex;
      hexDigit =
        d:
        let
          lower = lib.strings.toLower d;
        in
        if lower == "0" then
          0
        else if lower == "1" then
          1
        else if lower == "2" then
          2
        else if lower == "3" then
          3
        else if lower == "4" then
          4
        else if lower == "5" then
          5
        else if lower == "6" then
          6
        else if lower == "7" then
          7
        else if lower == "8" then
          8
        else if lower == "9" then
          9
        else if lower == "a" then
          10
        else if lower == "b" then
          11
        else if lower == "c" then
          12
        else if lower == "d" then
          13
        else if lower == "e" then
          14
        else if lower == "f" then
          15
        else
          abort "invalid hex digit: ${d}";
    in
    (hexDigit h) * 16 + (hexDigit t);

  hexToRgb = hexstr: {
    r = hexToInt (builtins.substring 1 2 hexstr);
    g = hexToInt (builtins.substring 3 2 hexstr);
    b = hexToInt (builtins.substring 5 2 hexstr);
  };

  rgbHexToHsl =
    hexstr:
    let
      rgb = hexToRgb hexstr;
      r' = rgb.r / 255.0;
      g' = rgb.g / 255.0;
      b' = rgb.b / 255.0;

      max = lib.trivial.max r' (lib.trivial.max g' b');
      min = lib.trivial.min r' (lib.trivial.min g' b');
      l' = (max + min) / 2;

      delta = max - min;

      s' =
        if delta == 0 then
          0
        else if l' < 0.5 then
          delta / (max + min)
        else
          delta / (2 - max - min);

      h' =
        if delta == 0 then
          0
        else if max == r' then
          let
            v = ((g' - b') / delta);
          in
          if v < 0 then v + 6 else v
        else if max == g' then
          ((b' - r') / delta) + 2
        else
          ((r' - g') / delta) + 4;

      hDeg = 60 * h';
      hNorm = if hDeg < 0 then hDeg + 360 else hDeg;
    in
    {
      h = hNorm;
      s = s' * 100;
      l = l' * 100;
    };

  colorBases = {
    red = {
      h = 0;
      s = 100;
    };
    orange = {
      h = 30;
      s = 100;
    };
    yellow = {
      h = 60;
      s = 100;
    };
    lime = {
      h = 90;
      s = 100;
    };
    green = {
      h = 120;
      s = 100;
    };
    springGreen = {
      h = 150;
      s = 100;
    };
    cyan = {
      h = 180;
      s = 100;
    };
    azure = {
      h = 210;
      s = 100;
    };
    blue = {
      h = 240;
      s = 100;
    };
    violet = {
      h = 270;
      s = 100;
    };
    magenta = {
      h = 300;
      s = 100;
    };
    rose = {
      h = 330;
      s = 100;
    };
    gray = {
      h = 0;
      s = 0;
    };
    brown = {
      h = 30;
      s = 50;
    };
  };

  background = rgbHexToHsl config.wolf.theme.colors.background;
  mode =
    if background.l <= 20 then
      "dark"
    else if background.l >= 90 then
      "light"
    else
      abort "Background lightness must be <= 20 for dark mode or >= 90 for light mode, got ${background.l}";
  lightnessModifier = if mode == "dark" then 60 else -60;

  mapColorAttrs =
    colors:
    lib.listToAttrs (
      lib.concatMap (
        name:
        let
          base = colorBases.${name};
          mk = extra: hslToRgb (lib.trivial.mergeAttrs base extra);
          bg = {
            l = background.l;
          };
          main = {
            l = background.l + lightnessModifier;
          };
          light = {
            l = background.l + lightnessModifier + 10;
          };
          dark = {
            l = background.l + lightnessModifier - 10;
          };
        in
        [
          {
            name = name;
            value = mk main;
          }
          {
            name = "light${lib.toUpper (lib.substring 0 1 name)}${
              lib.substring 1 (lib.stringLength name - 1) name
            }";
            value = mk light;
          }
          {
            name = "dark${lib.toUpper (lib.substring 0 1 name)}${
              lib.substring 1 (lib.stringLength name - 1) name
            }";
            value = mk dark;
          }
          {
            name = "background${lib.toUpper (lib.substring 0 1 name)}${
              lib.substring 1 (lib.stringLength name - 1) name
            }";
            value = mk bg;
          }
          {
            name = "weak${lib.toUpper (lib.substring 0 1 name)}${
              lib.substring 1 (lib.stringLength name - 1) name
            }";
            value =
              if mode == "dark" then
                config.wolf.theme.colors."dark${lib.toUpper (lib.substring 0 1 name)}${
                  lib.substring 1 (lib.stringLength name - 1) name
                }"
              else
                config.wolf.theme.colors."light${lib.toUpper (lib.substring 0 1 name)}${
                  lib.substring 1 (lib.stringLength name - 1) name
                }";
          }
          {
            name = "strong${lib.toUpper (lib.substring 0 1 name)}${
              lib.substring 1 (lib.stringLength name - 1) name
            }";
            value =
              if mode == "dark" then
                config.wolf.theme.colors."light${lib.toUpper (lib.substring 0 1 name)}${
                  lib.substring 1 (lib.stringLength name - 1) name
                }"
              else
                config.wolf.theme.colors."dark${lib.toUpper (lib.substring 0 1 name)}${
                  lib.substring 1 (lib.stringLength name - 1) name
                }";
          }
        ]
      ) (builtins.attrNames colors)
    );

in
{
  config.wolf.theme.defaultColors = (mapColorAttrs colorBases) // {
    foreground = hslToRgb {
      h = background.h;
      s = background.s;
      l = background.l + lightnessModifier;
    };
    dark = hslToRgb {
      h = background.h;
      s = background.s;
      l = background.l + lightnessModifier - 10;
    };
    light = hslToRgb {
      h = background.h;
      s = background.s;
      l = background.l + lightnessModifier + 10;
    };
    weak = if mode == "dark" then config.wolf.theme.colors.dark else config.wolf.theme.colors.light;
    strong = if mode == "dark" then config.wolf.theme.colors.light else config.wolf.theme.colors.dark;
  };
}
