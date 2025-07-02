{ inputs, pkgs, ... }:
let
  agsBundle = pkgs.stdenv.mkDerivation {
    name = "ags-bundle";
    src = ./.;
    installPhase = ''
      mkdir -p $out
      cp app.ts $out/app.ts
      mkdir -p $out/which_key_menu
      cp niri.ts $out/niri.ts
      cp which_key_menu/binds.ts $out/which_key_menu/binds.ts
      cp which_key_menu/menu.ts $out/which_key_menu/menu.ts
    '';
  };
in
{
  imports = [
    inputs.ags.homeManagerModules.default
  ];

  programs.ags = {
    enable = true;
  };

  xdg.configFile = {
    "ags/app.ts".source = "${agsBundle}/app.ts";
  };
}
