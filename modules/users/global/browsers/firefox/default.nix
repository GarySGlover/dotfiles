{
  config,
  lib,
  pkgs,
  ...
}:
let
  theme = config.wolf.theme;
  secrets = import "${config.wolf.secretsPath}/${config.home.username}-secrets.nix";
  sharedSettings =
    import ./fastfox.nix
    // import ./securefox.nix
    // import ./peskyfox.nix
    // {
      "browser.startup.homepage" = "about:blank";
      "dom.event.contextmenu.enabled" = false; # Prevent sites disabling right-click
      "layout.css.prefers-color-scheme.content-override" = 0; # Dark mode

      # Security / Privacy
      "privacy.donottrackheader.enabled" = true;

      # Fonts
      "browser.display.use_document_fonts" = 1;
      "font.default.x-western" = "sans-serif";
      "font.name.monospace.x-western" = theme.font.name;
      "font.name.sans-serif.x-western" = theme.font.name;
      "font.name.serif.x-western" = theme.font.name;
      "font.size.variable.x-western" = toString theme.font.size;

      # Form Fill
      "extensions.formautofill.addresses.enabled" = false;
      "extensions.formautofill.creditCards.enabled" = false;
      "signon.autofillForms" = false;
      "signon.firefoxRelay.feature" = "disabled";
      "signon.generation.enabled" = false;
      "signon.rememberSignons" = false;

      # Locale
      "intl.locale.requested" = "en-GB,en-US";

      # Downloads
      "browser.download.dir" = "/home/clover/tmp/personal";
      "browser.download.folderList" = 2;

      # DRM Play Content
      "media.eme.enabled" = true;

      # Extensions
      "extensions.autoDisableScopes" = 0;

      # Notifications
      "permissions.default.desktop-notification" = 1;
    };
in
{
  config = lib.mkIf config.wolf.roles.internet {
    programs.firefox = {
      enable = true;
      languagePacks = [ "en-GB" ];
      profiles.default = {
        settings = sharedSettings // {
          # Downloads
          "browser.download.dir" = "/home/clover/tmp/personal";
        };
        search = {
          force = true;
          default = "DuckDuckGo";
          privateDefault = "DuckDuckGo";
          engines = import ./search.nix { inherit pkgs; };
        };
        extensions = with pkgs.nur.repos.rycee.firefox-addons; [
          bitwarden
          enhancer-for-youtube
          ublock-origin
        ];
      };
    };
    programs.floorp = {
      enable = true;
      languagePacks = [ "en-GB" ];
      profiles.default = {
        settings = sharedSettings // {
          # Downloads
          "browser.download.dir" = "/home/clover/tmp/work";
        };
        search = {
          force = true;
          default = "DuckDuckGo";
          privateDefault = "DuckDuckGo";
          engines = import ./search.nix { inherit pkgs; } // secrets.work_firefox_search_engines;
        };
        extensions = with pkgs.nur.repos.rycee.firefox-addons; [
          bitwarden
          ublock-origin
        ];
      };
    };
  };
}
