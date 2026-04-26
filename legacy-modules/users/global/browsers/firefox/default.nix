{
  config,
  pkgs,
  ...
}:
let
  secrets = import ../../../../../secrets/${config.home.username}-secrets.nix;
  sharedSettings =
    import ./fastfox.nix
    // import ./securefox.nix
    // import ./peskyfox.nix
    // {
      "browser.startup.homepage" = "about:blank";

      # Security / Privacy
      "privacy.donottrackheader.enabled" = true;

      # Fonts
      "browser.display.use_document_fonts" = 1;
      "font.default.x-western" = "sans-serif";

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
      "browser.download.dir" = "/home/clover/Downloads";
      "browser.download.useDownloadDir" = false;
      "browser.download.folderList" = 2;

      # No resume previous session
      "browser.sessionstore.max_resumed_crashes" = 0;

      # DRM Play Content
      "media.eme.enabled" = true;

      # Extensions
      "extensions.autoDisableScopes" = 0;
    };
in
{
  config = {
    programs.firefox = {
      enable = true;
      package = pkgs.firefox-bin;
      languagePacks = [ "en-GB" ];
      configPath = "${config.xdg.configHome}/mozilla/firefox";
      profiles = {
        home = {
          id = 0;
          name = "home";
          isDefault = true;
          settings = sharedSettings // {
            "permissions.default.desktop-notification" = 2;
          };
          search = {
            force = true;
            default = "ddg";
            privateDefault = "ddg";
            engines = import ./search.nix { inherit pkgs; };
          };
        };
        work = {
          id = 1;
          name = "work";
          isDefault = false;
          settings = sharedSettings // {
            "permissions.default.desktop-notification" = 0;
          };
          search = {
            force = true;
            default = "ddg";
            privateDefault = "ddg";
            engines = import ./search.nix { inherit pkgs; } // secrets.work_firefox_search_engines;
          };
        };
      };
    };
  };
}
