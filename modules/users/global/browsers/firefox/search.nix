{ pkgs, ... }:
{
  # https://github.com/daluca/nix-config/blob/beefe3d38509f4d4218b395db0594db610122c98/home/firefox/search.nix#L4
  "google".metaData.hidden = true;
  "bing".metaData.hidden = true;
  "Nix Packages" = {
    urls = [
      {
        template = "https://search.nixos.org/packages";
        params = [
          {
            name = "channel";
            value = "unstable";
          }
          {
            name = "type";
            value = "packages";
          }
          {
            name = "query";
            value = "{searchTerms}";
          }
        ];
      }
    ];
    icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
    definedAliases = [ "@np" ];
  };
  "Nix Options" = {
    urls = [
      {
        template = "https://search.nixos.org/options";
        params = [
          {
            name = "channel";
            value = "unstable";
          }
          {
            name = "type";
            value = "packages";
          }
          {
            name = "query";
            value = "{searchTerms}";
          }
        ];
      }
    ];
    icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
    definedAliases = [ "@no" ];
  };
  "NixOS Wiki" = {
    urls = [
      {
        template = "https://nixos.wiki/index.php";
        params = [
          {
            name = "search";
            value = "{searchTerms}";
          }
        ];
      }
    ];
    icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
    definedAliases = [ "@nw" ];
  };
  "Home Manager Options" = {
    urls = [
      {
        template = "https://home-manager-options.extranix.com/";
        params = [
          {
            name = "release";
            value = "master";
          }
          {
            name = "query";
            value = "{searchTerms}";
          }
        ];
      }
    ];
    icon = "${pkgs.fetchurl {
      url = "https://home-manager-options.extranix.com/images/favicon.png";
      hash = "sha256-oFp+eoTLXd0GAK/VrYRUeoXntJDfTu6VnzisEt+bW74";
    }}";
    definedAliases = [ "@hmo" ];
  };
  "Nixpkgs issues" = {
    urls = [
      {
        template = "https://github.com/NixOS/nixpkgs/issues";
        params = [
          {
            name = "q";
            value = "is:issue is:open {searchTerms}";
          }
        ];
      }
    ];
    icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
    definedAliases = [ "@npi" ];
  };
  "GitHub" = {
    urls = [
      {
        template = "https://github.com/search";
        params = [
          {
            name = "type";
            value = "repositories";
          }
          {
            name = "q";
            value = "{searchTerms}";
          }
        ];
      }
    ];
    icon = "${pkgs.fetchurl {
      url = "https://github.com/favicon.ico";
      hash = "sha256-LuQyN9GWEAIQ8Xhue3O1fNFA9gE8Byxw29/9npvGlfg=";
    }}";
    definedAliases = [ "@github" ];
  };
  "Steam" = {
    urls = [
      {
        template = "https://store.steampowered.com/search/";
        params = [
          {
            name = "term";
            value = "{searchTerms}";
          }
        ];
      }
    ];
    icon = "${pkgs.fetchurl {
      url = "https://store.steampowered.com/favicon.ico";
      hash = "sha256-n4kKnevN/MwzkUmnlDvpr/nkySA8L6N9VnGlssiFA60=";
    }}";
    definedAliases = [ "@s" ];
  };
  "ProtonDB" = {
    urls = [
      {
        template = "https://www.protondb.com/search";
        params = [
          {
            name = "q";
            value = "{searchTerms}";
          }
        ];
      }
    ];
    icon = "${pkgs.fetchurl {
      url = "https://www.protondb.com/favicon.ico";
      hash = "sha256-oauOp0EASNjMcThfzYJ2TfbaOYHBPL8LOp+9lmp4pmc=";
    }}";
    definedAliases = [ "@pdb" ];
  };
}
