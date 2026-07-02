# [[file:../modules.org::*Browser][Browser:1]]
{
  flake.aspects.browser.homeManager =
    { pkgs, ... }:
    {
      programs.emacs.extraPackages = epkgs: [
        (epkgs.melpaBuild {
          pname = "browsel";
          version = "1";
          commit = "1";
          src = pkgs.fetchFromGitHub {
            owner = "dmgerman";
            repo = "browsel";
            rev = "6f7e2170d20e471f67035f556098600459dee7fd";
            sha256 = "sha256-XgLjOUCLCBfCq1nHiE9PTObkuUA2CNtOa/PzzwKDibk=";
          };
          packageRequires = with epkgs; [ websocket ];
          recipe = pkgs.writeText "recipe" ''
            (browsel
              :repo "dmgerman/browsel"
              :fetcher github
              :files ("*.el"))
          '';
        })
      ];
    };
  # perSystem =
  # { pkgs, ... }:
  # {
  #   packages.browsel = pkgs.
  # }
}
# Browser:1 ends here
