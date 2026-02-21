# [[file:../../modules.org::*Nginx][Nginx:1]]
{
  flake.aspects.nginx.homeManager = {
    programs.emacs.extraPackages = epkgs: with epkgs; [ nginx-mode ];
  };
}
# Nginx:1 ends here
