# [[file:../modules.org::*Project][Project:1]]
{
  flake.aspects = {
    programming = {
      homeManager = {
        programs.git.signing.format = null;
      };
      nixos = { };
    };
  };
}
# Project:1 ends here
