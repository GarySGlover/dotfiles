{
  melpaBuild,
  writeText,
  lib,
  fetchFromGitHub,
  compat,
  ...
}:
melpaBuild rec {
  pname = "indent-bars";
  ename = "indent-bars";
  version = "0.2.3";

  src = fetchFromGitHub {
    owner = "GarySGlover";
    repo = "indent-bars";
    rev = "230e3c8377fa6e31de9973aaa2a8d3f9c299a3ac";
    sha256 = "sha256-yi1u1+PZT4RgoSAyoTN2ktIcIjlTEgrGiYRAMDlB8QI=";
  };

  # Require to prevent file not found error
  commit = "fix";

  recipe = writeText "recipe" ''
    (indent-bars :repo "GarySGlover/indent-bars" :fetcher github)
  '';

  packageRequires = [compat];
}
