{
  melpaBuild,
  writeText,
  fetchFromGitHub,
  ...
}:
melpaBuild {
  pname = "eglot-booster";
  ename = "eglot-booster";
  version = "20241207.0";

  src = fetchFromGitHub {
    owner = "jdtsmith";
    repo = "eglot-booster";
    rev = "e6daa6bcaf4aceee29c8a5a949b43eb1b89900ed";
    sha256 = "sha256-PLfaXELkdX5NZcSmR1s/kgmU16ODF8bn56nfTh9g6bs=";
  };

  # Require to prevent file not found error
  commit = "fix";

  recipe = writeText "recipe" ''
    (eglot-booster :repo "jdtsmith/eglot-booster" :fetcher github)
  '';

  packageRequires = [ ];
}
