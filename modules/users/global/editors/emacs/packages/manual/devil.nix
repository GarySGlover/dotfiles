{
  melpaBuild,
  writeText,
  fetchFromGitHub,
  ...
}:
melpaBuild {
  pname = "devil";
  ename = "devil";
  version = "20241124.0";

  src = fetchFromGitHub {
    owner = "GarySGlover";
    repo = "devil";
    rev = "174b52526b21de631ff28fadb1343841e2a46204";
    sha256 = "sha256-2I+yiZsOpiZLA4SAW+A97tmzzP2xNu5IauTyc/4euJo=";
  };

  # Require to prevent file not found error
  commit = "fix";

  recipe = writeText "recipe" ''
    (devil :repo "GarySGlover/devil" :fetcher github)
  '';

  packageRequires = [ ];
}
