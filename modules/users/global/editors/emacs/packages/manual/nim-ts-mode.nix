{
  melpaBuild,
  writeText,
  lib,
  fetchFromGitHub,
  nim-mode,
  ...
}:
melpaBuild rec {
  pname = "nim-ts-mode";
  ename = "nim-ts-mode";
  version = "20240210.0";

  src = fetchFromGitHub {
    owner = "GarySGlover";
    repo = "nim-ts-mode";
    rev = "32adc17e47fb2faf603248bea42df6fb14639c88";
    sha256 = "sha256-63Z/iNkBH8yBjkETXERU4sFweru02PxZNR0C3WhlVDc=";
  };

  # Require to prevent file not found error
  commit = "fix";

  recipe = writeText "recipe" ''
    (nim-ts-mode :repo "niontrix/nim-ts-mode" :fetcher github)
  '';

  propagatedUserEnvPkgs = [
    nim-mode
  ];
  buildInputs = propagatedUserEnvPkgs;
}
