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
    owner = "niontrix";
    repo = "nim-ts-mode";
    rev = "d6470984167b1f0b1693aecf4d8360ee4bba8bb7";
    sha256 = "sha256-gfZfayfvqz41MbuQSstdaLPHy9CVI6OpQKTEMpBgU4E=";
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
