{
  melpaBuild,
  writeText,
  fetchFromGitHub,
  ...
}:
melpaBuild {
  pname = "combobulate";
  ename = "combobulate";
  version = "20240201.0";

  src = fetchFromGitHub {
    owner = "mickeynp";
    repo = "combobulate";
    rev = "f220e87c7bc1792e5fd46efaa86f75a9f5bcc1d0";
    sha256 = "sha256-jR8XlRAig/lgmaVoM3uPp+Odao0JIGG5x3FTHxnmJRo=";
  };

  # Require to prevent file not found error
  commit = "fix";

  recipe = writeText "recipe" ''
    (combobulate :repo "mickeynp/combobulate" :fetcher github)
  '';

  packageRequires = [ ];
}
