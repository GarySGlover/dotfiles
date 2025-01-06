{
  melpaBuild,
  writeText,
  fetchFromGitHub,
  ...
}:
melpaBuild {
  pname = "kbd-mode";
  ename = "kbd-mode";
  version = "20241207.0";

  src = fetchFromGitHub {
    owner = "kmonad";
    repo = "kbd-mode";
    rev = "b07f3e16043fbb268b3e47fb49abc419e8b7e2d7";
    sha256 = "sha256-QZiX6do8rV5keNFx8ToZkENuGbYHR/W3p3jb8MLJ1lI=";
  };

  # Require to prevent file not found error
  commit = "fix";

  recipe = writeText "recipe" ''
    (kbd-mode :repo "kmonad/kbd-mode" :fetcher github)
  '';

  packageRequires = [ ];
}
