{
  trivialBuild,
  fetchFromGitHub,
  ...
}:
trivialBuild rec {
  pname = "codeium.el";
  version = "202403.1.8.10";
  src = fetchFromGitHub {
    owner = "Exafunction";
    repo = "codeium.el";
    rev = "e8bab83f95626541afa5ea2298aa80ccc7567d53";
    sha256 = "sha256-9Vxd9UtVOY2t+br+O8aBampp0x18OznHoo4CljHTYf0=";
  };
}
