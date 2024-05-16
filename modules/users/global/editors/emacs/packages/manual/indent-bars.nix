{
  melpaBuild,
  writeText,
  fetchFromGitHub,
  compat,
  ...
}:
melpaBuild {
  pname = "indent-bars";
  ename = "indent-bars";
  version = "20240228.0.2.3";

  src = fetchFromGitHub {
    owner = "jdtsmith";
    repo = "indent-bars";
    rev = "fb1a0d6d1ae0131898dde9722ad44da2bbebc66f";
    sha256 = "sha256-tkjy7uNYpnI9IZwizqbJDIS3Yf6U7KhWfoyXbBoCfnk=";
  };

  # Require to prevent file not found error
  commit = "fix";

  recipe = writeText "recipe" ''
    (indent-bars :repo "jdtsmith/indent-bars" :fetcher github)
  '';

  packageRequires = [compat];
}
