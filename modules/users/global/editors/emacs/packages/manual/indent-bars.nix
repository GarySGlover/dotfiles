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
  version = "20240601.0.6.2";

  src = fetchFromGitHub {
    owner = "jdtsmith";
    repo = "indent-bars";
    rev = "2216793de857cc21a1faba95361446dacdc8f3f5";
    sha256 = "sha256-I+W3zXk7lza8XTuw/O7sXlXfn8wALtO6kMlj+nF5nfE=";
  };

  # Require to prevent file not found error
  commit = "fix";

  recipe = writeText "recipe" ''
    (indent-bars :repo "jdtsmith/indent-bars" :fetcher github)
  '';

  packageRequires = [compat];
}
