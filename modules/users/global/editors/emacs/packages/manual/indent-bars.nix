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
  version = "20240724.0.7.0";

  src = fetchFromGitHub {
    owner = "jdtsmith";
    repo = "indent-bars";
    rev = "c02ded1991f88d7d139b769e439e9244ac4d425a";
    sha256 = "sha256-MOYKMswk0iyJzncgJXm0eOyzE+PmITmhWENJ8V9tzD0=";
  };

  # Require to prevent file not found error
  commit = "fix";

  recipe = writeText "recipe" ''
    (indent-bars :repo "jdtsmith/indent-bars" :fetcher github)
  '';

  packageRequires = [compat];
}
