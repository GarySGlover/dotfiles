{
  melpaBuild,
  writeText,
  lib,
  fetchFromGitHub,
  sideline,
  ht,
  ...
}:
melpaBuild rec {
  pname = "sideline-eglot";
  ename = "sideline-eglot";
  version = "20240224.0.1.0";

  src = fetchFromGitHub {
    owner = "emacs-sideline";
    repo = "sideline-eglot";
    rev = "4b1b69df8b673b816cfea6942f66a4bbc6205236";
    sha256 = "sha256-mInznvAjjiLD9Jt/uGdnH9Fv9h5nqAmD8PRnwFPJF0I=";
  };

  # Require to prevent file not found error
  commit = "fix";

  recipe = writeText "recipe" ''
    (sideline-eglot :repo "emacs-sideline/sideline-eglot" :fetcher github)
  '';

  packageRequires = [sideline ht];
}
