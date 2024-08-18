{
  melpaBuild,
  writeText,
  fetchFromGitHub,
  compat,
  ...
}:
melpaBuild {
  pname = "org-modern-indent";
  ename = "org-modern-indent";
  version = "20240320.0.1.4";

  src = fetchFromGitHub {
    owner = "jdtsmith";
    repo = "org-modern-indent";
    rev = "f2b859bc53107b2a1027b76dbf4aaebf14c03433";
    sha256 = "sha256-vtbaa3MURnAI1ypLueuSfgAno0l51y3Owb7g+jkK6JU=";
  };

  # Require to prevent file not found error
  commit = "fix";

  recipe = writeText "recipe" ''
    (org-modern-indent :repo "jdtsmith/org-modern-indent" :fetcher github)
  '';

  packageRequires = [compat];
}
