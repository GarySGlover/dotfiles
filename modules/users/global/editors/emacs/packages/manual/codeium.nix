{
  trivialBuild,
  fetchFromGitHub,
  ...
}:
trivialBuild {
  pname = "codeium.el";
  version = "202408.1.12.0";
  src = fetchFromGitHub {
    owner = "Exafunction";
    repo = "codeium.el";
    rev = "08d5ecfa74d960cf18af46c2d7fa0449d789d73b";
    sha256 = "sha256-NnCpoGMJKBsPa7KtavEg/4+tdqbrCCemvYYT1p6BcdY=";
  };
}
