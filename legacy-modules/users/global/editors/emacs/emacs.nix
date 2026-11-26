{ ... }:
{
  config = {
    programs.git.ignores = [
      "*~"
      ".#*"
      "*.elc"
      "*.tmp"
      "*#"
      ".aider*"
    ];
  };
}
