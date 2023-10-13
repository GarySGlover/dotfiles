{lib, ...}:
with lib; {
  options.wolf.languages.nix.enable = mkOption {
    type = types.bool;
    default = false;
  };
}
