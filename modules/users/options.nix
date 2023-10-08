{lib, ...}:
with lib; {
  options = {
    wolf = {
      hostname = mkOption {
        type = types.str;
      };
    };
  };
}
