{lib, ...}:
with lib; {
  options = {
    wolf = {
      host = mkOption {
        type = types.str;
      };
    };
  };
}
