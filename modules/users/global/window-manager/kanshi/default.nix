{
  config,
  lib,
  ...
}:
let
  inherit (lib) mkIf;
  laptopMonitor = "BOE NE160WUM-NX2 Unknown";
  officeDellUpper = "Dell Inc. DELL P2421 803C773";
  officeDellLower = "Dell Inc. DELL P2421 5Z2C773";
  officePhilips = "Philips Consumer Electronics Company PHL BDM3275 0x00000673";
  bedroomSony = "Sony SONY TV  *00 0x01010101";
  projector = "Optoma Corporation OPTOMA 1080P Q7EH9350054";
in
{
  config = mkIf config.wolf.roles.desktop {
    services.kanshi = {
      enable = true;
      settings = [
        {
          output = {
            criteria = laptopMonitor;
            scale = 1.0;
            adaptiveSync = true;
            mode = "1920x1200@165Hz";
          };
        }
        {
          output = {
            criteria = officeDellUpper;
            scale = 1.0;
            mode = "1920x1200@59.950Hz";

          };
        }
        {
          output = {
            criteria = officeDellLower;
            scale = 1.0;
            mode = "1920x1200@59.950Hz";

          };
        }
        {
          output = {
            criteria = officePhilips;
            scale = 1.5;
            mode = "3840x2160@59.997Hz";
          };
        }
        {
          output = {
            criteria = projector;
            scale = 1.0;
            mode = "1920x1080@60";
          };
        }
        {
          profile = {
            name = "laptop";
            outputs = [ { criteria = laptopMonitor; } ];
          };
        }
        {
          profile = {
            name = "office";
            outputs = [
              {
                criteria = laptopMonitor;
                status = "disable";
              }
              {
                criteria = officeDellUpper;
                position = "0,0";
              }
              {
                criteria = officeDellLower;
                position = "0,1200";
              }
              {
                criteria = officePhilips;
                position = "1920,560";
              }
            ];
          };
        }
        {
          profile = {
            name = "bedroom";
            outputs = [
              {
                criteria = laptopMonitor;
                status = "disable";
              }
              {
                criteria = bedroomSony;
              }
            ];
          };
        }
        {
          profile = {
            name = "projector";
            outputs = [
              {
                criteria = laptopMonitor;
                status = "disable";
              }
              {
                criteria = projector;
              }
            ];
          };
        }
      ];
    };
  };
}
