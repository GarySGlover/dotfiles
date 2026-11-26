# [[file:../modules.org::*Terraform][Terraform:1]]
{
  flake.aspects.terraform = {
    homeManager =
      {
        pkgs,
        config,
        lib,
        ...
      }:
      {
        home.packages = with pkgs; [
          terraform-ls
        ];
        niri.configFiles.terraform.text = ''

        '';
        xdg.configFile."terraform/.terraformrc".text = ''
          plugin_cache_dir = "${config.home.homeDirectory}/.terraform.d/plugin-cache"
        '';
        home.activation.createTerraformPluginCacheDir = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          mkdir -p "${config.home.homeDirectory}/.terraform.d/plugin-cache"
        '';
      };
    nixos = { };
  };
}
# Terraform:1 ends here
