# [[file:../modules.org::*Terraform][Terraform:3]]
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
        programs.emacs.extraPackages =
          epkgs: with epkgs; [
            terraform-mode
          ];
        editor.initFiles.terraform.text = ''
          (with-eval-after-load 'terraform-mode
            (setenv "TF_CLI_CONFIG_FILE" (file-name-concat "${config.xdg.configHome}" "terraform/.terraformrc"))
            (add-hook 'terraform-mode-hook #'eglot-ensure)
            (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))
        '';
        niri.configFiles.terraform.text = ''
          environment {
            TF_CLI_CONFIG_FILE "${config.xdg.configHome}/terraform/.terraformrc"
          }
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
# Terraform:3 ends here
