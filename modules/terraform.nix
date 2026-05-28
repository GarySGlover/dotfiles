# [[file:../modules.org::*Terraform][Terraform:2]]
{
  flake.aspects.terraform = {
    homeManager =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [
        ];
        programs.emacs.extraPackages =
          epkgs: with epkgs; [
            terraform-mode
          ];
        editor.initFiles.terraform.text = ''
          (with-eval-after-load 'terraform-mode
            (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))
        '';
      };
    nixos = { };
  };
}
# Terraform:2 ends here
