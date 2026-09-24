# [[file:../modules.org::*JSON][JSON:1]]
{
  flake.aspects.yaml = {
    homeManager =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [ ];
        programs.emacs.extraPackages =
          epkgs: with epkgs; [
            json-snatcher
          ];
        editor.initFiles.json.text = ''
          (defun json-parse-whole-buffer (buffer)
            "Parse JSON from start of the buffer."
            (with-current-buffer buffer
              (goto-char (point-min))
              (json-parse-buffer
               :object-type 'plist
               :array-type 'list)))
        '';
      };
  };
}
# JSON:1 ends here
