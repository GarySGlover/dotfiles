# [[file:../../modules.org::*Use package][Use package:2]]
{
  flake.aspects.editor.homeManager = {
    editor.initFiles.usePackage.text = ''
      (require 'use-package)
      (setopt
       use-package-always-defer t
       use-package-expand-minimally t)
    '';
  };
}
# Use package:2 ends here
