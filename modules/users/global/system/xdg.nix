{
  config,
  pkgs,
  lib,
  ...
}: {
  config = {
    xdg.desktopEntries.emacs-dired = {
      name = "emacs-dired";
      exec = "emacsclient -n -u %F";
      type = "Application";
      mimeType = ["application/x-directory" "inode/directory"];
    };
  };
}
