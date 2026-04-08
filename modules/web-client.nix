# [[file:../modules.org::*Web Requests][Web Requests:2]]
{
  flake.aspects = {
    webRequests.homeManager = {
      programs.emacs.extraPackages = epkgs: with epkgs; [ verb ];
      editor.initFiles.web-requests.text = ''
        (with-eval-after-load 'org
          (org-babel-do-load-languages
           'org-babel-load-languages
           (add-to-list 'org-babel-load-languages '(verb . t))))
      '';
    };
  };
}
# Web Requests:2 ends here
