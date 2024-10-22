{ config, ... }:
let
  opt = config.wolf;
  host_role = {
    auberon = [
      "cad"
      "desktop"
      "devops"
      "editing"
      "gaming"
      "internet"
      "programming"
      "work"
    ];
    belisarius = [
      "cad"
      "desktop"
      "devops"
      "editing"
      "gaming"
      "internet"
      "programming"
      "work"
    ];
    clover-z270pd3 = [
      "desktop"
      "devops"
      "editing"
      "programming"
      "work"
    ];
  };
  host_languages = {
    auberon = [
      "go"
      "json"
      "lisp"
      "nim"
      "nix"
      "powershell"
      "python"
      "zig"
    ];
    belisarius = [
      "json"
    ];
    clover-z270pd3 = [
      "json"
    ];
  };
in
{
  wolf = {
    roles = {
      cad = builtins.elem "cad" host_role."${opt.host}";
      desktop = builtins.elem "desktop" host_role."${opt.host}";
      devops = builtins.elem "devops" host_role."${opt.host}";
      editing = opt.roles.programming || builtins.elem "editing" host_role."${opt.host}";
      gaming = builtins.elem "gaming" host_role."${opt.host}";
      internet = builtins.elem "internet" host_role."${opt.host}";
      programming = builtins.elem "programming" host_role."${opt.host}";
      work = builtins.elem "work" host_role."${opt.host}";
    };

    languages = {
      go = builtins.elem "go" host_languages."${opt.host}";
      json = builtins.elem "json" host_languages."${opt.host}";
      lisp = builtins.elem "lisp" host_languages."${opt.host}";
      nim = builtins.elem "nim" host_languages."${opt.host}";
      nix = opt.roles.editing || builtins.elem "nix" host_languages."${opt.host}";
      powershell = builtins.elem "powershell" host_languages."${opt.host}";
      python = builtins.elem "python" host_languages."${opt.host}";
      zig = builtins.elem "zig" host_languages."${opt.host}";
    };
    user.interactive = true;
  };
}
