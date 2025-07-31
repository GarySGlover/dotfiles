{ pkgs, ... }:
{
  home.packages = with pkgs; [
    n8n
  ];

  home.file.".local/bin/n8n" = {
    text = ''
      #!/usr/bin/env bash
      export N8N_DIAGNOSTICS_ENABLED=false
      export N8N_VERSION_NOTIFICATIONS_ENABLED=false
      export N8N_TEMPLATES_ENABLED=false
      exec ${pkgs.n8n}/bin/n8n
    '';
    executable = true;
  };
}
