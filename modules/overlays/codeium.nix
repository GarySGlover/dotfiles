self: super: {
  codeium = super.codeium.overrideAttrs (oldAttrs: {
    version = "1.12.0";
     src = super.fetchurl {
      url = "https://github.com/Exafunction/codeium/releases/download/language-server-v1.12.0/language_server_linux_x64.gz";
       hash = "sha256-CpLhD3nQcnku8Tib1cmVSUJI8/f0FfPNk/eOGlCmHLI=";
     };
  });
}
