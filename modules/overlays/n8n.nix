final: prev: {
  n8n = prev.n8n.overrideAttrs (finalAttrs: {
    pname = "n8n";
    version = "1.103.2";
    src = prev.fetchFromGitHub {
      owner = "n8n-io";
      repo = "n8n";
      rev = "n8n@1.103.2";
      sha256 = "sha256-jCIvhQMRHmhaZKIr+zGQ18s1dChUoGE6gsUzknhCvHE=";
    };
    pnpmDeps = final.pnpm_10.fetchDeps {
      inherit (finalAttrs) pname version src;
      fetcherVersion = 1;
      hash = "sha256-LierbGPkVIy5/2vtBl94TQcSpmNX9OUDMntDdo5BeiU=";
    };
    preBuild = "export NODE_OPTIONS=--max-old-space-size=4096";
  });
}
