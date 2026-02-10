inputs: final: prev: {

  mcp-server-azuredevops = prev.buildNpmPackage rec {
    pname = "mcp-server-azure-devops";
    version = "latest";
    src = "${inputs.azure-devops-mcp}";

    npmDeps = prev.importNpmLock {
      npmRoot = src;
    };

    npmConfigHook = prev.importNpmLock.npmConfigHook;

    nativeBuildInputs = [
      prev.typescript
    ];

    dontCheckForBrokenSymlinks = true;

    meta = {
      description = "The MCP server for Azure DevOps, bringing the power of Azure DevOps directly to your agents.";
      homepage = "https://github.com/microsoft/azure-devops-mcp";
      license = prev.lib.licenses.mit;
      mainProgram = "mcp-server-azuredevops";
    };
  };
}
