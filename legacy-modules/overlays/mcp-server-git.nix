inputs: _: prev: {
  mcp-server-git = prev.python3Packages.buildPythonApplication {
    pname = "mcp-server-git";
    version = "latest";
    src = "${inputs.modelcontextprotocol-servers}/src/git";
    format = "pyproject";
    build-system = [ prev.python3Packages.hatchling ];
    propagatedBuildInputs = with prev.python3Packages; [
      click
      mcp
      gitpython
      pydantic
    ];
    pythonImportsCheck = [ "mcp_server_git" ];
  };
}
