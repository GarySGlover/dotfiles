{
  services.ollama = {
    enable = true;
    # Optional: preload models, see https://ollama.com/library
    loadModels = [ "deepseek-r1:7b" ];
    acceleration = "rocm";
  };
}
