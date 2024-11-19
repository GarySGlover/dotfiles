{
  # GENERAL
  "content.notify.interval" = 100000;

  # GFX
  "gfx.canvas.accelerated.cache-items" = 4096;
  "gfx.canvas.accelerated.cache-size" = 512;
  "gfx.content.skia-font-cache-size" = 20;

  # DISK CACHE
  "browser.cache.jsbc_compression_level" = 3;

  # MEDIA CACHE
  "media.memory_cache_max_size" = 65536;
  "media.cache_readahead_limit" = 7200;
  "media.cache_resume_threshold" = 3600;

  # IMAGE CACHE
  "image.mem.decode_bytes_at_a_time" = 32768;

  # NETWORK
  "network.http.max-connections" = 1800;
  "network.http.max-persistent-connections-per-server" = 10;
  "network.http.max-urgent-start-excessive-connections-per-host" = 5;
  "network.http.pacing.requests.enabled" = false;
  "network.dnsCacheExpiration" = 3600;
  "network.ssl_tokens_cache_capacity" = 10240;

  # SPECULATIVE LOADING
  "network.dns.disablePrefetch" = true;
  "network.dns.disablePrefetchFromHTTPS" = true;
  "network.prefetch-next" = false;
  "network.predictor.enabled" = false;
  "network.predictor.enable-prefetch" = false;

  # EXPERIMENTAL
  "layout.css.grid-template-masonry-value.enabled" = true;
  "dom.enable_web_task_scheduling" = true;
}
