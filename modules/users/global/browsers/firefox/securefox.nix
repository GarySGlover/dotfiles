{
  # TRACKING PROTECTION
  "browser.contentblocking.category" = "strict";
  "urlclassifier.trackingSkipURLs" = "*.reddit.com = *.twitter.com = *.twimg.com = *.tiktok.com";
  "urlclassifier.features.socialtracking.skipURLs" = "*.instagram.com = *.twitter.com = *.twimg.com";
  "browser.download.start_downloads_in_tmp_dir" = true;
  "browser.helperApps.deleteTempFileOnExit" = true;
  "browser.uitour.enabled" = false;
  "privacy.globalprivacycontrol.enabled" = true;

  # OCSP & CERTS / HPKP
  "security.OCSP.enabled" = 0;
  "security.remote_settings.crlite_filters.enabled" = true;
  "security.pki.crlite_mode" = 2;

  # SSL / TLS
  "security.ssl.treat_unsafe_negotiation_as_broken" = true;
  "browser.xul.error_pages.expert_bad_cert" = true;
  "security.tls.enable_0rtt_data" = false;

  # DISK AVOIDANCE
  "browser.privatebrowsing.forceMediaMemoryCache" = true;
  "browser.sessionstore.interval" = 60000;

  # SHUTDOWN & SANITIZING
  "privacy.history.custom" = true;

  # SEARCH / URL BAR
  "browser.urlbar.trimHttps" = true;
  "browser.urlbar.untrimOnUserInteraction.featureGate" = true;
  "browser.search.separatePrivateDefault.ui.enabled" = true;
  "browser.urlbar.update2.engineAliasRefresh" = true;
  "browser.search.suggest.enabled" = false;
  "browser.urlbar.quicksuggest.enabled" = false;
  "browser.urlbar.groupLabels.enabled" = false;
  "browser.formfill.enable" = false;
  "security.insecure_connection_text.enabled" = true;
  "security.insecure_connection_text.pbmode.enabled" = true;
  "network.IDN_show_punycode" = true;

  # HTTPS-FIRST POLICY
  "dom.security.https_first" = true;

  # PASSWORDS
  "signon.formlessCapture.enabled" = false;
  "signon.privateBrowsingCapture.enabled" = false;
  "network.auth.subresource-http-auth-allow" = 1;
  "editor.truncate_user_pastes" = false;

  # MIXED CONTENT + CROSS-SITE
  "security.mixed_content.block_display_content" = true;
  "pdfjs.enableScripting" = false;

  # EXTENSIONS
  "extensions.enabledScopes" = 5;

  # HEADERS / REFERERS
  "network.http.referer.XOriginTrimmingPolicy" = 2;

  # CONTAINERS
  "privacy.userContext.ui.enabled" = true;

  # SAFE BROWSING
  "browser.safebrowsing.downloads.remote.enabled" = false;

  # MOZILLA
  "permissions.default.desktop-notification" = 2;
  "permissions.default.geo" = 2;
  "permissions.manager.defaultsUrl" = "";
  "webchannel.allowObject.urlWhitelist" = "";

  # TELEMETRY
  "datareporting.policy.dataSubmissionEnabled" = false;
  "datareporting.healthreport.uploadEnabled" = false;
  "toolkit.telemetry.unified" = false;
  "toolkit.telemetry.enabled" = false;
  "toolkit.telemetry.server" = "data: =";
  "toolkit.telemetry.archive.enabled" = false;
  "toolkit.telemetry.newProfilePing.enabled" = false;
  "toolkit.telemetry.shutdownPingSender.enabled" = false;
  "toolkit.telemetry.updatePing.enabled" = false;
  "toolkit.telemetry.bhrPing.enabled" = false;
  "toolkit.telemetry.firstShutdownPing.enabled" = false;
  "toolkit.telemetry.coverage.opt-out" = true;
  "toolkit.coverage.opt-out" = true;
  "toolkit.coverage.endpoint.base" = "";
  "browser.newtabpage.activity-stream.feeds.telemetry" = false;
  "browser.newtabpage.activity-stream.telemetry" = false;

  # EXPERIMENTS
  "app.shield.optoutstudies.enabled" = false;
  "app.normandy.enabled" = false;
  "app.normandy.api_url" = "";

  # CRASH REPORTS
  "breakpad.reportURL" = "";
  "browser.tabs.crashReporting.sendReport" = false;
  "browser.crashReports.unsubmittedCheck.autoSubmit2" = false;

  # DETECTION
  "captivedetect.canonicalURL" = "";
  "network.captive-portal-service.enabled" = false;
  "network.connectivity-service.enabled" = false;
}
