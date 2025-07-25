// user.js has precedence over about:config or settings panel customizations.
// (they will be reset to as in user.js when closing browser)
// But you can "install" settings by placing user.js, launch browser, then remove user.js.
// (Settings will be saved in prefs.js)

user_pref("accessibility.blockautorefresh", true); // note: uMatrix "Spoof <noscript> tags" must be DISABLED to block <meta http-equiv="refresh"> inside <noscript>
user_pref("browser.aboutConfig.showWarning", false);
user_pref("browser.compactmode.show", true);
user_pref("browser.display.use_document_fonts", 1); // remote font: 1 = enable (you can "Block remote fonts" in uBO config page and only allow on certain sites via its config panel)
user_pref("browser.download.autohideButton", true);
user_pref("browser.download.alwaysOpenPanel", false);
user_pref("browser.shell.checkDefaultBrowser", false);
user_pref("browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons", false);
user_pref("browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features", false);
user_pref("browser.newtabpage.activity-stream.showSponsoredTopSites", false);
user_pref("browser.tabs.closeWindowWithLastTab", false);
user_pref("browser.tabs.warnOnClose", false);
user_pref("browser.theme.dark-private-windows", false);
user_pref("browser.urlbar.suggest.trending", false);
user_pref("browser.privatebrowsing.enable-new-indicator", false);
user_pref("browser.privatebrowsing.enable-new-logo", false);
user_pref("browser.quitShortcut.disabled", true);
user_pref("browser.toolbars.bookmarks.visibility", "never");
user_pref("browser.urlbar.groupLabels.enabled", false);
user_pref("browser.urlbar.suggest.engines", false);
user_pref("datareporting.policy.dataSubmissionEnabled", false);
user_pref("datareporting.usage.uploadEnabled", false);
user_pref("devtools.chrome.enabled", true);
user_pref("devtools.debugger.prompt-connection", false);
user_pref("devtools.debugger.remote-enabled", true);
user_pref("devtools.netmonitor.responseBodyLimit", 0);
user_pref("dom.event.clipboardevents.enabled", true); // false prevents pasting in Teams
// user_pref("dom.event.contextmenu.enabled", false);
user_pref("extensions.pocket.enabled", false);
user_pref("full-screen-api.warning.timeout", 0);
user_pref("general.smoothScroll", false);
user_pref("gfx.font_rendering.cleartype_params.enhanced_contrast", 100); // windows font rendering
user_pref("gfx.font_rendering.cleartype_params.rendering_mode", 5); // windows font rendering
user_pref("media.videocontrols.picture-in-picture.video-toggle.enabled", false);
user_pref("network.http.request.max-attempts", 2); // default 10, too many (possibly banned)
user_pref("network.security.ports.banned.override", "1-65535");
user_pref("pointer-lock-api.warning.timeout", 0);
user_pref("privacy.file_unique_origin", false); // Allow local script access local iframe's document
user_pref("security.dialog_enable_delay", 0);
user_pref("security.sandbox.content.level", 2); // needed for reading symlinked userContent.css (but not neeed  for reading symlinked userChrome.css)
user_pref("sidebar.position_start", false);
user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);
user_pref("toolkit.tabbox.switchByScrolling", true);
user_pref("ui.key.menuAccessKeyFocuses", false);
user_pref("xpinstall.whitelist.required", false);

// fonts (the "Default" fonts)
// user_pref("font.name-list.monospace.ja"         , "Droid Sans Japanese,Droid Sans Mono,IPAexGothic");
// user_pref("font.name-list.monospace.x-western"  , "Droid Sans Japanese,Droid Sans Mono,IPAexGothic");
// user_pref("font.name-list.sans-serif.ja"        , "Droid Sans Japanese,Droid Sans,FreeSans,IPAexGothic");
// user_pref("font.name-list.sans-serif.x-western" , "Droid Sans Japanese,Droid Sans,FreeSans,IPAexGothic");
// user_pref("font.name-list.serif.ja"             , "Liberation Serif,FreeSerif,IPAexMincho");
// user_pref("font.name-list.serif.x-western"      , "Liberation Serif,FreeSerif,IPAexMincho");
// Using:
//   Droid Sans Mono Dotted.ttf
//   YUGOTHM.TTC
user_pref("font.name-list.monospace.ja"         , "Droid Sans Mono Dotted,Droid Sans Mono,IPAexGothic1,IPAexGothic");
user_pref("font.name-list.monospace.x-western"  , "Droid Sans Mono Dotted,Droid Sans Mono,IPAexGothic1,IPAexGothic");
user_pref("font.name-list.sans-serif.ja"        , "Segoe UI,FreeSans,Yu Gothic,IPAexGothic1,IPAexGothic");
user_pref("font.name-list.sans-serif.x-western" , "Segoe UI,FreeSans,Yu Gothic,IPAexGothic1,IPAexGothic");
user_pref("font.name-list.serif.ja"             , "Liberation Serif,FreeSerif,IPAexMincho");
user_pref("font.name-list.serif.x-western"      , "Liberation Serif,FreeSerif,IPAexMincho");

// url bar: these are domains, not search words
user_pref("browser.fixup.domainsuffixwhitelist.server", true);

// addons on amo
user_pref("extensions.webextensions.restrictedDomains", "");
user_pref("privacy.resistFingerprinting.block_mozAddonManager", true);

// telemetry
user_pref("app.shield.optoutstudies.enabled", false);
user_pref("browser.safebrowsing.malware.enabled", false);
user_pref("browser.safebrowsing.phishing.enabled", false);
user_pref("datareporting.healthreport.uploadEnabled", false);

// firefox 128
// https://github.com/mozilla/explainers/tree/main/ppa-experiment
user_pref("dom.private-attribution.submission.enabled", false);

// Copied from https://gist.github.com/AetherEternity/5a6bb6e493a3d34988fc7342013f2ea6

// Mozilla User Preferences
// To change a preference value, you can either:
// - modify it via the UI (e.g. via about:config in the browser); or
// - set it within a user.js file in your profile (create it if it doesn't exist).
//
// Profile folder location on different systems:
// Windows: C:\Users\<username>\AppData\Roaming\Mozilla\Firefox\Profiles\xxxxxxxx.default
// Mac OS X: Users/<username>/Library/Application Support/Firefox/Profiles/xxxxxxxx.default
// Linux: /home/<username>/.mozilla/firefox/xxxxxxxx.default

user_pref("accessibility.force_disabled", 1);
user_pref("accessibility.typeaheadfind.flashBar", 0);
user_pref("app.normandy.first_run", false);
user_pref("app.shield.optoutstudies.enabled", false);
user_pref("app.update.auto", false);
user_pref("app.update.checkInstallTime", false);
user_pref("app.update.doorhanger", false);
user_pref("browser.download.useDownloadDir", false);
user_pref("browser.feeds.showFirstRunUI", false);
// user_pref("browser.newtabpage.activity-stream.feeds.section.highlights", false); // Use in firefox home
user_pref("browser.newtabpage.activity-stream.feeds.snippets", false);
user_pref("browser.newtabpage.activity-stream.feeds.telemetry", false);
// user_pref("browser.newtabpage.activity-stream.feeds.topsites", false); // Use in firefox home
user_pref("browser.newtabpage.activity-stream.prerender", false);
user_pref("browser.newtabpage.activity-stream.telemetry", false);
user_pref("browser.newtabpage.activity-stream.telemetry.ping.endpoint", "https://localhost");
user_pref("browser.ping-centre.telemetry", false);
user_pref("browser.safebrowsing.blockedURIs.enabled", false);
user_pref("browser.safebrowsing.downloads.enabled", false);
user_pref("browser.safebrowsing.downloads.remote.block_dangerous", false);
user_pref("browser.safebrowsing.downloads.remote.block_dangerous_host", false);
user_pref("browser.safebrowsing.downloads.remote.block_potentially_unwanted", false);
user_pref("browser.safebrowsing.downloads.remote.block_uncommon", false);
user_pref("browser.safebrowsing.downloads.remote.enabled", false);
user_pref("browser.safebrowsing.downloads.remote.url", "https://localhost");
user_pref("browser.safebrowsing.malware.enabled", false);
user_pref("browser.safebrowsing.phishing.enabled", false);
user_pref("browser.safebrowsing.provider.google.advisoryURL", "https://localhost'");
user_pref("browser.safebrowsing.provider.google.gethashURL", "https://localhost");
user_pref("browser.safebrowsing.provider.google.lists", "https://localhost");
user_pref("browser.safebrowsing.provider.google.reportMalwareMistakeURL", "https://localhost");
user_pref("browser.safebrowsing.provider.google.reportPhishMistakeURL", "https://localhost");
user_pref("browser.safebrowsing.provider.google.reportURL", "https://localhost");
user_pref("browser.safebrowsing.provider.google.updateURL", "https://localhost");
user_pref("browser.safebrowsing.provider.google4.advisoryURL", "https://localhost");
user_pref("browser.safebrowsing.provider.google4.dataSharingURL", "https://localhost");
user_pref("browser.safebrowsing.provider.google4.gethashURL", "https://localhost");
user_pref("browser.safebrowsing.provider.google4.reportMalwareMistakeURL", "https://localhost");
user_pref("browser.safebrowsing.provider.google4.reportPhishMistakeURL", "https://localhost");
user_pref("browser.safebrowsing.provider.google4.reportURL", "https://localhost");
user_pref("browser.safebrowsing.provider.google4.updateURL", "https://localhost");
user_pref("browser.safebrowsing.provider.mozilla.gethashURL", "https://localhost");
user_pref("browser.safebrowsing.provider.mozilla.nextupdatetime", "995795774646");
user_pref("browser.safebrowsing.provider.mozilla.updateURL", "https://localhost");
user_pref("browser.safebrowsing.reportPhishURL", "https://localhost");
user_pref("browser.search.geoSpecificDefaults.url", "https://localhost");
user_pref("browser.search.searchEnginesURL", "https://localhost");
// user_pref("browser.search.suggest.enabled", false); // I want suggestion in some profiles
user_pref("browser.search.update", false);
user_pref("browser.search.update.interval", 24000000);
user_pref("browser.startup.page", 3); // 3 - restore previous tabs
// user_pref("browser.startup.page", 0); // 0 - startup with single tab: about:blank
// user_pref("browser.urlbar.suggest.bookmark", false); // I want to suggest bookmark
// user_pref("browser.urlbar.suggest.history", false); // I want to suggest history
user_pref("captivedetect.maxRetryCount", 0);
user_pref("experiments.activeExperiment", false);
user_pref("experiments.manifest.uri", "https://localhost");
user_pref("extensions.update.enabled", false);
user_pref("general.warnOnAboutConfig", false);
user_pref("network.captive-portal-service.enabled", false);
user_pref("security.ssl.errorReporting.url", "https://localhost");
user_pref("services.settings.server", "https://localhost");
user_pref("services.sync.nextSync", 0);
user_pref("services.sync.prefs.sync.browser.safebrowsing.downloads.enabled", false);
user_pref("services.sync.prefs.sync.browser.safebrowsing.malware.enabled", false);
user_pref("services.sync.prefs.sync.browser.safebrowsing.passwords.enabled", false);
user_pref("services.sync.prefs.sync.browser.safebrowsing.phishing.enabled", false);
user_pref("toolkit.telemetry.archive.enabled", false);
user_pref("toolkit.telemetry.bhrPing.enabled", false);
user_pref("toolkit.telemetry.firstShutdownPing.enabled", false);
user_pref("toolkit.telemetry.hybridContent.enabled", false);
user_pref("toolkit.telemetry.newProfilePing.enabled", false);
user_pref("toolkit.telemetry.reportingpolicy.firstRun", false);
user_pref("toolkit.telemetry.shutdownPingSender.enabled", false);
user_pref("toolkit.telemetry.unified", false);
user_pref("toolkit.telemetry.updatePing.enabled", false);
user_pref("browser.newtabpage.activity-stream.section.highlights.includePocket", false);
user_pref("extensions.pocket.enabled", false);
user_pref("services.sync.prefs.sync.browser.newtabpage.activity-stream.section.highlights.includePocket", false);
user_pref("app.normandy.enabled", false);
user_pref("captivedetect.canonicalURL", "http://localhost");
user_pref("app.normandy.api_url", "http://localhost");
user_pref("app.normandy.shieldLearnMoreUrl", "http://localhost");
// optional - disable password saving
user_pref("signon.passwordEditCapture.enabled", false);
user_pref("services.sync.engine.passwords", false);
// optional - disable updates
user_pref("extensions.systemAddon.update.enabled", false);
user_pref("extensions.update.autoUpdateDefault", false);
