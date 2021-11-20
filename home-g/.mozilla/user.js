// user.js has precedence over about:config or settings panel customizations.
// (they will be reset to as in user.js when closing browser)
// But you can "install" settings by placing user.js, launch browser, then remove user.js.
// (Settings will be saved in prefs.js)

user_pref("browser.aboutConfig.showWarning", false);
user_pref("browser.download.autohideButton", true);
user_pref("browser.shell.checkDefaultBrowser", false);
user_pref("browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons", false);
user_pref("browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features", false);
user_pref("browser.newtabpage.activity-stream.showSponsoredTopSites", false);
user_pref("browser.tabs.closeWindowWithLastTab", false);
user_pref("browser.tabs.warnOnClose", false);
user_pref("browser.quitShortcut.disabled", true);
user_pref("browser.toolbars.bookmarks.visibility", "never");
user_pref("browser.urlbar.groupLabels.enabled", false);
user_pref("browser.urlbar.suggest.engines", false);
user_pref("devtools.chrome.enabled", true);
user_pref("devtools.debugger.prompt-connection", false);
user_pref("devtools.debugger.remote-enabled", true);
user_pref("devtools.netmonitor.responseBodyLimit", 0);
user_pref("extensions.pocket.enabled", false);
user_pref("full-screen-api.warning.timeout", 0);
user_pref("general.smoothScroll", false);
user_pref("privacy.file_unique_origin", false); // Allow local script access local iframe's document
user_pref("security.dialog_enable_delay", 0);
user_pref("sidebar.position_start", false);
user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);
user_pref("ui.key.menuAccessKeyFocuses", false);
user_pref("xpinstall.whitelist.required", false);

// addons on amo
user_pref("extensions.webextensions.restrictedDomains", "");
user_pref("privacy.resistFingerprinting.block_mozAddonManager", true);

// telemetry
user_pref("app.shield.optoutstudies.enabled", false);
user_pref("browser.safebrowsing.malware.enabled", false);
user_pref("browser.safebrowsing.phishing.enabled", false);
user_pref("datareporting.healthreport.uploadEnabled", false);



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
