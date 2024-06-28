options(repos = Sys.getenv("CRAN_MIRROR"))

# Source: https://github.com/REditorSupport/vscode-R/wiki/Plot-viewer#svg-in-httpgd-webpage
if (interactive() && Sys.getenv("TERM_PROGRAM") == "vscode") {
    if ("httpgd" %in% .packages(all.available = TRUE)) {
        options(vsc.plot = FALSE)
        options(device = function(...) {
            httpgd::hgd(silent = TRUE)
            .vsc.browser(httpgd::hgd_url(history = FALSE), viewer = "Beside")
        })
    }
}

# Source: https://github.com/REditorSupport/vscode-R/wiki/R-Session-watcher#advanced-usage-for-self-managed-r-sessions
if (interactive()) {
    source("/root/.vscode-R/init.R")
}
