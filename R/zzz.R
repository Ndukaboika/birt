# This runs when someone loads the package with library(bayesRasch)
# .onAttach is the correct place for startup messages (not .onLoad)
.onAttach <- function(libname, pkgname) {
  if (requireNamespace("cmdstanr", quietly = TRUE)) {
    tryCatch(
      cmdstanr::cmdstan_version(),
      error = function(e) {
        packageStartupMessage(
          "CmdStan not found. Install it with:\n",
          "  cmdstanr::install_cmdstan()\n",
          "See https://mc-stan.org/cmdstanr/ for details."
        )
      }
    )
  }
}
