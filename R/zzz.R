.onLoad <- function(libname, pkgname) {
  options(
    CONSTANTS_QUANTOGRAM=list(
      RNG_START = 0.015,   # minimal threshold for search
      RNG_END = 1.0,       # maximal threshold for search
      STEP = 0.005,        # single step size
      Q_MIN = 0.03),        # minimal possible quantum within threshold range
    CONSTANTS_SLS=list(
      SLS_RNG_START = 0.2,
      SLS_RNG_END = 0.8,
      SLS_STEP = 0.01,
      SLS_MAX_N_QUANTA = 2))
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome! This is a version 3e-14 of quantatools.")
}
