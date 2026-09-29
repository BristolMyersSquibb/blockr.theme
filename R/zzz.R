# The board options sidebar in blockr.dock summarises each option category
# in one line, through its option_summary() generic. The scale map says how
# many variables it pins. blockr.dock is not a dependency, and older
# versions have no option_summary(), so the method is registered at run
# time, when blockr.dock is (or gets) loaded and has the generic.

scale_map_option_summary <- function(x, value, ...) {

  n <- length(unclass(value))

  if (!n) {
    return("No variables")
  }

  paste(n, if (n == 1L) "variable" else "variables")
}

register_dock_summary <- function(...) {

  if (!isNamespaceLoaded("blockr.dock")) {
    return(invisible(FALSE))
  }

  ns <- asNamespace("blockr.dock")

  if (!exists("option_summary", envir = ns, inherits = FALSE)) {
    return(invisible(FALSE))
  }

  registerS3method(
    "option_summary",
    "scale_map_option",
    scale_map_option_summary,
    envir = ns
  )

  invisible(TRUE)
}

.onLoad <- function(libname, pkgname) {
  register_dock_summary()
  setHook(
    packageEvent("blockr.dock", "onLoad"),
    register_dock_summary
  )
}
