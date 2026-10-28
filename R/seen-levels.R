# Levels the board's renderers have resolved, per bound variable. The scale
# map editor offers them when pinning a level, so a user can search patient
# ids without typing them. Every renderer reaches the map through
# resolve_scales_col(), which notes the levels here. Kept per session in
# `session$userData`; nothing is stored with the board.

seen_levels_store <- function(session) {
  if (is.null(session)) {
    return(NULL)
  }
  ud <- session$userData
  if (is.null(ud$blockr_theme_seen)) {
    st <- new.env(parent = emptyenv())
    st$levels <- list()
    st$tick <- shiny::reactiveVal(0L)
    ud$blockr_theme_seen <- st
  }
  ud$blockr_theme_seen
}

scale_map_note_levels <- function(var, levels,
                                  session = shiny::getDefaultReactiveDomain()) {
  st <- seen_levels_store(session)
  if (is.null(st) || !length(levels)) {
    return(invisible(NULL))
  }
  old <- st$levels[[var]]
  new <- setdiff(levels, old)
  if (length(new)) {
    st$levels[[var]] <- c(old, new)
    st$tick(shiny::isolate(st$tick()) + 1L)
  }
  invisible(NULL)
}

# Reactive read: re-runs the caller when a renderer shows new levels.
scale_map_seen_levels <- function(var,
                                  session = shiny::getDefaultReactiveDomain()) {
  st <- seen_levels_store(session)
  if (is.null(st)) {
    return(character())
  }
  st$tick()
  sort(st$levels[[var]] %||% character())
}
