# Board-level scale map: per-variable discrete scales (level -> color/shape/
# linetype) carried as the "scale_map" board option and resolved at render
# time. Spec: blockr.design/open/blockr.theme (supersedes the placement of
# open/cdex-attribute-map). This package is the single home of the option id,
# value shape and hash assignment; renderers consume via
# `Suggests: blockr.theme` and fall back to their standard colors when the
# package is absent.

SCALE_MAP_CHANNELS <- c("color", "shape", "linetype")

# Reserved entry name carrying the board palette through serialization;
# rejected as a variable name in as_scale_map().
SCALE_MAP_PALETTE_KEY <- ".palette"

# Reserved key carrying the pool of a channel that also has fixed values,
# when the channel travels as a JSON object.
SCALE_MAP_POOL_KEY <- ".pool"

#' Scale map: study-wide level aesthetics
#'
#' A scale map binds variable levels to aesthetics (colors, shapes, line
#' types) board-wide: every rendering block that consumes the map shows the
#' same level in the same color. Bindings are keyed by variable name. Each
#' channel is one vector: *named* entries fix values per level (stated in
#' display order), *unnamed* entries are a pool from which the other levels
#' are assigned by a stable hash of the level name (consistent across views,
#' sessions and data refreshes). One vector may hold both: pinned patients on
#' top of a patient pool, `c("01-701-1015" = "#d55e00", pool)`. A hashed
#' level never takes a value pinned in the same binding: it moves on to the
#' next pool entry, so a new treatment arm cannot share a pinned arm's colour
#' while the pool has a free one. Levels of a `color` channel not covered by
#' either fall back to the map's board palette, then to the palette supplied
#' at resolution time (typically the renderer's default colors).
#'
#' `new_scale_map()` accepts bindings and whole maps in any mix and flattens
#' them with later-wins-by-variable semantics, so a study overrides a default
#' catalog by listing replacement bindings after it. Overriding replaces the
#' whole binding (no channel-level merge).
#'
#' @param ... For `new_scale_map()`: `scale_binding()` objects, `scale_map`
#'   objects or plain lists of the same shape (later entries win by variable
#'   name). For `new_scale_map_option()`: forwarded to
#'   [blockr.core::new_board_option()].
#' @param var Variable (column) name the binding applies to
#' @param color,shape,linetype Channel vectors: named entries = fixed values
#'   per level (names are always matched as character, also for
#'   numeric-looking levels such as AE grades `"1"`–`"5"`), unnamed entries =
#'   pool for stable-hash auto-assignment. Both may be mixed in one vector. `shape` is coerced to integer (R `pch` / symbol codes).
#' @param palette Optional board palette: an unnamed character vector of
#'   colors used as the fallback pool for any binding without a pool of its
#'   own. Carried with the map (and the board). When merging maps, the last
#'   non-`NULL` palette wins.
#'
#' @return `new_scale_map()` and `as_scale_map()` return a `scale_map` object
#'   (a named list of bindings); `scale_binding()` returns a `scale_binding`;
#'   `resolve_scales()` returns a list with entries `color`, `shape`,
#'   `linetype` (named vectors over the supplied levels; absent when nothing
#'   resolves) and `order` (character), or `NULL` for an unregistered
#'   variable; `new_scale_map_option()` returns a `board_option`.
#'
#' @examples
#' # Fixed colors for pinned levels, a pool for auto-assigned ones, and a
#' # board palette as the fallback pool for bare registrations:
#' map <- new_scale_map(
#'   scale_binding("BOR", color = c(CR = "#006400", PD = "#8b0000")),
#'   scale_binding("USUBJID", color = palette_colors(12, "Many")),
#'   scale_binding("RACE"),
#'   palette = c("#0072B2", "#D55E00", "#F0E442")
#' )
#'
#' # A study overrides by listing replacement bindings after a template map:
#' study <- new_scale_map(
#'   map,
#'   scale_binding("BOR", color = c(CR = "#008000", PD = "#FF2C2C"))
#' )
#'
#' # Renderers resolve against the levels actually shown; same level, same
#' # color, in every view (runnable board: dev/scale-map-demo.R):
#' resolve_scales(study, "BOR", levels = c("PD", "CR", "NEW"))
#' resolve_scales(study, "RACE", levels = c("WHITE", "ASIAN"))
#'
#' # Pins on top of a pool: the pinned patient keeps its colour, the others
#' # are hashed into the pool.
#' pts <- new_scale_map(scale_binding(
#'   "USUBJID",
#'   color = c("01-701-1015" = "#d55e00", palette_colors(12, "Many"))
#' ))
#' resolve_scales(pts, "USUBJID", c("01-701-1015", "01-701-1023"))
#'
#' @export
new_scale_map <- function(..., palette = NULL) {
  args <- Filter(Negate(is.null), list(...))

  res <- list()
  pal <- palette
  for (x in args) {
    if (inherits(x, "scale_binding")) {
      res[[attr(x, "var")]] <- unclass_binding(x)
    } else if (is.list(x)) {
      x <- as_scale_map(x)
      for (var in names(x)) {
        res[[var]] <- x[[var]]
      }
      pal <- attr(x, "palette", exact = TRUE) %||% pal
    } else {
      stop("`new_scale_map()` expects scale_binding or scale_map objects.",
           call. = FALSE)
    }
  }

  if (!is.null(palette)) {
    pal <- palette
  }

  structure(
    res,
    palette = validate_palette(pal),
    class = c("scale_map", "list")
  )
}

validate_palette <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.list(x)) {
    x <- unlist(x, use.names = FALSE)
  }
  if (!is.character(x) || !length(x) || !is.null(names(x))) {
    stop("`palette` must be an unnamed character vector of colors.",
         call. = FALSE)
  }
  x
}

#' @rdname new_scale_map
#' @export
scale_binding <- function(var, color = NULL, shape = NULL, linetype = NULL) {
  stopifnot(is.character(var), length(var) == 1L, nzchar(var))

  channels <- Filter(
    Negate(is.null),
    list(
      color = validate_channel(color, "color", var),
      shape = validate_channel(shape, "shape", var),
      linetype = validate_channel(linetype, "linetype", var)
    )
  )

  structure(channels, var = var, class = "scale_binding")
}

unclass_binding <- function(x) {
  attr(x, "var") <- NULL
  unclass(x)
}

validate_channel <- function(x, channel, var) {
  if (is.null(x)) {
    return(NULL)
  }

  if (is.list(x)) {
    # Serialized form of a mixed channel: the fixed values as a named list
    # with the pool under SCALE_MAP_POOL_KEY (see scale_map_to_plain()).
    pool <- NULL
    if (SCALE_MAP_POOL_KEY %in% names(x)) {
      pool <- unlist(x[[SCALE_MAP_POOL_KEY]], use.names = FALSE)
      x <- x[names(x) != SCALE_MAP_POOL_KEY]
    }
    nms <- names(x)
    x <- unlist(x, use.names = FALSE)
    names(x) <- nms
    if (length(pool)) {
      x <- c(x, pool)
    }
  }

  if (!is.atomic(x) || length(x) == 0L) {
    stop("Channel `", channel, "` of binding `", var,
         "` must be a non-empty vector.", call. = FALSE)
  }

  # NA names (from c() of a named and an unnamed vector in some paths) mean
  # unnamed; no named entry at all means a plain pool.
  nms <- names(x)
  if (!is.null(nms)) {
    nms[is.na(nms)] <- ""
    names(x) <- nms
    if (!any(nzchar(nms))) {
      names(x) <- NULL
      nms <- NULL
    }
  }

  if (!is.null(nms) && anyDuplicated(nms[nzchar(nms)])) {
    stop("Channel `", channel, "` of binding `", var,
         "` has duplicated level names.", call. = FALSE)
  }

  if (identical(channel, "shape")) {
    nms <- names(x)
    x <- as.integer(x)
    names(x) <- nms
  } else {
    nms <- names(x)
    x <- as.character(x)
    names(x) <- nms
  }

  x
}

#' @param x Object to coerce / test
#' @rdname new_scale_map
#' @export
is_scale_map <- function(x) {
  inherits(x, "scale_map")
}

#' @rdname new_scale_map
#' @export
as_scale_map <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

  if (is_scale_map(x)) {
    return(x)
  }

  stopifnot(is.list(x))

  pal <- attr(x, "palette", exact = TRUE)
  if (SCALE_MAP_PALETTE_KEY %in% names(x)) {
    pal <- pal %||% x[[SCALE_MAP_PALETTE_KEY]]
    x <- x[setdiff(names(x), SCALE_MAP_PALETTE_KEY)]
  }

  if (length(x) && (is.null(names(x)) || !all(nzchar(names(x))))) {
    stop("A scale map must be a fully named list (variable names).",
         call. = FALSE)
  }

  if (any(startsWith(names(x), "."))) {
    stop("Variable names starting with `.` are reserved in a scale map.",
         call. = FALSE)
  }

  res <- lapply(names(x), function(var) {
    binding <- x[[var]]
    stopifnot(is.list(binding))
    # Entries other than the known channels are tolerated and dropped:
    # forward-compat with binding kinds this version does not know (e.g. a
    # future `ramp`). Authoring through scale_binding() stays strict.
    chans <- lapply(
      SCALE_MAP_CHANNELS,
      function(ch) validate_channel(binding[[ch]], ch, var)
    )
    names(chans) <- SCALE_MAP_CHANNELS
    Filter(Negate(is.null), chans)
  })
  names(res) <- names(x)

  structure(
    res,
    palette = validate_palette(pal),
    class = c("scale_map", "list")
  )
}

#' @export
print.scale_map <- function(x, ...) {
  pal <- attr(x, "palette", exact = TRUE)
  cat("<scale_map[", length(x), "]>",
      if (!is.null(pal)) sprintf(" palette[%d]", length(pal)),
      "\n", sep = "")
  for (var in names(x)) {
    chs <- names(x[[var]])
    desc <- if (length(chs)) {
      paste(
        vapply(chs, function(ch) {
          v <- x[[var]][[ch]]
          parts <- c(
            if (length(scale_spec_fixed(v))) {
              paste("fixed", length(scale_spec_fixed(v)))
            },
            if (length(scale_spec_pool(v))) {
              paste("pool", length(scale_spec_pool(v)))
            }
          )
          paste0(ch, "[", paste(parts, collapse = " + "), "]")
        }, character(1L)),
        collapse = ", "
      )
    } else {
      "auto"
    }
    cat("  ", var, ": ", desc, "\n", sep = "")
  }
  invisible(x)
}

#' Split a scale map channel into fixed values and pool
#'
#' A channel vector holds fixed values (named entries) and a pool (unnamed
#' entries), in any mix. Renderers that look at a binding themselves, rather
#' than through [resolve_scales()], use these two to read it.
#'
#' @param spec One channel vector of a binding, e.g. `map[["USUBJID"]]$color`.
#' @return `scale_spec_fixed()`: the named entries, or `NULL`.
#'   `scale_spec_pool()`: the unnamed entries without names, or `NULL`.
#' @examples
#' spec <- c("01-701-1015" = "#d55e00", "#0072b2", "#e69f00")
#' scale_spec_fixed(spec)
#' scale_spec_pool(spec)
#' @export
scale_spec_fixed <- function(spec) {
  nms <- names(spec)
  if (!length(spec) || is.null(nms)) {
    return(NULL)
  }
  keep <- !is.na(nms) & nzchar(nms)
  if (any(keep)) spec[keep] else NULL
}

#' @rdname scale_spec_fixed
#' @export
scale_spec_pool <- function(spec) {
  if (!length(spec)) {
    return(NULL)
  }
  nms <- names(spec)
  keep <- if (is.null(nms)) rep(TRUE, length(spec)) else is.na(nms) | !nzchar(nms)
  if (any(keep)) unname(spec[keep]) else NULL
}

# Auto-assignment for one level: a pure function of the level name, so a value
# keeps its scale across views showing different subsets of levels. That is the
# property the map exists for, and the one worth preserving here.
#
# Colors are not stable across rlang versions: rlang::hash() was reimplemented
# in 1.3.0 and every hash changed. Since resolve_scales() runs at render time,
# an rlang upgrade re-colors auto-assigned levels on an existing board. That
# drift is accepted — views stay consistent with each other, which is what
# matters. Pin expected colors in a test and it will fail on the next rlang
# hash change.
#
# `taken`: values pinned to other levels of the same binding. A hashed level
# that lands on one of them steps on to the next pool entry nobody pinned, so
# a new treatment arm never takes a pinned arm's colour. The pool keeps its
# length, so pinning one level only moves the levels that landed on exactly
# that colour. This depends on the binding alone, never on which levels a
# view shows.
scale_map_hash_pick <- function(level, pool, taken = NULL) {
  n <- length(pool)
  idx <- strtoi(substr(rlang::hash(level), 1L, 7L), 16L) %% n
  free <- !tolower(as.character(pool)) %in% tolower(as.character(taken))
  if (!any(free)) {
    return(pool[[idx + 1L]])
  }
  step <- which(free[(seq_len(n) + idx - 1L) %% n + 1L])[[1L]] - 1L
  pool[[(idx + step) %% n + 1L]]
}

#' @param map A `scale_map` (or plain list of the same shape, or `NULL`)
#' @param levels Character vector of levels actually shown (pass
#'   `levels(col)` for factors, `unique(as.character(col))` otherwise)
#' @param palette Fallback pool for the `color` channel when neither the
#'   binding nor the map carries one (typically the renderer's default
#'   colors); `shape`/`linetype` have no fallback pool
#' @rdname new_scale_map
#' @export
resolve_scales <- function(map, var, levels, palette = NULL) {
  map <- as_scale_map(map)

  if (is.null(map) || is.null(var) || !length(levels) ||
        !var %in% names(map)) {
    return(NULL)
  }

  levels <- unique(as.character(levels))
  binding <- map[[var]]
  board_palette <- attr(map, "palette", exact = TRUE)

  resolve_channel <- function(channel, fallback_pool = NULL) {
    spec <- binding[[channel]]
    fixed <- scale_spec_fixed(spec)
    pool <- scale_spec_pool(spec) %||% fallback_pool

    vals <- lapply(levels, function(lv) {
      if (!is.null(fixed) && lv %in% names(fixed)) {
        fixed[[lv]]
      } else if (!is.null(pool) && length(pool)) {
        scale_map_hash_pick(lv, pool, taken = unname(fixed))
      } else {
        NULL
      }
    })

    keep <- !vapply(vals, is.null, logical(1L))
    if (!any(keep)) {
      return(NULL)
    }

    out <- unlist(vals[keep])
    names(out) <- levels[keep]
    out
  }

  fixed_names <- unique(unlist(lapply(
    binding[SCALE_MAP_CHANNELS],
    function(spec) names(scale_spec_fixed(spec))
  )))

  res <- Filter(
    Negate(is.null),
    list(
      color = resolve_channel(
        "color",
        fallback_pool = board_palette %||% palette
      ),
      shape = resolve_channel("shape"),
      linetype = resolve_channel("linetype")
    )
  )

  res$order <- c(intersect(fixed_names, levels), setdiff(levels, fixed_names))

  res
}

#' @param column The data column itself (not just its name). Levels are taken
#'   from it (factor levels, else observed values in appearance order, `NA`
#'   dropped), and when `var` is not bound in the map the column's
#'   `blockr_source` attribute -- the provenance stamped by column-copying
#'   blocks such as the picker -- is tried instead, so a copy inherits its
#'   source column's binding (a "color" column picked from SEX keeps the
#'   fixed SEX colors). A factor's levels also set `order`, ahead of the
#'   binding's: a factor declares the order the data wants shown.
#' @rdname new_scale_map
#' @export
resolve_scales_col <- function(map, var, column, palette = NULL) {
  m <- as_scale_map(map)
  if (is.null(m) || is.null(var)) {
    return(NULL)
  }

  levels <- if (is.factor(column)) {
    levels(column)
  } else {
    lv <- unique(as.character(column))
    lv[!is.na(lv)]
  }

  bind_var <- var
  if (!var %in% names(m)) {
    src <- attr(column, "blockr_source", exact = TRUE)
    if (is.character(src) && length(src) == 1L && nzchar(src) &&
          src %in% names(m)) {
      bind_var <- src
    }
  }

  res <- resolve_scales(m, bind_var, levels = levels, palette = palette)
  if (!is.null(res)) scale_map_note_levels(bind_var, levels)
  # A factor's levels are the data's own order and beat the binding's: the
  # binding still colours each level by name.
  if (!is.null(res) && is.factor(column)) res$order <- levels
  res
}

#' @rdname new_scale_map
#' @export
board_scale_map <- function() {
  shiny::reactive({
    val <- blockr.core::get_board_option_or_null(
      "scale_map", blockr.core::get_session()
    )
    if (is.null(val) || !length(val)) NULL else as_scale_map(val)
  })
}

#' @param map Initial map value (e.g. a template catalog amended with study
#'   bindings)
#' @param category Settings sidebar category
#' @rdname new_scale_map
#' @export
new_scale_map_option <- function(map = new_scale_map(), category = "Scales",
                                 ...) {
  blockr.core::new_board_option(
    id = "scale_map",
    default = as_scale_map(map),
    ui = scale_map_editor_ui,
    server = scale_map_editor_server,
    update_trigger = NULL,
    transform = function(x) as_scale_map(x),
    category = category,
    ...
  )
}

# The option value (a named list keyed by VARIABLE names) cannot go through
# blockr_ser.board_option() as-is: that method matches value names against
# constructor argument names. Wrap it under the `map` argument, mirroring
# blockr_ser.llm_model_option().
#' @exportS3Method blockr.core::blockr_ser
blockr_ser.scale_map_option <- function(x, option = NULL, ...) {
  val <- option %||% blockr.core::board_option_value(x)
  NextMethod(option = list(map = scale_map_to_plain(val)))
}

scale_map_to_plain <- function(x) {
  if (is.null(x)) {
    return(list())
  }
  # jsonlite drops names on atomic vectors (only lists serialize as JSON
  # objects), so fixed channels must travel as named lists. The board
  # palette travels as the reserved `.palette` entry (unnamed -> array).
  plain <- lapply(unclass(x), function(binding) {
    lapply(binding, function(spec) {
      fixed <- scale_spec_fixed(spec)
      pool <- scale_spec_pool(spec)
      if (is.null(fixed)) {
        return(pool)
      }
      out <- as.list(fixed)
      if (length(pool)) out[[SCALE_MAP_POOL_KEY]] <- pool
      out
    })
  })
  pal <- attr(x, "palette", exact = TRUE)
  if (!is.null(pal)) {
    plain[[SCALE_MAP_PALETTE_KEY]] <- pal
  }
  plain
}
