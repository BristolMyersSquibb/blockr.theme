# Board-level look-and-feel theme: one object bundling the several axes that
# together make a board "look like" an organisation. Distinct from the scale
# map (scale-map.R), which pins DATA values to aesthetics; a `blockr_theme`
# is the surrounding house style. The two are complementary and a theme may
# carry a scale map as one of its facets.
#
# The facets, and why each is a facet:
#   * chrome    -- UI custom-property overrides. Every blockr package styles
#                  its widgets off `var(--blockr-*, <fallback>)` tokens
#                  defined in blockr.dock's :root. A theme is therefore just
#                  a scoped redefinition of those tokens; nothing is
#                  hard-coded to override. Vanilla == the fallbacks.
#   * exhibits  -- rendered-output styling read as `blockr.viz.*` options
#                  (e.g. flextable header bands). Content, not chrome.
#   * palettes  -- colour palettes keyed by ROLE (what the colour is for),
#                  plus any palette definitions the theme contributes. One
#                  vocabulary for every consumer: before this, series colours
#                  were a hard-coded constant in blockr.viz, header bands were
#                  a blockr.viz option and heatmap ramps were a function
#                  argument, so a client theme could reach exactly one of the
#                  three. See theme_palette().
#   * scales    -- an optional scale_map (data value -> colour/shape).
#   * templates -- document templates keyed by FORMAT (pptx today; docx /
#                  quarto / latex later). Either a pointer into another
#                  package's inst/ or a file contained in this package.
#   * font_family -- an optional CSS font stack for the whole board.
#
# Renderers and apps consume a theme through soft, additive seams: inject
# the chrome CSS into the page head, set the exhibit options, hand the scale
# map to the board, pass the template into the render call. Absent any of
# them, the board degrades to vanilla.

#' Board look-and-feel theme
#'
#' A `blockr_theme` bundles the house style of a board: UI chrome tokens,
#' rendered-exhibit palettes, an optional data scale map, document templates
#' and a font. It is consumed through additive seams (see [use_theme()],
#' [theme_head()], [theme_template()], [theme_scale_map_option()]); a board
#' with no theme, or a theme missing a facet, simply falls back to the
#' blockr defaults.
#'
#' @param name Short theme name (e.g. `"BMS"`).
#' @param chrome Named list of UI token overrides. Names are token stems
#'   appended to `--blockr-` (`"color-primary"`, `"blue-600"`, ...); a few
#'   friendly aliases are accepted (`primary`, `primary_hover`, `primary_bg`,
#'   `border`, `text`). Values are any CSS colour. Overriding the base scale
#'   (`blue-600`) plus the semantic layer (`color-primary`) shifts the whole
#'   cascade, since the semantic tokens reference the scale.
#' @param exhibits Named list of `blockr.viz.*` option values (the leading
#'   `blockr.viz.` is added), e.g. `list(ft_header_bg = c(...))`.
#' @param palettes Named list mapping a colour ROLE to a palette name (or to a
#'   literal colour vector). Roles are `categorical` (series identity),
#'   `identity` (many-level pools such as subject ids), `sequential`,
#'   `diverging` and `bands` (table header / emphasis fills). Unset roles fall
#'   back to the blockr defaults, so a theme names only what it changes. See
#'   [theme_palette()].
#' @param palette_defs Named list of palette definitions this theme
#'   contributes, each a colour vector or a `function(n)`. They become
#'   available to [palette_colors()] / [palette_ramp()] and to the scale-map
#'   editor's palette picker for as long as the theme is applied, without
#'   mutating any global registry.
#' @param scales Optional [new_scale_map()] (or plain list of that shape).
#' @param templates Named list of document templates keyed by format
#'   (`pptx`, `docx`, `quarto`, ...). Each value is a [pkg_template()] /
#'   [local_template()] pointer or a plain file path.
#' @param font_family Optional CSS font-family stack applied board-wide.
#'   When `webfont` is set and this is `NULL`, it is derived as the webfont
#'   family followed by a system fallback stack.
#' @param webfont Optional bundled web font (see [bundled_font()]). Its
#'   `@font-face` is inlined into the theme's CSS as a base64 data URI, so
#'   the font renders without any external request or resource-path setup.
#' @param description Optional one-line human description.
#'
#' @return `blockr_theme()` returns a `blockr_theme` object.
#' @examples
#' th <- blockr_theme(
#'   name = "demo",
#'   chrome = list("blue-600" = "#BE2BBB", "color-primary" = "#BE2BBB"),
#'   exhibits = list(ft_header_bg = c(.stub = "#EEE", "#33D6F1"))
#' )
#' cat(theme_css(th))
#' @export
blockr_theme <- function(name, chrome = list(), exhibits = list(),
                         palettes = list(), palette_defs = list(),
                         scales = NULL, templates = list(),
                         font_family = NULL, webfont = NULL,
                         description = NULL) {

  stopifnot(is.character(name), length(name) == 1L, nzchar(name))

  if (!is.null(webfont)) {
    if (!inherits(webfont, "blockr_webfont")) {
      stop("`webfont` must come from bundled_font().", call. = FALSE)
    }
    # A bundled font sets the family unless the caller pinned a stack.
    if (is.null(font_family)) {
      font_family <- paste0(
        "'", webfont$family, "', system-ui, -apple-system, 'Segoe UI', ",
        "Roboto, sans-serif"
      )
    }
  }

  named_list <- function(x, what) {
    if (!is.list(x) || (length(x) && is.null(names(x))) ||
          (length(x) && !all(nzchar(names(x))))) {
      stop("`", what, "` must be a fully named list.", call. = FALSE)
    }
    x
  }

  chrome <- named_list(chrome, "chrome")
  exhibits <- named_list(exhibits, "exhibits")
  templates <- named_list(templates, "templates")
  palettes <- named_list(palettes, "palettes")
  palette_defs <- named_list(palette_defs, "palette_defs")

  unknown <- setdiff(names(palettes), PALETTE_ROLES)
  if (length(unknown)) {
    stop("Unknown palette role(s): ", paste(unknown, collapse = ", "),
         ". Known roles: ", paste(PALETTE_ROLES, collapse = ", "), ".",
         call. = FALSE)
  }

  # Fail here rather than at render time: a malformed definition is an
  # authoring mistake in the theme, and the theme is built at startup.
  lapply(palette_defs, as_palette_entry)

  if (!is.null(scales)) {
    scales <- as_scale_map(scales)
  }

  structure(
    list(
      name = name,
      description = description,
      chrome = chrome,
      exhibits = exhibits,
      palettes = palettes,
      palette_defs = palette_defs,
      scales = scales,
      templates = templates,
      font_family = font_family,
      webfont = webfont
    ),
    class = c("blockr_theme", "list")
  )
}

#' @param x Object to test / operate on.
#' @rdname blockr_theme
#' @export
is_blockr_theme <- function(x) {
  inherits(x, "blockr_theme")
}

#' @export
print.blockr_theme <- function(x, ...) {
  cat("<blockr_theme: ", x$name, ">\n", sep = "")
  if (!is.null(x$description)) {
    cat("  ", x$description, "\n", sep = "")
  }
  cat("  chrome    : ", length(x$chrome), " token(s)\n", sep = "")
  cat("  exhibits  : ",
      if (length(x$exhibits)) paste(names(x$exhibits), collapse = ", ") else "-",
      "\n", sep = "")
  cat("  palettes  : ",
      if (length(x$palettes)) {
        paste(vapply(names(x$palettes), function(r) {
          v <- x$palettes[[r]]
          paste0(r, "=", if (length(v) == 1L && is.character(v)) v else
            sprintf("<%d colors>", length(v)))
        }, character(1L)), collapse = ", ")
      } else "-",
      if (length(x$palette_defs)) {
        sprintf(" (+%d definition(s))", length(x$palette_defs))
      },
      "\n", sep = "")
  cat("  scales    : ",
      if (is.null(x$scales)) "-" else sprintf("scale_map[%d]", length(x$scales)),
      "\n", sep = "")
  cat("  templates : ",
      if (length(x$templates)) paste(names(x$templates), collapse = ", ") else "-",
      "\n", sep = "")
  if (!is.null(x$font_family)) {
    cat("  font      : ", x$font_family, "\n", sep = "")
  }
  invisible(x)
}

# --- webfont: a bundled face inlined as a data URI ------------------------

#' Bundle a web font with a theme
#'
#' Describes a font file shipped in a package's `inst/fonts` (this package by
#' default). [theme_css()] inlines it as a base64 `@font-face` data URI, so
#' the font travels inside the theme's `<style>` with no external request or
#' Shiny resource path -- the same self-contained approach as the token rule.
#'
#' @param family CSS family name to register the face under (e.g. `"Inter"`).
#' @param file Font file within `inst/fonts` (a `.woff2` is expected).
#' @param package Package holding the font (default this one).
#' @param weight,style `font-weight` / `font-style` descriptors; the default
#'   `"100 900"` weight declares a variable face spanning the range.
#' @return A `blockr_webfont` object consumed by [blockr_theme()].
#' @export
bundled_font <- function(family, file, package = "blockr.theme",
                         weight = "100 900", style = "normal") {
  stopifnot(is.character(family), length(family) == 1L, nzchar(family))
  structure(
    list(family = family, file = file, package = package,
         weight = weight, style = style),
    class = "blockr_webfont"
  )
}

# Read the bundled file and return its @font-face rule with the woff2 inlined
# as a data URI. Returns "" when the file cannot be resolved (soft, like
# theme_template) so a missing asset degrades to the fallback stack rather
# than erroring.
webfont_face_css <- function(webfont) {
  if (is.null(webfont)) {
    return("")
  }
  if (!requireNamespace(webfont$package, quietly = TRUE)) {
    return("")
  }
  path <- system.file(file.path("fonts", webfont$file), package = webfont$package)
  if (!nzchar(path)) {
    return("")
  }
  raw <- readBin(path, "raw", file.info(path)$size)
  uri <- paste0(
    "data:font/woff2;base64,", jsonlite::base64_enc(raw)
  )
  paste0(
    "@font-face {\n",
    "  font-family: '", webfont$family, "';\n",
    "  font-style: ", webfont$style, ";\n",
    "  font-weight: ", webfont$weight, ";\n",
    "  font-display: swap;\n",
    "  src: url(", uri, ") format('woff2');\n",
    "}\n"
  )
}

# --- chrome: tokens -> CSS ------------------------------------------------

# A handful of high-level aliases so a preset can say `primary` instead of
# the full semantic token. Anything not listed is used verbatim (after
# `_`->`-`), so the full token vocabulary stays reachable.
THEME_TOKEN_ALIASES <- c(
  primary        = "color-primary",
  primary_hover  = "color-primary-hover",
  primary_bg     = "color-primary-bg",
  border         = "color-border",
  text           = "color-text-primary",
  bg             = "color-bg-subtle"
)

normalize_token <- function(nm) {
  nm <- gsub("_", "-", nm)
  hit <- THEME_TOKEN_ALIASES[nm]
  if (!is.na(hit)) unname(hit) else nm
}

#' Compile a theme's chrome to a CSS rule
#'
#' Emits the token overrides as a single custom-property rule on `selector`
#' (`:root` by default). Because every consumer reads the tokens with a
#' fallback, this rule is all a chrome theme is: redefine the properties on
#' a scope that wins the cascade (source order after the base CSS).
#'
#' @param x A [blockr_theme()].
#' @param selector CSS selector the rule is scoped to. `:root` themes the
#'   whole page; a wrapper class (e.g. `.theme-bms`) scopes it to a subtree,
#'   which is how two differently themed boards could share one page.
#' @return A CSS string (length-1 character), or `""` if the theme sets no
#'   chrome tokens and no font.
#' @export
theme_css <- function(x, selector = ":root") {
  stopifnot(is_blockr_theme(x))

  # The bundled face leads, so the family it registers is defined before the
  # font-family rule below references it.
  face <- webfont_face_css(x$webfont)

  decls <- vapply(
    names(x$chrome),
    function(nm) sprintf("  --blockr-%s: %s;", normalize_token(nm), x$chrome[[nm]]),
    character(1L)
  )

  css <- if (length(decls)) {
    paste0(selector, " {\n", paste(decls, collapse = "\n"), "\n}\n")
  } else {
    ""
  }

  if (!is.null(x$font_family)) {
    # The board's base face is bslib's Bootstrap variable, applied at `body`.
    # Redefine that variable AND set the family on `body` so both the
    # var-driven elements and any that read font-family directly switch. A
    # non-`:root` selector scopes both to that subtree instead (two boards,
    # one page).
    if (identical(selector, ":root")) {
      css <- paste0(
        css,
        sprintf(":root { --bs-body-font-family: %s; }\n", x$font_family),
        sprintf("body { font-family: %s; }\n", x$font_family)
      )
    } else {
      css <- paste0(
        css,
        sprintf("%s { --bs-body-font-family: %s; font-family: %s; }\n",
                selector, x$font_family, x$font_family)
      )
    }
  }

  paste0(face, css)
}

#' Theme chrome as page-head tags
#'
#' Wraps [theme_css()] in a `<style>` for dropping into an app's UI. Place it
#' after the blockr dependencies so its `:root` rule wins by source order.
#'
#' @param x A [blockr_theme()].
#' @param selector Passed to [theme_css()].
#' @return An [htmltools::tagList()] (empty when the theme sets no chrome).
#' @export
theme_head <- function(x, selector = ":root") {
  css <- theme_css(x, selector)
  if (!nzchar(css)) {
    return(htmltools::tagList())
  }
  htmltools::tags$style(htmltools::HTML(css))
}

# --- exhibits: options ----------------------------------------------------

#' Apply a theme's exhibit options
#'
#' Sets the `blockr.viz.*` options the theme carries (flextable header bands
#' and the like). Returns the previous values invisibly, so a caller can
#' restore them (the [options()] convention).
#'
#' @param x A [blockr_theme()].
#' @return Previous option values, invisibly.
#' @export
apply_theme_options <- function(x) {
  stopifnot(is_blockr_theme(x))
  # The theme itself is recorded too, so a consumer can call theme_palette()
  # with no arguments instead of having to be handed the object. Renderers run
  # deep inside a board's expressions, where threading a theme through would
  # mean touching every block signature.
  exhibits <- if (length(x$exhibits)) {
    stats::setNames(x$exhibits, paste0("blockr.viz.", names(x$exhibits)))
  } else {
    list()
  }
  invisible(options(c(exhibits, list(blockr.theme.current = x))))
}

# --- palettes -------------------------------------------------------------

PALETTE_ROLES <- c(
  "categorical", "identity", "sequential", "diverging", "bands"
)

# Vanilla: what a board looks like with no theme applied. `bands` has no
# default -- blockr.viz keeps its own header-band behaviour when unset, so an
# untouched app renders exactly as before.
PALETTE_ROLE_DEFAULTS <- list(
  categorical = "Blockr",
  identity = "Many",
  sequential = "Blues",
  diverging = "Blue-Red 3",
  bands = NULL
)

#' The theme's palette for a colour role
#'
#' Resolves what colours to use for a given *job*, which is the vocabulary a
#' theme is written in: `categorical` for series identity, `identity` for
#' many-level pools (subject ids), `sequential` for magnitude, `diverging` for
#' polarity, `bands` for table header and emphasis fills.
#'
#' Consumers ask by role and never name a palette, so one theme entry
#' recolours every renderer that shares the role. With no theme applied the
#' blockr defaults answer, so a themeless board is unchanged.
#'
#' @param role One of `categorical`, `identity`, `sequential`, `diverging`,
#'   `bands`.
#' @param n Number of colours. `NULL` returns the palette's own length for a
#'   fixed set, and is an error for a ramp.
#' @param theme A [blockr_theme()]; defaults to the one [use_theme()] /
#'   [apply_theme_options()] last applied, or none.
#' @return A character vector of hex colours, or `NULL` when the role has no
#'   value and no default (`bands`).
#' @examples
#' theme_palette("categorical", 3)
#' theme_palette("sequential", 5, theme = theme_blockr())
#' @export
theme_palette <- function(role, n = NULL, theme = current_theme()) {
  role <- match.arg(role, PALETTE_ROLES)

  spec <- if (is_blockr_theme(theme)) theme$palettes[[role]]
  spec <- spec %||% PALETTE_ROLE_DEFAULTS[[role]]

  if (is.null(spec)) {
    return(NULL)
  }

  # A role may be answered with literal colours (`bands` usually is) rather
  # than a palette name.
  if (length(spec) > 1L || !is.null(names(spec))) {
    return(if (is.null(n)) spec else rep_len(spec, n))
  }

  defs <- if (is_blockr_theme(theme)) theme$palette_defs
  entry <- lookup_palette(spec, defs)

  if (!is.null(entry) && !is.null(entry$fn)) {
    if (is.null(n)) {
      stop("`n` is required: role `", role, "` resolves to ramp `", spec,
           "`.", call. = FALSE)
    }
    return(palette_ramp(n, spec, defs = defs))
  }

  if (identical(role, "sequential") || identical(role, "diverging")) {
    if (is.null(n)) {
      stop("`n` is required for role `", role, "`.", call. = FALSE)
    }
    return(palette_ramp(n, spec, defs = defs))
  }

  palette_colors(n, spec, defs = defs)
}

#' @rdname theme_palette
#' @export
current_theme <- function() {
  getOption("blockr.theme.current")
}

# --- templates ------------------------------------------------------------

#' Document-template pointers
#'
#' A theme's `templates` entry is resolved to a file path at render time.
#' `pkg_template()` points at a file in another package's `inst/` (resolved
#' via [system.file()], so it follows that package wherever it installs);
#' `local_template()` names a file contained in blockr.theme's own
#' `inst/templates`. A plain path string is also accepted.
#'
#' @param package Package whose `inst/` holds the template.
#' @param file Path of the template within `inst/` (for `local_template()`,
#'   within `inst/templates/`).
#' @return A classed pointer consumed by [theme_template()].
#' @export
pkg_template <- function(package, file) {
  structure(list(package = package, file = file), class = "blockr_template_ref")
}

#' @rdname pkg_template
#' @export
local_template <- function(file) {
  pkg_template("blockr.theme", file.path("templates", file))
}

#' Resolve a theme's template for a format
#'
#' @param x A [blockr_theme()].
#' @param format Template format key (`"pptx"`, `"docx"`, `"quarto"`, ...).
#' @return An absolute file path, or `NULL` when the theme has no template
#'   for that format (or the pointed-at package/file is unavailable).
#' @export
theme_template <- function(x, format = "pptx") {
  stopifnot(is_blockr_theme(x))
  resolve_template_ref(x$templates[[format]])
}

resolve_template_ref <- function(t) {
  if (is.null(t)) {
    return(NULL)
  }
  if (inherits(t, "blockr_template_ref")) {
    if (!requireNamespace(t$package, quietly = TRUE)) {
      return(NULL)
    }
    path <- system.file(t$file, package = t$package)
    return(if (nzchar(path)) path else NULL)
  }
  if (is.character(t) && length(t) == 1L && file.exists(t)) {
    return(t)
  }
  NULL
}

# --- scales ---------------------------------------------------------------

#' A theme's scale map as a board option
#'
#' Convenience wrapper: if the theme carries a scale map, build the board
#' option for it (see [new_scale_map_option()]); otherwise `NULL`.
#'
#' @param x A [blockr_theme()].
#' @param ... Forwarded to [new_scale_map_option()].
#' @return A `board_option`, or `NULL`.
#' @export
theme_scale_map_option <- function(x, ...) {
  stopifnot(is_blockr_theme(x))
  if (is.null(x$scales)) {
    return(NULL)
  }
  new_scale_map_option(map = x$scales, ...)
}

# --- one-call wiring ------------------------------------------------------

#' Apply a theme and return its head tags
#'
#' The common app-side call: sets the exhibit options as a side effect and
#' returns the chrome `<style>` to inject into the UI head. The scale map and
#' templates are wired separately (they belong to the board and the render
#' call respectively) via [theme_scale_map_option()] and [theme_template()].
#'
#' @param x A [blockr_theme()].
#' @param selector Passed to [theme_head()].
#' @return The theme's head tags (see [theme_head()]).
#' @export
use_theme <- function(x, selector = ":root") {
  apply_theme_options(x)
  theme_head(x, selector)
}
