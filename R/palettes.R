# Named colour palettes, addressed the way base R addresses them.
#
# The interface deliberately mirrors grDevices: `palette_colors(n, palette)`
# for fixed qualitative sets (cf. grDevices::palette.colors) and
# `palette_ramp(n, palette)` for interpolating ramps (cf.
# grDevices::hcl.colors), with `palette_pals()` listing what is available (cf.
# palette.pals() / hcl.pals()). Three reasons, in order of weight:
#
#   1. A palette spec is then a NAME plus an integer, which serialises with a
#      board. A closure does not. Everything downstream (the scale map's
#      binding metadata, the sidebar select) stores the string.
#   2. Base already splits fixed sets from interpolating ramps, and that split
#      is load-bearing here: palette.colors() is prefix-stable
#      (palette.colors(3) == palette.colors(8)[1:3]) while hcl.colors() is not
#      (it re-interpolates the whole range for each n, so every colour moves
#      when the level count changes). Stable-under-filtering assignment needs
#      the former.
#   3. `recycle = FALSE` is base's default: too many levels warns and
#      truncates rather than cycling a hue onto a second series.
#
# Unknown names fall through to grDevices, so every base palette stays
# reachable by its own name and this package only adds entries.

# --- the entries this package contributes ---------------------------------

# Okabe-Ito with black and yellow dropped: at the light-mode surface those two
# sit outside the usable lightness band (yellow #F0E442 at L 0.90). The
# remaining six pass the discriminability checks as a set; the order is the
# one theme_blockr() has always shipped.
PAL_BLOCKR <- c(
  "#0072b2", "#e69f00", "#009e73", "#cc79a7", "#56b4e9", "#d55e00"
)

# Identity pool for many-level variables (patient ids and the like), where the
# job is a stable, visually separated colour per entity rather than a legend
# anyone reads. Built by farthest-point packing over the sRGB gamut restricted
# to the usable lightness band and a chroma floor, maximising the minimum
# OKLab separation (normal vision and protan/deutan simulation together) at
# each step. Consequences worth knowing before reaching for it:
#
#   * Ordered, so a prefix is the best-separated subset: the worst pair within
#     the first 6 is dE 22.3, within 14 it is 14.0, within 50 it is 6.8, and
#     across all 240 it is 2.9.
#   * Only ~14 mutually distinguishable colours exist at all, so beyond that
#     these are wallpaper. Two of 240 entities CAN look alike; identity has to
#     come from interaction (hover, tooltip, drill), not from the colour.
#   * Chosen for separation, not for looks. For anything with a legend use
#     PAL_BLOCKR instead.
PAL_MANY <- c(
  "#0072B2", "#36d800", "#de0000", "#e48afc", "#7800fc", "#841e6c",
  "#42b4a8", "#24783c", "#c68a4e", "#d20096", "#8478fc", "#1e36c6",
  "#fc7890", "#961206", "#b4606c", "#fc18de", "#6030a2", "#2496d8",
  "#a878b4", "#b4ba6c", "#1ea800", "#fc0c7e", "#ae42c0", "#ae306c",
  "#fc660c", "#846000", "#d896c6", "#36b4fc", "#d25496", "#7e5a9c",
  "#843c4e", "#5a42d8", "#9c42fc", "#5478d8", "#72c042", "#ba4236",
  "#78ae72", "#f660e4", "#1e8a72", "#a2008a", "#0cd2a8", "#e4365a",
  "#4e00e4", "#42c6de", "#6c4284", "#b47efc", "#424eb4", "#fc5496",
  "#608a48", "#de6054", "#ea8a72", "#d242c6", "#964884", "#cc54f0",
  "#cc7ec0", "#a2063c", "#fc3c00", "#d20ce4", "#0090a8", "#963c24",
  "#c62a54", "#7e3ca2", "#b4ae00", "#ae7236", "#7e78c6", "#8ac68a",
  "#6c5ad2", "#6c00b4", "#7e3cc6", "#9612b4", "#f60cae", "#c67890",
  "#e48aa2", "#dea24e", "#069c7e", "#840096", "#609ce4", "#963060",
  "#a24e60", "#9648ae", "#de6cd2", "#f66c66", "#ea48c6", "#ea5a72",
  "#96005a", "#127eea", "#ba1236", "#cc7eea", "#9c9636", "#8a00ea",
  "#2ac084", "#24a860", "#ae0072", "#006624", "#ba66a2", "#cc607e",
  "#e47836", "#f07eb4", "#9660a2", "#a2a2f6", "#0000f6", "#4296fc",
  "#902a96", "#8436f6", "#3ca2cc", "#7e3c72", "#c01e7e", "#6c7e00",
  "#cc3c8a", "#9c60d2", "#4236d2", "#a84296", "#00c0c6", "#ae180c",
  "#ea006c", "#ae0054", "#ea90d8", "#6cbaea", "#f60048", "#9c6cba",
  "#06c0a2", "#24a896", "#4eae84", "#48ccba", "#c60c00", "#72307e",
  "#d80c3c", "#ae72d8", "#5aae5a", "#42cc5a", "#ae48ea", "#a290f0",
  "#005aa2", "#9642cc", "#b42aa8", "#d80060", "#d86ca2", "#669606",
  "#5460ba", "#ba5478", "#8a2448", "#a25aba", "#54cc78", "#ba9c48",
  "#a82a90", "#a21eea", "#ae3054", "#963c42", "#301ed8", "#66ba72",
  "#f07ed2", "#009696", "#de60ba", "#605aa2", "#2a548a", "#008a5a",
  "#900078", "#ea6cba", "#6c72fc", "#9c247e", "#a2543c", "#5a2ade",
  "#5484f0", "#8a6cae", "#962a2a", "#cc4e48", "#a84248", "#f690a2",
  "#9c5a8a", "#96c024", "#904e24", "#ea24fc", "#3c42c0", "#7e3060",
  "#30965a", "#3672de", "#7e3636", "#cc90d8", "#b4a22a", "#0c785a",
  "#9030ae", "#de7884", "#4260de", "#a836d8", "#5aba96", "#90b45a",
  "#36a2f0", "#66c69c", "#fc2a66", "#ea663c", "#668acc", "#c05a2a",
  "#0cb496", "#9078e4", "#d28466", "#9054c6", "#c06c8a", "#549c60",
  "#965a2a", "#a23c78", "#fc2a30", "#005ac0", "#6c8af0", "#d82aae",
  "#9c126c", "#12903c", "#ba4ed2", "#ea54ae", "#36cc2a", "#00a8b4",
  "#903c9c", "#f65acc", "#6c24ae", "#c0306c", "#962a48", "#3cc0fc",
  "#e41890", "#a2121e", "#1884a8", "#de2a78", "#d86cfc", "#30b4de",
  "#d8ae48", "#96721e", "#de300c", "#a284d8", "#de4ea2", "#6066f0",
  "#a80ca8", "#ae9612", "#008ac6", "#ea428a", "#ba6c48", "#c05aba",
  "#84368a", "#de84c0", "#a8607e", "#664296", "#00d290", "#00a2c0"
)

# Single-hue sequential ramps. Base's "<hue> 3" ramps run all the way to
# near-white, where the light end vanishes against the page (contrast 1.03:1)
# and the hue reading becomes meaningless. Sampling the darker half instead
# keeps the light end visible at any n and holds the hue spread under 10deg.
# Practical ceiling is about six steps: past that adjacent steps stop
# separating in lightness, and a continuous fill is the right tool anyway.
PAL_SEQUENTIAL <- c(
  Blues = "Blues 3", Purples = "Purples 3", Greens = "Greens 3",
  Reds = "Reds 3", Oranges = "Oranges", Greys = "Grays"
)

seq_ramp <- function(base_pal) {
  # Forced here: the closure is built in a loop over PAL_SEQUENTIAL, and an
  # unforced promise would resolve against the loop variable's FINAL value,
  # making every ramp the last one.
  force(base_pal)
  function(n) {
    full <- rev(grDevices::hcl.colors(101L, base_pal))
    full[round(seq(0.5, 1, length.out = n) * 100) + 1L]
  }
}

# name -> list(type =, colors = | fn =). `fn` takes n and returns n colours.
palette_registry <- function() {
  reg <- list(
    Blockr = list(type = "qualitative", colors = PAL_BLOCKR),
    Many = list(type = "identity", colors = PAL_MANY)
  )
  for (nm in names(PAL_SEQUENTIAL)) {
    reg[[nm]] <- list(type = "sequential", fn = seq_ramp(PAL_SEQUENTIAL[[nm]]))
  }
  reg
}

# --- lookup ---------------------------------------------------------------

# Entries contributed by a theme win over this package's, which win over
# grDevices -- the same later-wins-by-name rule new_scale_map() uses for
# bindings. `defs` is a theme's `palette_defs`.
lookup_palette <- function(palette, defs = NULL) {
  if (!is.null(defs) && palette %in% names(defs)) {
    return(as_palette_entry(defs[[palette]]))
  }
  reg <- palette_registry()
  if (palette %in% names(reg)) {
    return(reg[[palette]])
  }
  NULL
}

# A theme contributes either a plain colour vector or a function(n).
as_palette_entry <- function(x) {
  if (is.function(x)) {
    return(list(type = "sequential", fn = x))
  }
  if (is.character(x) && length(x)) {
    return(list(type = "qualitative", colors = unname(x), names = names(x)))
  }
  stop("A palette definition must be a character vector or a function(n).",
       call. = FALSE)
}

#' Named colour palettes
#'
#' `palette_colors()` returns a fixed qualitative set and `palette_ramp()` an
#' interpolating one, mirroring [grDevices::palette.colors()] and
#' [grDevices::hcl.colors()] in both signature and behaviour. Names this
#' package does not define fall through to `grDevices`, so every base palette
#' stays reachable; `palette_pals()` lists what is available.
#'
#' The split matters beyond familiarity. `palette_colors()` is prefix-stable
#' (asking for three colours gives the first three of eight), so a level keeps
#' its colour when the set of levels shown changes. `palette_ramp()`
#' re-interpolates for each `n`, so every colour moves. Use the former for
#' identity, the latter for magnitude.
#'
#' Palettes this package adds:
#' \describe{
#'   \item{`"Blockr"`}{Six-colour qualitative set (Okabe-Ito without black and
#'     yellow, which fall outside the usable lightness band).}
#'   \item{`"Many"`}{240 colours ordered by farthest-point packing, for
#'     many-level identity variables such as subject ids. A prefix is the
#'     best-separated subset. Past roughly 14 entries these are wallpaper:
#'     identity has to come from interaction, not from the colour.}
#'   \item{`"Blues"`, `"Purples"`, `"Greens"`, `"Reds"`, `"Oranges"`,
#'     `"Greys"`}{Single-hue sequential ramps over the darker half of the
#'     corresponding base ramp, so the light end stays visible against the
#'     page. Good to about six steps.}
#' }
#'
#' @param n Number of colours. For `palette_colors()`, `NULL` returns the
#'   whole palette; asking for more than it holds warns and truncates unless
#'   `recycle = TRUE`.
#' @param palette Palette name (see `palette_pals()`).
#' @param alpha Opacity in `[0, 1]`; passed through to `grDevices`.
#' @param recycle Cycle the palette when `n` exceeds its length? Defaults to
#'   `FALSE`, matching base: reusing a hue for a second series makes two
#'   things look like one.
#' @param names Return the palette with its level names, when it has any.
#' @param rev Reverse the ramp (dark to light).
#' @param defs Optional named list of extra definitions (a theme's
#'   `palette_defs`), consulted before this package's registry.
#'
#' @return A character vector of hex colours. `palette_pals()` returns a
#'   character vector of palette names.
#'
#' @examples
#' palette_colors(3)                       # first three of the Blockr set
#' identical(palette_colors(3), palette_colors(6)[1:3])   # prefix-stable
#' palette_ramp(5, "Blues")
#' palette_colors(4, "Okabe-Ito")          # falls through to grDevices
#' head(palette_pals("sequential"))
#' @export
palette_colors <- function(n = NULL, palette = "Blockr", alpha = NULL,
                           recycle = FALSE, names = FALSE, defs = NULL) {
  stopifnot(is.character(palette), length(palette) == 1L)

  entry <- lookup_palette(palette, defs)

  if (is.null(entry)) {
    args <- list(n = n, palette = palette, recycle = recycle, names = names)
    if (!is.null(alpha)) {
      args$alpha <- alpha
    }
    return(do.call(grDevices::palette.colors, args))
  }

  cols <- entry$colors
  if (is.null(cols)) {
    # A ramp asked for as a fixed set: materialise n steps of it.
    if (is.null(n)) {
      stop("`n` is required for ramp palette `", palette, "`.", call. = FALSE)
    }
    cols <- entry$fn(n)
  } else if (!is.null(n)) {
    if (n > length(cols) && !recycle) {
      warning("'n' set to ", length(cols),
              ", the maximum available for ", palette, " palette",
              call. = FALSE)
      n <- length(cols)
    }
    cols <- rep_len(cols, n)
  }

  if (isTRUE(names) && !is.null(entry$names)) {
    stats::setNames(cols, rep_len(entry$names, length(cols)))
  } else {
    apply_alpha(cols, alpha)
  }
}

#' @rdname palette_colors
#' @export
palette_ramp <- function(n, palette = "Blues", alpha = NULL, rev = FALSE,
                         defs = NULL) {
  stopifnot(is.character(palette), length(palette) == 1L)

  entry <- lookup_palette(palette, defs)

  cols <- if (is.null(entry)) {
    grDevices::hcl.colors(n, palette, rev = rev)
  } else if (!is.null(entry$fn)) {
    out <- entry$fn(n)
    if (rev) rev(out) else out
  } else {
    out <- rep_len(entry$colors, n)
    if (rev) rev(out) else out
  }

  apply_alpha(cols, alpha)
}

apply_alpha <- function(cols, alpha) {
  if (is.null(alpha)) {
    return(cols)
  }
  rgb <- grDevices::col2rgb(cols)
  grDevices::rgb(rgb[1L, ], rgb[2L, ], rgb[3L, ],
                 alpha = alpha * 255, maxColorValue = 255)
}

#' @param type Restrict to `"qualitative"`, `"sequential"`, `"diverging"` or
#'   `"identity"`; `NULL` lists everything.
#' @rdname palette_colors
#' @export
palette_pals <- function(type = NULL, defs = NULL) {
  reg <- palette_registry()

  own <- vapply(reg, `[[`, character(1L), "type")
  mine <- if (is.null(type)) names(own) else names(own)[own == type]

  theirs <- if (!is.null(defs)) {
    tp <- vapply(defs, function(x) as_palette_entry(x)$type, character(1L))
    if (is.null(type)) names(tp) else names(tp)[tp == type]
  }

  base_pals <- switch(
    type %||% "all",
    qualitative = grDevices::palette.pals(),
    sequential = grDevices::hcl.pals("sequential"),
    diverging = grDevices::hcl.pals("diverging"),
    identity = character(),
    all = c(grDevices::palette.pals(), grDevices::hcl.pals())
  )

  unique(c(theirs, mine, base_pals))
}
