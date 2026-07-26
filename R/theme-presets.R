# Built-in theme. This OPEN package carries only the engine and its own open
# default:
#   theme_blockr() -- the OPEN blockr house style, font + deck CONTAINED in
#                     this package so it ships freely. Empty chrome on purpose:
#                     the blockr design-system default IS the blockr look, so
#                     "vanilla" and this theme are the same chrome. It exists
#                     to (a) name that, (b) carry a shareable font + deck +
#                     palette.
# Every OTHER theme -- client (BMS) or company (cynkra) -- lives in its own
# package built on this engine (e.g. blockr.bms::theme_bms(), vendored in
# blockr.sandbox), so an org's name and assets never enter the open package.
#
# The exhibit `ft_header_bg` is a stub grey then a short band cycle; the scale
# `palette` is the series pool renderers fall back to.

#' The open blockr theme
#'
#' `theme_blockr()` is the open house style (freely usable and remixable; its
#' font and deck template are shipped in this package). Every other theme --
#' company or client -- lives in its own package built on this engine (for
#' example `blockr.bms::theme_bms()`), never here.
#'
#' @return A [blockr_theme()].
#' @seealso [blockr_theme()], [use_theme()]
#' @export
theme_blockr <- function() {
  blockr_theme(
    name = "blockr",
    description = paste(
      "Open blockr house theme: design-system chrome, colourblind-safe",
      "(Okabe-Ito) series palette, shareable blockr deck. Free to remix."
    ),
    # Empty on purpose: the blockr design-system tokens ARE the blockr look,
    # so this theme keeps vanilla chrome and contributes the font, deck and
    # palette.
    chrome = list(),
    # Inter (SIL OFL) is the open house face -- bundled and inlined, so the
    # font travels with the theme and needs no install or network.
    webfont = bundled_font("Inter", "inter-latin.woff2"),
    exhibits = list(
      ft_header_bg = c(.stub = "#EEEEEE", "#0072B2", "#009E73", "#E69F00")
    ),
    # The open theme names the roles it means rather than restating hexes: the
    # defaults ARE the blockr palettes, so this documents the mapping and gives
    # client themes a worked example of the slot to override.
    palettes = list(
      categorical = "Blockr",
      identity = "Many",
      sequential = "Blues",
      diverging = "Blue-Red 3",
      bands = c(.stub = "#EEEEEE", "#0072B2", "#009E73", "#E69F00")
    ),
    scales = new_scale_map(palette = palette_colors(palette = "Blockr")),
    templates = list(
      # CONTAINED: shipped in this package's inst/templates, so the open
      # theme is self-sufficient (no client package needed to render).
      pptx = local_template("blockr-template.pptx")
    )
  )
}
