# blockr_theme() demo: the same outline deck under a chosen house theme.
#
#   BLOCKR_THEME=vanilla Rscript blockr.theme/dev/theme-demo.R [port]
#   BLOCKR_THEME=blockr  Rscript blockr.theme/dev/theme-demo.R
#   BLOCKR_THEME=bms     Rscript blockr.theme/dev/theme-demo.R  (loads blockr.bms)
#
# What each theme touches on screen:
#   * chrome   -> the toolbar switcher, the Code/Output toggle, buttons and
#                 focus rings recolour (via the injected :root <style>).
#   * exhibits -> in Output mode the flextable header bands take the theme's
#                 ft_header_bg palette.
# "vanilla" applies no theme, so it is the blockr design-system default --
# which is exactly what theme_blockr() keeps (empty chrome).

port <- local({
  a <- commandArgs(trailingOnly = TRUE)
  if (length(a)) as.integer(a[[1L]]) else as.integer(Sys.getenv("BLOCKR_PORT", "3838"))
})
options(shiny.port = port, shiny.host = "0.0.0.0")
options(blockr.dock_is_locked = FALSE)
options(blockr.background_construction_delay = 0)

root <- "."
deps <- c("blockr.core", "blockr.dag", "blockr.dock", "blockr.viz",
          "blockr.outline", "blockr.theme")
for (d in deps) {
  pkgload::load_all(file.path(root, d), helpers = FALSE,
                    attach_testthat = FALSE, export_all = FALSE)
}

which <- tolower(Sys.getenv("BLOCKR_THEME", "blockr"))

# BMS is a CLIENT theme: it does not live in blockr.theme but in blockr.bms,
# which is vendored inside blockr.sandbox and load_all'd (a real namespace, so
# any blocks it later adds survive board restore -- see app.R). Load it the
# same way app.R does before asking it for the theme.
if (identical(which, "bms")) {
  bms_src <- file.path(root, "blockr.sandbox", "inst", "blockr.bms")
  pkgload::load_all(bms_src, helpers = FALSE, attach_testthat = FALSE,
                    export_all = FALSE)
}

theme <- switch(
  which,
  vanilla = NULL,
  blockr  = blockr.theme::theme_blockr(),
  bms     = blockr.bms::theme_bms(),
  stop("BLOCKR_THEME must be vanilla|blockr|bms")
)

# Apply the theme: set exhibit options now, inject the chrome <style> into
# the served page head. apply_theme_options + a blockr_app_ui wrap is the
# whole integration surface (see reference_blockr_app_ui_injection_seam).
if (!is.null(theme)) {
  blockr.theme::apply_theme_options(theme)
  head_tags <- blockr.theme::theme_head(theme)
  local({
    dock_ui <- getS3method("blockr_app_ui", "dock_board")
    registerS3method(
      "blockr_app_ui", "dock_board",
      function(id, x, plugins, options, ...) {
        dock_ui(id, x, plugins, options, ..., head_tags)
      },
      envir = asNamespace("blockr.core")
    )
  })
}

message("Theme: ", which, "  |  http://127.0.0.1:", port, "/")

board <- new_dock_board(
  blocks = c(
    data = new_dataset_block("iris", block_name = "Iris data"),
    tbl = blockr.viz::new_summary_table_block(
      vars = c("Sepal.Length", "Sepal.Width", "Petal.Length"),
      by = "Species", block_name = "Sepal summary"
    )
  ),
  links = links(from = "data", to = "tbl"),
  stacks = stacks(
    deck = new_dock_stack(c("data", "tbl"), name = "Deck", color = "#7c3aed")
  ),
  extensions = list(
    blockr.dag::new_dag_extension(),
    blockr.outline::new_outline_extension(
      annotations = list(
        data = list(report = FALSE),
        tbl = list(
          description = "Baseline sepal measures by species -- the exhibit whose header bands the theme colours."
        )
      ),
      stack_annotations = list(deck = list(description = "A one-exhibit deck.")),
      title = paste0(tools::toTitleCase(which), " deck")
    )
  )
)

serve(board)
