# Patient colours demo: one USUBJID binding with a 12-colour pool drives the
# line colour of every chart that colours by patient. The same patient has
# the same colour in the 40-patient chart and the 5-patient chart. Pin a
# patient in the board options sidebar ("Scales", USUBJID): the level field
# suggests the patients the charts have shown.
#
# Run from the workspace root, with the theme and viz worktrees:
#   Rscript _worktrees/theme-patient-colours/dev/patient-colours-demo.R [port]

.port <- as.integer(c(commandArgs(TRUE), Sys.getenv("BLOCKR_PORT"), "4730")[1])
options(shiny.port = .port, shiny.host = "127.0.0.1",
        blockr.dock_is_locked = FALSE)

pkgload::load_all("blockr.core")
pkgload::load_all(Sys.getenv("BLOCKR_UI", "/private/tmp/claude-501/viztable/ui"))
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dag")
pkgload::load_all("_worktrees/theme-patient-colours")
pkgload::load_all("_worktrees/viz-patient-colours")

alt <- safetyData::adam_adlbc
alt <- alt[alt$PARAMCD == "ALT" & alt$AVISITN %in% c(0, 2, 4, 6, 8, 12, 16, 20, 24), ]
full <- names(which(table(alt$USUBJID) == 9L))
ids40 <- utils::head(full, 40L)
lab40 <- alt[alt$USUBJID %in% ids40, c("USUBJID", "AVISITN", "AVAL")]
names(lab40) <- c("USUBJID", "WEEK", "ALT")
lab5 <- lab40[lab40$USUBJID %in% ids40[c(3, 9, 14, 22, 31)], ]

map <- new_scale_map(
  scale_binding("USUBJID", color = palette_colors(12, "Many"))
)

board <- new_dock_board(
  blocks = c(
    lab40 = new_static_block(lab40, block_name = "ALT, 40 patients"),
    lab5 = new_static_block(lab5, block_name = "ALT, 5 patients"),
    chart40 = new_chart_block(
      type = "line", x = "WEEK", y = "ALT", series = "USUBJID",
      color = "USUBJID", block_name = "ALT over time, 40 patients"),
    chart5 = new_chart_block(
      type = "line", x = "WEEK", y = "ALT", series = "USUBJID",
      color = "USUBJID", block_name = "ALT over time, 5 patients")
  ),
  links = c(
    new_link("lab40", "chart40", "data"),
    new_link("lab5", "chart5", "data")
  ),
  options = c(
    dock_board_options(),
    new_board_options(new_scale_map_option(map))
  ),
  extensions = list(blockr.dag::new_dag_extension())
)

serve(board)
