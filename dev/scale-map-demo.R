# Scale map demo — study-wide level colors across views.
#
# ONE definition ("Xanomeline High Dose is dark red") drives every view that
# colors by the variable: the stacked bar colors via the `color` role, the
# pie via the `group` role, and both read the same board option. The map is
# editable live in the board options sidebar ("Scales" section) and
# serializes with the board JSON. Drop the scale_map option (or uninstall
# blockr.theme) and the charts fall back to their standard palette.
#
# Run from workspace root:
#   Rscript blockr.theme/dev/scale-map-demo.R

options(blockr.dock_is_locked = FALSE)
options(shiny.port = 3838L, shiny.host = "0.0.0.0")

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dm")
pkgload::load_all("blockr.theme")
pkgload::load_all("blockr.bi")

# A study map: fixed colors for AESEV and the treatment arms, a board
# palette as the auto-assignment pool for everything merely registered.
study_scale_map <- new_scale_map(
  scale_binding(
    "AESEV",
    color = c(MILD = "#CA8A04", MODERATE = "#D97706", SEVERE = "#DC2626")
  ),
  scale_binding("TRT01A", color = c(
    "Placebo"              = "#6D8196",
    "Xanomeline Low Dose"  = "#E69F00",
    "Xanomeline High Dose" = "#8b0000",
    "Screen Failure"       = "#bdbdbd"
  )),
  scale_binding("RACE"),
  palette = c("#0072B2", "#D55E00", "#F0E442", "#009E73", "#56B4E9")
)

board <- new_dock_board(
  blocks = c(
    data = new_dm_example_block(dataset = "pharmaverseadam",
      block_name = "ADaM data"),
    adsl = new_dm_pull_block(table = "adsl", block_name = "Pull adsl"),
    arm_by_race = new_drilldown_chart_block(
      chart_type = "bar", group = "RACE", color = "TRT01A",
      block_name = "Race by arm (stacked bar — color role)"),
    arm_pie = new_drilldown_chart_block(
      chart_type = "pie", group = "TRT01A",
      block_name = "Arm split (pie — group role)"),
    race_pie = new_drilldown_chart_block(
      chart_type = "pie", group = "RACE",
      block_name = "Race split (auto colors from board palette)")
  ),
  links = links(
    from = c("data", "adsl", "adsl", "adsl"),
    to = c("adsl", "arm_by_race", "arm_pie", "race_pie")
  ),
  layouts = list(
    Demo = dock_layout(c("arm_by_race", "arm_pie", "race_pie"))
  ),
  options = c(
    dock_board_options(),
    new_board_options(new_scale_map_option(study_scale_map))
  ),
  active = "Demo"
)

serve(board)
