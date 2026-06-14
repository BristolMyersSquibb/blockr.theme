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
# options(shiny.port = 3838L, shiny.host = "0.0.0.0")

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dm")
pkgload::load_all("blockr.theme")
pkgload::load_all("blockr.viz")
pkgload::load_all("blockr.ggplot")
pkgload::load_all("blockr.pharma")

# A study map: fixed colors for AESEV and the treatment arms, a board
# palette as the auto-assignment pool for everything merely registered.
# The AESEV pins are deliberately NON-standard (sky/violet/pink instead of
# the amber/orange/red defaults) so it is visible that the patient profile
# (overview + gantt) takes its severity colors from the map, not from its
# built-in constants.
study_scale_map <- new_scale_map(
  scale_binding(
    "AESEV",
    color = c(MILD = "#0EA5E9", MODERATE = "#8B5CF6", SEVERE = "#EC4899")
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
    arm_by_race = new_chart_block(
      chart_type = "bar", group = "RACE", color = "TRT01A",
      block_name = "Race by arm (stacked bar — color role)"),
    arm_pie = new_chart_block(
      chart_type = "pie", group = "TRT01A",
      block_name = "Arm split (pie — group role)"),
    race_pie = new_chart_block(
      chart_type = "pie", group = "RACE",
      block_name = "Race split (auto colors from board palette)"),
    # Same variable, different renderer: ggplot consumes the same board map
    # (fill reads the binding's `color` channel), so the arms match the
    # echarts charts hex-for-hex — one color language across both engines.
    arm_by_race_gg = new_ggplot_block(
      type = "bar", x = "RACE", fill = "TRT01A",
      block_name = "Race by arm (ggplot — same map)"),
    # Patient profile: overview + gantt both take their AE severity colors
    # from the map's AESEV binding (one resolution, injected into both vizs
    # by the block server). Filter to a patient that has a severe AE so all
    # three severities show.
    severe_patient = new_dm_filter_block(
      table = "adsl",
      state = list(
        conditions = list(list(
          type = "values", column = "USUBJID",
          values = list("01-701-1211"), mode = "include"
        )),
        operator = "&"
      ),
      block_name = "Pick a patient with a severe AE"),
    pt_profile = new_patient_profile_block(
      selected = c("patient_overview", "ae_gantt"),
      block_name = "Patient profile (overview + gantt — same map)")
  ),
  links = links(
    from = c("data", "adsl", "adsl", "adsl", "adsl",
             "data", "severe_patient"),
    to = c("adsl", "arm_by_race", "arm_pie", "race_pie", "arm_by_race_gg",
           "severe_patient", "pt_profile")
  ),
  layouts = list(
    Demo = dock_layout(
      c("arm_by_race", "arm_by_race_gg", "arm_pie", "race_pie",
        "pt_profile")
    )
  ),
  options = c(
    dock_board_options(),
    new_board_options(new_scale_map_option(study_scale_map))
  ),
  active = "Demo"
)

serve(board)
