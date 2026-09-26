# Sidebar editor for the "scale_map" board option. v1 scope (see
# blockr.design/open/cdex-attribute-map): per-binding rows of level + color
# swatch (recolor, add/remove levels, add/remove bindings); pools, shapes and
# linetypes are shown read-only. Edits write the option value through
# set_board_option_value(), so they serialize with the board and consumers
# re-render reactively.
#
# Mechanics: the option UI slot is a container div; the server re-renders its
# content via removeUI()/insertUI() whenever the map *structure* changes
# (color-only changes update the colour inputs in place). All buttons funnel
# through a single "sm_action" input via Shiny.setInputValue, so there are no
# dynamic observers to manage.

scale_map_editor_dep <- function() {
  htmltools::htmlDependency(
    name = "blockr-theme-scale-map-editor",
    version = utils::packageVersion("blockr.theme"),
    src = system.file(package = "blockr.theme"),
    script = "js/scale-map-editor.js",
    stylesheet = "css/scale-map-editor.css",
    all_files = FALSE
  )
}

# The editor is a page of the board options sidebar (blockr.dock); it asks
# the sidebar for more width while it is open (`data-blockr-page-width`).
scale_map_editor_ui <- function(id) {
  htmltools::tagList(
    scale_map_editor_dep(),
    htmltools::div(
      id = shiny::NS(id, "sm_editor"),
      class = "bsm-editor",
      `data-blockr-page-width` = "480"
    )
  )
}

# Thin icons, drawn like the design system's small icons.
bsm_icons <- list(
  chev = paste0(
    '<svg width="12" height="12" viewBox="0 0 12 12" fill="none" ',
    'stroke="currentColor" stroke-width="1.25" stroke-linecap="round" ',
    'stroke-linejoin="round" aria-hidden="true"><path d="M4.5 3l3 3-3 3">',
    '</path></svg>'
  ),
  x = paste0(
    '<svg width="10" height="10" viewBox="0 0 10 10" fill="none" ',
    'stroke="currentColor" stroke-width="1" stroke-linecap="round" ',
    'aria-hidden="true"><path d="M2.5 2.5l5 5M7.5 2.5l-5 5"></path></svg>'
  ),
  plus = paste0(
    '<svg width="12" height="12" viewBox="0 0 16 16" fill="none" ',
    'stroke="currentColor" stroke-width="1.25" stroke-linecap="round" ',
    'aria-hidden="true"><path d="M8 3v10M3 8h10"></path></svg>'
  )
)

scale_map_editor_server <- function(board, ..., session) {
  ns <- session$ns

  st <- new.env(parent = emptyenv())
  st$gen <- 0L
  st$registry <- list() # raw input id -> list(var, level)
  st$rendered_sig <- NULL
  # Bumped on every render. The colour-scanner observer reads it FIRST so a
  # re-render (new generation of input ids) invalidates the observer and it
  # re-subscribes to the new inputs — otherwise its reactive deps stay on
  # the previous generation's ids, which never change again, and swatch
  # edits after any add/remove go unheard.
  st$render_count <- shiny::reactiveVal(0L)

  current_map <- function() {
    as_scale_map(
      blockr.core::get_board_option_or_null("scale_map", session)
    ) %||% new_scale_map()
  }

  write_map <- function(map) {
    blockr.core::set_board_option_value("scale_map", map, board$board, session)
  }

  structure_sig <- function(map) {
    rlang::hash(lapply(unclass(map), function(binding) {
      lapply(binding, function(spec) {
        names(spec) %||% paste0("pool", length(spec))
      })
    }))
  }

  js_str <- function(x) {
    as.character(jsonlite::toJSON(x, auto_unbox = TRUE))
  }

  # Every action re-renders the editor, which would drop the sidebar's scroll
  # offset; save it here, while the current nodes are still on screen.
  funnel_btn <- function(label, payload_js, class = "bsm-rm", title = NULL) {
    htmltools::tags$button(
      type = "button",
      class = class,
      title = title,
      `aria-label` = title,
      onclick = sprintf(
        paste0(
          "event.preventDefault(); event.stopPropagation(); ",
          "window.blockrScaleMapEditor.saveScroll(%s); ",
          "Shiny.setInputValue(%s, %s, {priority: 'event'})"
        ),
        js_str(ns("sm_editor")), js_str(ns("sm_action")), payload_js
      ),
      label
    )
  }

  static_payload <- function(action, var, level = NULL) {
    entries <- c(
      sprintf("action:%s", js_str(action)),
      sprintf("var:%s", js_str(var)),
      if (!is.null(level)) sprintf("level:%s", js_str(level))
    )
    sprintf("{%s}", paste(entries, collapse = ","))
  }

  binding_tags <- function(var, binding, gen, bi) {
    color <- binding$color
    fixed <- if (!is.null(color) && !is.null(names(color))) color

    level_rows <- if (!is.null(fixed)) {
      lapply(seq_along(fixed), function(li) {
        lv <- names(fixed)[[li]]
        input_id <- sprintf("sm_c_%d_%d_%d", gen, bi, li)
        st$registry[[input_id]] <<- list(var = var, level = lv)
        htmltools::div(
          class = "bsm-row",
          htmltools::span(class = "bsm-level", title = lv, lv),
          colourpicker::colourInput(ns(input_id), NULL,
                                    value = unname(fixed[[li]])),
          funnel_btn(htmltools::HTML(bsm_icons$x),
                     static_payload("rmlev", var, lv),
                     title = "Remove level")
        )
      })
    }

    notes <- c(
      if (!is.null(color) && is.null(names(color))) {
        sprintf("pool of %d colors, auto-assigned", length(color))
      },
      if (is.null(color)) "auto colors (theme palette)",
      if (!is.null(binding$shape)) {
        sprintf("shapes: %s", paste(
          paste0(names(binding$shape), "=", binding$shape),
          collapse = ", "
        ))
      },
      if (!is.null(binding$linetype)) {
        sprintf("linetypes: %s", paste(
          paste0(names(binding$linetype), "=", binding$linetype),
          collapse = ", "
        ))
      }
    )

    lev_id <- sprintf("sm_nl_%d_%d", gen, bi)
    col_id <- sprintf("sm_nc_%d_%d", gen, bi)
    add_payload <- sprintf(
      paste0(
        "{action:'addlev', var:%s, ",
        "level:document.getElementById(%s).value, ",
        "color:document.getElementById(%s).value}"
      ),
      js_str(var), js_str(ns(lev_id)), js_str(ns(col_id))
    )

    count <- if (!is.null(fixed)) {
      n <- length(fixed)
      paste(n, if (n == 1L) "level" else "levels")
    } else if (!is.null(color)) {
      "colour pool"
    } else {
      "theme colours"
    }

    # Collapsed; which variables are open survives a re-render
    # (scale-map-editor.js).
    htmltools::tags$details(
      class = "bsm-binding",
      `data-var` = var,
      htmltools::tags$summary(
        htmltools::span(class = "bsm-chev", htmltools::HTML(bsm_icons$chev)),
        htmltools::span(class = "bsm-var", title = var, var),
        htmltools::span(class = "bsm-count", paste0("\u00b7 ", count)),
        funnel_btn(htmltools::HTML(bsm_icons$x),
                   static_payload("rmvar", var),
                   title = "Remove variable")
      ),
      level_rows,
      lapply(notes, function(n) htmltools::div(class = "bsm-note", n)),
      htmltools::div(
        class = "bsm-add",
        shiny::textInput(ns(lev_id), NULL, placeholder = "Add level"),
        colourpicker::colourInput(ns(col_id), NULL, value = "#888888"),
        funnel_btn(htmltools::HTML(bsm_icons$plus), add_payload,
                   class = "bsm-plus", title = "Add level")
      )
    )
  }

  render_editor <- function(map) {
    st$gen <- st$gen + 1L
    st$registry <- list()

    gen <- st$gen
    var_id <- sprintf("sm_nv_%d", gen)
    addvar_payload <- sprintf(
      "{action:'addvar', var:document.getElementById(%s).value}",
      js_str(ns(var_id))
    )

    # No "Scales" label: the sidebar page is titled by its category.
    content <- htmltools::tagList(
      if (length(map)) {
        lapply(seq_along(map), function(bi) {
          binding_tags(names(map)[[bi]], map[[bi]], gen, bi)
        })
      } else {
        htmltools::div(class = "bsm-empty", "No variables yet")
      },
      htmltools::div(
        class = "bsm-add bsm-addvar",
        shiny::textInput(ns(var_id), NULL, placeholder = "Variable name"),
        funnel_btn("Add variable", addvar_payload, class = "bsm-addvar-btn")
      )
    )

    shiny::removeUI(
      selector = sprintf("#%s > *", ns("sm_editor")),
      multiple = TRUE,
      immediate = TRUE,
      session = session
    )
    shiny::insertUI(
      selector = sprintf("#%s", ns("sm_editor")),
      where = "beforeEnd",
      ui = content,
      immediate = TRUE,
      session = session
    )

    # Queued, so it reaches the client after the immediate insert above. A
    # no-op unless a button saved an offset, which is what we want for renders
    # triggered from outside the editor (restore, assistant edits).
    session$sendCustomMessage("blockr.theme-scale-map-scroll", list(TRUE))

    st$rendered_sig <- structure_sig(map)
    st$render_count(shiny::isolate(st$render_count()) + 1L)
  }

  obs_value <- shiny::observe({
    map <- current_map()

    if (!identical(structure_sig(map), st$rendered_sig)) {
      render_editor(map)
      return()
    }

    # Structure unchanged: sync colour inputs that drifted (external edits,
    # e.g. assistant or restore).
    for (input_id in names(st$registry)) {
      entry <- st$registry[[input_id]]
      target <- map[[entry$var]][["color"]][[entry$level]]
      cur <- shiny::isolate(session$input[[input_id]])
      if (!is.null(target) && !is.null(cur) &&
            !identical(tolower(cur), tolower(target))) {
        colourpicker::updateColourInput(session, input_id, value = target)
      }
    }
  })

  obs_colors <- shiny::observe({
    st$render_count() # re-subscribe to the current generation's inputs
    map <- shiny::isolate(current_map())
    changed <- FALSE

    for (input_id in names(st$registry)) {
      entry <- st$registry[[input_id]]
      val <- session$input[[input_id]]
      cur <- map[[entry$var]][["color"]][[entry$level]]
      if (!is.null(val) && !is.null(cur) &&
            !identical(tolower(val), tolower(cur))) {
        map[[entry$var]][["color"]][[entry$level]] <- val
        changed <- TRUE
      }
    }

    if (changed) {
      write_map(as_scale_map(unclass(map)))
    }
  })

  obs_action <- shiny::observeEvent(session$input$sm_action, {
    act <- session$input$sm_action
    map <- unclass(current_map())
    var <- act$var %||% ""

    if (identical(act$action, "rmvar") && var %in% names(map)) {
      map[[var]] <- NULL
    } else if (identical(act$action, "rmlev") && var %in% names(map)) {
      color <- map[[var]][["color"]]
      color <- color[setdiff(names(color), act$level)]
      map[[var]][["color"]] <- if (length(color)) color
    } else if (identical(act$action, "addlev") && var %in% names(map)) {
      lv <- trimws(act$level %||% "")
      if (nzchar(lv)) {
        color <- map[[var]][["color"]]
        if (!is.null(color) && is.null(names(color))) {
          return() # pool channel: not editable in v1
        }
        color <- color[setdiff(names(color), lv)]
        map[[var]][["color"]] <- c(color, stats_setNames(act$color, lv))
      }
    } else if (identical(act$action, "addvar")) {
      var <- trimws(var)
      if (nzchar(var) && !var %in% names(map)) {
        map[[var]] <- list()
      }
    }

    new <- as_scale_map(map)
    write_map(new)
    render_editor(new)
  })

  list(obs_value, obs_colors, obs_action)
}

stats_setNames <- function(x, nm) {
  names(x) <- nm
  x
}
