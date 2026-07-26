test_that("palette_colors is prefix-stable and palette_ramp is not", {
  # The property the scale map depends on: fewer levels shown must not
  # repaint the ones that remain.
  expect_identical(palette_colors(3), palette_colors(6)[1:3])
  expect_identical(palette_colors(2, "Many"), palette_colors(50, "Many")[1:2])

  # A ramp re-interpolates, which is exactly why the two are separate
  # functions rather than one with a `type` argument.
  expect_false(
    identical(palette_ramp(3, "Blues"), palette_ramp(6, "Blues")[1:3])
  )
})

test_that("palette_colors truncates rather than cycling, like base", {
  expect_warning(
    cols <- palette_colors(10, "Blockr"),
    "maximum available"
  )
  expect_length(cols, 6L)

  expect_silent(cyc <- palette_colors(10, "Blockr", recycle = TRUE))
  expect_length(cyc, 10L)
  expect_identical(cyc[7:10], palette_colors(4))
})

test_that("unknown names fall through to grDevices", {
  expect_identical(
    unname(palette_colors(4, "Okabe-Ito")),
    unname(grDevices::palette.colors(4, "Okabe-Ito"))
  )
  expect_identical(
    palette_ramp(5, "Viridis"),
    grDevices::hcl.colors(5, "Viridis")
  )
})

test_that("sequential ramps keep their light end off the page", {
  # Base's own ramps run to near-white (contrast ~1.03:1 against the page);
  # ours sample the darker half so the first step stays visible at any n.
  lum <- function(h) {
    v <- grDevices::col2rgb(h)[, 1L] / 255
    v <- ifelse(v <= 0.04045, v / 12.92, ((v + 0.055) / 1.055)^2.4)
    sum(c(0.2126, 0.7152, 0.0722) * v)
  }
  contrast <- function(h) (lum("#fcfcfb") + 0.05) / (lum(h) + 0.05)

  for (n in c(3L, 5L, 6L)) {
    light_end <- palette_ramp(n, "Blues")[[1L]]
    expect_gt(contrast(light_end), 2)
  }

  # monotone light -> dark
  expect_true(all(diff(vapply(palette_ramp(5, "Greens"), lum, numeric(1))) < 0))

  # Each ramp is its own hue: the registry builds these closures in a loop,
  # and an unforced argument would collapse them all onto the last entry.
  ramps <- lapply(c("Blues", "Purples", "Greens", "Reds", "Oranges", "Greys"),
                  palette_ramp, n = 5)
  expect_length(unique(ramps), 6L)
  # rev flips it
  expect_identical(palette_ramp(4, "Blues", rev = TRUE),
                   rev(palette_ramp(4, "Blues")))
})

test_that("the Many pool is ordered best-separated-first", {
  # Every prefix is a valid palette, which is what lets one pool serve both a
  # six-level variable and a 200-patient one.
  expect_length(palette_colors(palette = "Many"), 240L)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", palette_colors(palette = "Many"))))
  expect_false(anyDuplicated(toupper(palette_colors(palette = "Many"))) > 0)
})

test_that("palette_pals lists ours plus the base sets", {
  qual <- palette_pals("qualitative")
  expect_true("Blockr" %in% qual)
  expect_true("Okabe-Ito" %in% qual)

  seqs <- palette_pals("sequential")
  expect_true(all(c("Blues", "Greens") %in% seqs))
  expect_true("Viridis" %in% seqs)

  expect_true("Many" %in% palette_pals("identity"))
})

test_that("a theme contributes definitions without touching the registry", {
  th <- blockr_theme(
    "client",
    palette_defs = list(
      "Client Series" = c("#be2bbb", "#33d6f1", "#fda97c", "#a59f9f"),
      "Client Purple" = function(n) {
        grDevices::colorRampPalette(c("#f7e9fb", "#8f1f8d"))(n)
      }
    ),
    palettes = list(categorical = "Client Series", sequential = "Client Purple")
  )

  expect_identical(theme_palette("categorical", 2, th),
                   c("#be2bbb", "#33d6f1"))
  expect_length(theme_palette("sequential", 5, th), 5L)
  expect_true("Client Series" %in% palette_pals("qualitative",
                                                defs = th$palette_defs))

  # No global state: the definition is invisible without the theme.
  expect_false("Client Series" %in% palette_pals("qualitative"))
  expect_identical(theme_palette("categorical", 2), palette_colors(2))
})

test_that("roles fall back to the blockr defaults", {
  bare <- blockr_theme("bare")
  expect_identical(theme_palette("categorical", 3, bare), palette_colors(3))
  expect_identical(theme_palette("identity", 3, bare),
                   palette_colors(3, "Many"))
  expect_length(theme_palette("diverging", 5, bare), 5L)
  # `bands` has no default, so an unthemed board keeps blockr.viz's own
  # header-band behaviour instead of gaining one.
  expect_null(theme_palette("bands", theme = bare))

  expect_error(theme_palette("nope"), "arg")
  expect_error(blockr_theme("t", palettes = list(colour = "Blockr")),
               "Unknown palette role")
  expect_error(blockr_theme("t", palette_defs = list(x = 1)),
               "character vector or a function")
})

test_that("theme_palette reads the applied theme when none is passed", {
  th <- blockr_theme(
    "applied",
    palette_defs = list(P = c("#111111", "#222222")),
    palettes = list(categorical = "P")
  )
  old <- apply_theme_options(th)
  on.exit(options(old), add = TRUE)

  expect_identical(current_theme()$name, "applied")
  expect_identical(theme_palette("categorical", 2), c("#111111", "#222222"))

  options(old)
  expect_null(current_theme())
  expect_identical(theme_palette("categorical", 2), palette_colors(2))
})

test_that("theme_blockr's roles resolve to the blockr palettes", {
  bl <- theme_blockr()
  expect_identical(theme_palette("categorical", 6, bl), palette_colors(6))
  expect_identical(theme_palette("bands", theme = bl), bl$exhibits$ft_header_bg)
  expect_identical(
    attr(bl$scales, "palette"), palette_colors(palette = "Blockr")
  )
})
