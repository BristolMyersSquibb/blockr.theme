test_that("blockr_theme validates its facets", {
  expect_error(blockr_theme(name = ""), "name")
  expect_error(blockr_theme("t", chrome = list("#fff")), "chrome")
  expect_error(blockr_theme("t", exhibits = list(1)), "exhibits")
  expect_error(blockr_theme("t", templates = list("x")), "templates")

  th <- blockr_theme("t")
  expect_true(is_blockr_theme(th))
  expect_null(th$scales)
})

test_that("theme_css emits token overrides, aliases and font scope", {
  th <- blockr_theme(
    "t",
    chrome = list(primary = "#123456", "blue-600" = "#654321"),
    font_family = "'Foo', sans-serif"
  )
  css <- theme_css(th)

  # alias -> semantic token, raw stem passes through
  expect_match(css, "--blockr-color-primary: #123456;", fixed = TRUE)
  expect_match(css, "--blockr-blue-600: #654321;", fixed = TRUE)
  # default selector and font rule
  expect_match(css, "^:root \\{")
  expect_match(css, "font-family: 'Foo', sans-serif;", fixed = TRUE)

  # a scoping selector reaches both the tokens and the font (Bootstrap var
  # + font-family) instead of :root / body
  scoped <- theme_css(th, selector = ".theme-t")
  expect_match(scoped, "^.theme-t \\{")
  expect_match(scoped, ".theme-t { --bs-body-font-family:", fixed = TRUE)
  expect_match(scoped, "font-family: 'Foo', sans-serif;", fixed = TRUE)
  # scoped output does not fall back to a bare body rule
  expect_false(grepl("body {", scoped, fixed = TRUE))
})

test_that("theme_css and theme_head are empty for a bare theme", {
  th <- blockr_theme("t")
  expect_identical(theme_css(th), "")
  expect_length(theme_head(th), 0L)
})

test_that("apply_theme_options sets blockr.viz.* and restores", {
  th <- blockr_theme("t", exhibits = list(ft_header_bg = c("#111", "#222")))
  old <- apply_theme_options(th)
  expect_identical(getOption("blockr.viz.ft_header_bg"), c("#111", "#222"))
  options(old)
  expect_null(getOption("blockr.viz.ft_header_bg"))
})

test_that("templates resolve from a pointer, a local file and a path", {
  # local_template resolves out of this package's inst/templates
  th <- theme_blockr()
  path <- theme_template(th, "pptx")
  expect_true(is.character(path) && file.exists(path))

  # absent format -> NULL
  expect_null(theme_template(th, "docx"))

  # pkg_template to a missing package -> NULL (soft, never errors)
  th2 <- blockr_theme(
    "t",
    templates = list(pptx = pkg_template("no.such.package.xyz", "x.pptx"))
  )
  expect_null(theme_template(th2, "pptx"))

  # a plain existing path is returned as-is
  tmp <- tempfile(fileext = ".pptx")
  file.create(tmp)
  th3 <- blockr_theme("t", templates = list(pptx = tmp))
  expect_identical(theme_template(th3, "pptx"), tmp)
})

test_that("theme_scale_map_option is built only when scales are present", {
  expect_null(theme_scale_map_option(blockr_theme("t")))
  opt <- theme_scale_map_option(theme_blockr())
  expect_s3_class(opt, "board_option")
})

test_that("a bundled webfont inlines as an @font-face data URI", {
  th <- blockr_theme("t", webfont = bundled_font("Inter", "inter-latin.woff2"))
  # family auto-derived from the webfont
  expect_match(th$font_family, "^'Inter',")
  css <- theme_css(th)
  expect_match(css, "@font-face", fixed = TRUE)
  expect_match(css, "src: url(data:font/woff2;base64,", fixed = TRUE)
  # the face leads, before the family is referenced
  expect_lt(regexpr("@font-face", css), regexpr("font-family: 'Inter'", css))

  # a missing font file degrades to "" (no face), never errors
  th2 <- blockr_theme("t", webfont = bundled_font("X", "no-such.woff2"))
  expect_no_match(theme_css(th2), "@font-face", fixed = TRUE)

  expect_error(blockr_theme("t", webfont = list(family = "x")), "bundled_font")
})

test_that("presets have the intended shape", {
  # blockr: open, vanilla chrome, ships its own deck AND font
  bl <- theme_blockr()
  expect_length(bl$chrome, 0L)
  expect_true(file.exists(theme_template(bl, "pptx")))
  expect_match(theme_css(bl), "@font-face", fixed = TRUE)

  # org themes (cynkra company, BMS client) are NOT shipped by the open
  # engine -- they live in their own packages
  expect_false(exists("theme_cynkra", where = asNamespace("blockr.theme"),
                      inherits = FALSE))
  expect_false(exists("theme_bms", where = asNamespace("blockr.theme"),
                      inherits = FALSE))
})
