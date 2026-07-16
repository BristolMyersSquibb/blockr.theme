# Scale map: constructors, resolver, palette, serialization. The hash-pin
# test at the bottom is the convention's single source: saved boards rely on
# stable assignment across versions, so the expected values never change.

test_that("scale_binding validates channels", {
  b <- scale_binding("BOR", color = c(CR = "#006400", PD = "#8b0000"))
  expect_s3_class(b, "scale_binding")
  expect_named(b$color, c("CR", "PD"))

  expect_error(
    scale_binding("X", color = c(CR = "#1", "#2")),
    "fully named"
  )
  expect_error(
    scale_binding("X", color = c(CR = "#1", CR = "#2")),
    "duplicated"
  )
  expect_error(scale_binding(""), "nzchar")

  # shape coerced to integer, names kept
  b <- scale_binding("V", shape = c(EOT = 18, SCHEDULED = 19))
  expect_identical(b$shape, c(EOT = 18L, SCHEDULED = 19L))
})

test_that("new_scale_map flattens with later-wins by variable", {
  m <- new_scale_map(
    scale_binding("A", color = c(x = "#111111")),
    scale_binding("B", color = c(y = "#222222"))
  )
  m2 <- new_scale_map(
    m,
    scale_binding("A", color = c(x = "#999999"))
  )

  expect_identical(names(m2), c("A", "B"))
  expect_identical(m2$A$color, c(x = "#999999"))
  expect_identical(m2$B$color, c(y = "#222222"))

  # whole-binding replacement, no channel merge
  m3 <- new_scale_map(
    new_scale_map(scale_binding("A", color = c(x = "#1"), shape = c(x = 1))),
    scale_binding("A", color = c(x = "#2"))
  )
  expect_null(m3$A$shape)
})

test_that("as_scale_map normalizes deser shapes and rejects junk", {
  plain <- list(
    BOR = list(color = list(CR = "#006400", PD = "#8b0000")),
    POOL = list(color = list("#111111", "#222222")),
    BARE = list()
  )
  m <- as_scale_map(plain)

  expect_s3_class(m, "scale_map")
  expect_identical(m$BOR$color, c(CR = "#006400", PD = "#8b0000"))
  expect_identical(m$POOL$color, c("#111111", "#222222"))
  expect_length(m$BARE, 0L)

  expect_error(as_scale_map(list(list(color = "#1"))), "named")
  expect_null(as_scale_map(NULL))
})

test_that("as_scale_map tolerates unknown binding entries (forward compat)", {
  m <- as_scale_map(list(
    PCHG = list(color = list(a = "#111111"), ramp = list("#1", "#2"))
  ))
  expect_identical(m$PCHG$color, c(a = "#111111"))
  expect_null(m$PCHG$ramp)

  r <- resolve_scales(m, "PCHG", levels = "a")
  expect_identical(r$color, c(a = "#111111"))
})

test_that("reserved dotted variable names are rejected", {
  expect_error(
    as_scale_map(list(.weird = list())),
    "reserved"
  )
})

test_that("board palette: construction, merge, fallback order", {
  pal <- c("#0072B2", "#D55E00", "#F0E442")

  m <- new_scale_map(scale_binding("TRT"), palette = pal)
  expect_identical(attr(m, "palette"), pal)
  expect_error(
    new_scale_map(palette = c(a = "#111111")),
    "unnamed"
  )

  # merge keeps the last non-NULL palette
  m2 <- new_scale_map(m, scale_binding("SEX"))
  expect_identical(attr(m2, "palette"), pal)
  m3 <- new_scale_map(m, palette = c("#101010"))
  expect_identical(attr(m3, "palette"), "#101010")

  # binding pool > board palette > caller palette
  pool <- c("#101010", "#202020")
  mp <- new_scale_map(scale_binding("ID", color = pool), palette = pal)
  r <- resolve_scales(mp, "ID", levels = c("p1", "p2"),
                      palette = "#aaaaaa")
  expect_true(all(r$color %in% pool))

  r2 <- resolve_scales(mp, "TRT", levels = "A", palette = "#aaaaaa")
  expect_null(r2) # TRT not in mp

  m4 <- new_scale_map(scale_binding("TRT"), palette = pal)
  r3 <- resolve_scales(m4, "TRT", levels = "A", palette = "#aaaaaa")
  expect_true(r3$color[["A"]] %in% pal)

  m5 <- new_scale_map(scale_binding("TRT"))
  r4 <- resolve_scales(m5, "TRT", levels = "A", palette = "#aaaaaa")
  expect_identical(r4$color[["A"]], "#aaaaaa")
})

test_that("resolve_scales: fixed values, pool fallback, order", {
  m <- new_scale_map(
    scale_binding(
      "BOR",
      color = c(CR = "#006400", PR = "#FFD700", PD = "#8b0000")
    )
  )
  pal <- c("#aaaaaa", "#bbbbbb", "#cccccc")

  r <- resolve_scales(m, "BOR", levels = c("PD", "CR", "NEW"), palette = pal)

  # fixed beats palette; unbound level gets a palette color
  expect_identical(r$color[["PD"]], "#8b0000")
  expect_identical(r$color[["CR"]], "#006400")
  expect_true(r$color[["NEW"]] %in% pal)

  # order: fixed levels in binding order first, then the rest in input order
  expect_identical(r$order, c("CR", "PD", "NEW"))

  # unregistered variable -> NULL; empty levels -> NULL
  expect_null(resolve_scales(m, "NOPE", levels = "a"))
  expect_null(resolve_scales(m, "BOR", levels = character()))
  expect_null(resolve_scales(NULL, "BOR", levels = "CR"))
})

test_that("resolve_scales: hash assignment is stable across level subsets", {
  m <- new_scale_map(scale_binding("TRT"))
  pal <- c("#0072B2", "#D55E00", "#F0E442")

  all_lv <- resolve_scales(m, "TRT", levels = c("A", "B", "C"), palette = pal)
  one_lv <- resolve_scales(m, "TRT", levels = "B", palette = pal)

  expect_identical(all_lv$color[["B"]], one_lv$color[["B"]])
})

test_that("resolve_scales: shape has no fallback pool", {
  m <- new_scale_map(
    scale_binding("V", shape = c(EOT = 18L))
  )
  r <- resolve_scales(m, "V", levels = c("EOT", "WEEK 1"), palette = "#111111")

  expect_identical(r$shape, c(EOT = 18L))
  expect_true("WEEK 1" %in% names(r$color))
})

test_that("scale_map option round-trips through core JSON serdes", {
  opt <- new_scale_map_option(new_scale_map(
    scale_binding("BEST_OVERALL_RESPONSE",
                  color = c(CR = "#111111", PD = "#222222")),
    scale_binding("USUBJID", color = c("#101010", "#202020")),
    scale_binding("VISIT_TYPE", shape = c(EOT = 18L)),
    palette = c("#0072B2", "#D55E00")
  ))

  ser <- blockr.core::blockr_ser(opt)
  json <- jsonlite::toJSON(ser, null = "null")
  back <- jsonlite::fromJSON(json, simplifyDataFrame = FALSE,
                             simplifyMatrix = FALSE)
  opt2 <- blockr.core::blockr_deser(back)

  v1 <- blockr.core::board_option_value(opt)
  v2 <- blockr.core::board_option_value(opt2)
  expect_identical(v1, v2)
  expect_identical(attr(v2, "palette"), c("#0072B2", "#D55E00"))
  expect_identical(blockr.core::board_option_id(opt2), "scale_map")
})

test_that("v1 deck shape (no .palette) deserializes unchanged", {
  plain <- list(
    BOR = list(color = list(CR = "#006400", PD = "#8b0000")),
    TRT = list()
  )
  m <- as_scale_map(plain)
  expect_null(attr(m, "palette"))
  expect_identical(m$BOR$color, c(CR = "#006400", PD = "#8b0000"))
})
