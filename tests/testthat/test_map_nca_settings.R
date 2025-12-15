test_that("Mapping supplied NCA settings to PKNCA options works when settings is NULL or empty list", {
  expect_equal(
    map_nca_settings(NULL),
    NULL
  )
  settings <- list(
    "integralMethod" = "linearUpLogDown"
  )
  expect_error(
    map_nca_settings(settings, "blabla"),
    "not found"
  )
})

test_that("Mapping supplied NCA settings to PKNCA options works with all settings supplied", {
  settings <- list(
    "integralMethod" = "linearUpLogDown",
    "maxMissingValues" = 0.5,
    "missingConcentrations" = "drop",
    "concentrationsBelowLOQFirst" = "keep",
    "concentrationsBelowLOQMiddle" = "drop",
    "concentrationsBelowLOQLast" = "keep",
    "allowanceAdjustedR2" = 0.0001,
    "includeTMax" = FALSE,
    "minPointsRegression" = 3,
    "minSpanRatio" = 2,
    "maxAUCExtrapolation" = 20,
    "minAcceptableR2" = 0.9
  )
  res <- map_nca_settings(settings)
  expect_equal(
    res,
    list(
      adj.r.squared.factor = 1e-04,
      max.missing = 0.5,
      auc.method = "lin up/log down",
      conc.na = "drop",
      conc.blq = list(
        first = "keep",
        middle = "drop",
        last = "keep"
      ),
      allow.tmax.in.half.life = FALSE,
      min.hl.points = 3,
      min.span.ratio = 2,
      max.aucinf.pext = 20,
      min.hl.r.squared = 0.9
    )
  )
})

test_that("Mapping supplied NCA settings to PKNCA options works with some settings supplied", {
  settings <- list(
    "integralMethod" = "linearUpLogDown",
    "maxMissingValues" = 0.5,
    "concentrationsBelowLOQFirst" = "keep",
    "concentrationsBelowLOQMiddle" = "drop",
    "concentrationsBelowLOQLast" = "keep",
    "minAcceptableR2" = 0.9
  )
  res <- map_nca_settings(settings)
  expect_equal(
    res,
    list(
      max.missing = 0.5,
      auc.method = "lin up/log down",
      conc.blq = list(
        first = "keep",
        middle = "drop",
        last = "keep"
      ),
      min.hl.r.squared = 0.9
    )
  )
})
