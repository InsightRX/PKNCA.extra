dat <- nca_admiral
ids <- unique(dat$USUBJID)

test_that("NCA works properly and returns expected results for default config and saves to file", {
  # set up a temporary directory so we can check the file saves as expected
  tdir <- tempdir()
  on.exit(file.remove(file.path(tdir, "nca.rds")))
  nca_data <- run_nca(
    dat,
    path = file.path(tdir, "nca.rds"),
    no_dots = FALSE,
    verbose = F
  )
  expect_equal(class(nca_data), "data.frame")
  expect_equal(nrow(nca_data), 168)
  expect_equal(ncol(nca_data), 20)
  expect_equal(names(nca_data), c("USUBJID", "ACTARM", "GROUP", "ADM", "STUDYID", "EXROUTE",
                                  "nca_start", "nca_end",
                                      "auclast", "aucall", "cmax", "tmax", "half.life", "lambda.z.n.points",
                                      "aucinf.obs", "aucinf.pred", "cl.pred", "vz.pred", "nca_interval",
                                      "lambdaz_fail"))
  expect_equal(round(sum(nca_data$half.life), 1), 3288.5)
  expect_equal(round(sum(nca_data$aucinf.obs), 1), 14490.3)
  expect_equal(nca_data$nca_interval, rep("0 - Inf", 168))
  expect_true(file.exists(file.path(tdir, "nca.rds")))
  expect_true("PKNCA_object" %in% names(attributes(nca_data)))
  expect_true(inherits(attr(nca_data, "PKNCA_object"), "PKNCAresults"))
  expect_true("parameters" %in% names(attributes(nca_data)))
  expect_equal(attr(nca_data, "parameters"), c("auclast", "aucall", "cmax", "tmax", "half.life", "lambda.z.n.points", 
                                               "aucinf.obs", "aucinf.pred", "cl.pred", "vz.pred"))
})

test_that("NCA returns expected results for a non-default config", {
  nca_data <- run_nca(
    dat[dat$USUBJID %in% ids[1:2],], 
    specification = "default2",
    no_dots = FALSE,
    verbose = T
  )
  expect_equal(class(nca_data), "data.frame")
  expect_equal(nrow(nca_data), 2)
  expect_equal(ncol(nca_data), 24)
  expect_equal(names(nca_data), c("USUBJID", "ACTARM", "GROUP", "ADM", "STUDYID", "EXROUTE",
                                  "nca_start", "nca_end", "AUCLAST", "AUCALL", "CMAX", "TMAX",
                                  "cl.all", "THALF", "adj.r.squared", "lambda.z", "KELNOPT", "aucinf.obs",
                                  "AUCINF", "CLF", "VZF", "kel.pred", "nca_interval", "lambdaz_fail"))
  expect_equal(round(sum(nca_data$THALF), 1), 25.8)
  expect_equal(round(sum(nca_data$AUCINF), 1), 116.6)
  
  ## Check that PKNCA.options() get set appropriately
  tmp <- PKNCA::PKNCA.options()
  expect_equal(tmp$conc.blq$middle, "keep") # this settings different from default 
})

test_that("NCA returns expected results when settings are passed", {
  settings <- list(
    "administrationType" = "IV",
    "integralMethod" = "linearUpLogDown",
    "maxMissingValues" = 0.5,
    "missingConcentrations" = "drop",
    "concentrationsBelowLOQFirst" = "keep",
    "concentrationsBelowLOQMiddle" = "keep",
    "concentrationsBelowLOQLast" = "keep",
    "allowanceAdjustedR2" = 0.0001,
    "includeTMax" = FALSE,
    "minPointsRegression" = 5, # higher than default!
    "minSpanRatio" = 2,
    "maxAUCExtrapolation" = 20,
    "minAcceptableR2" = 0.9
  )
  nca_data <- run_nca(
    dat[dat$USUBJID %in% ids[1:2],], 
    settings = settings,
    no_dots = FALSE,
    verbose = T
  )
  expect_equal(class(nca_data), "data.frame")
  expect_equal(nrow(nca_data), 2)
  expect_equal(ncol(nca_data), 20)
  expect_equal(min(nca_data$lambda.z.n.points), 5)
})

test_that("NCA works properly and returns expected results when requesting partial AUC", {
  nca_data <- run_nca(
    dat[dat$USUBJID %in% ids[1:2],], 
    partial_auc = data.frame(start = 0, end = 4),
    no_dots = FALSE,
    verbose = F
  )
  expect_equal(class(nca_data), "data.frame")
  expect_equal(nrow(nca_data), 2)
  expect_equal(ncol(nca_data), 21)
  expect_equal(names(nca_data), c("USUBJID", "ACTARM", "GROUP", "ADM", "STUDYID", "EXROUTE",
                                  "nca_start", "nca_end",
                                  "auclast", "aucall", "cmax", "tmax", "half.life", "lambda.z.n.points",
                                  "aucinf.obs", "aucinf.pred", "cl.pred", "vz.pred",
                                  "auc_partial_0_4", "nca_interval", "lambdaz_fail"))
  expect_equal(round(sum(nca_data$auc_partial_0_4), 1), 12.8)
  expect_true("PKNCA_object" %in% names(attributes(nca_data)))
  expect_true(inherits(attr(nca_data, "PKNCA_object"), "PKNCAresults"))
  expect_equal(attr(nca_data, "parameters"), c("auclast", "aucall", "cmax", "tmax", "half.life", "lambda.z.n.points", 
                                               "aucinf.obs", "aucinf.pred", "cl.pred", "vz.pred", "auc_partial_0_4"))
  
})

test_that("NCA works properly and returns expected results when requesting one specific intervals", {
  nca_data <- run_nca(
    dat[dat$USUBJID %in% ids[1:2],],
    intervals = data.frame(
      start = c(0), 
      end = c(24)
    ),
    no_dots = FALSE,
    verbose = F
  )
  expect_equal(class(nca_data), "data.frame")
  expect_equal(nrow(nca_data), 2)
  expect_equal(ncol(nca_data), 21)
  expect_equal(names(nca_data), c("USUBJID", "ACTARM", "GROUP", "ADM", "STUDYID", "EXROUTE",
                                  "nca_start", "nca_end",
                                  "auclast", "aucall", "cmax", "tmax", "half.life", "lambda.z.n.points",
                                  "aucinf.obs", "aucinf.pred", "cl.pred", "vz.pred", "nca_interval", "auctau",
                                  "lambdaz_fail"))
  expect_equal(round(sum(nca_data$auclast), 1), 77.9)
  expect_true("PKNCA_object" %in% names(attributes(nca_data)))
  expect_true(inherits(attr(nca_data, "PKNCA_object"), "PKNCAresults"))
})

test_that("NCA works properly and returns expected results when requesting two intervals", {
  suppressWarnings(
    nca_data <- run_nca(
      dat[dat$USUBJID %in% ids[1:4],], 
      intervals = data.frame(
        start = c(0, 0), 
        end = c(12, Inf)
      ),
      no_dots = FALSE,
      verbose = F
    ) 
  )
  expect_equal(class(nca_data), "data.frame")
  expect_equal(nrow(nca_data), 4 * 2)
  expect_equal(ncol(nca_data), 21)
  expect_equal(names(nca_data), c("USUBJID", "ACTARM", "GROUP", "ADM", "STUDYID", "EXROUTE",
                                  "nca_start", "nca_end",
                                  "auclast", "aucall", "cmax", "tmax", "half.life", "lambda.z.n.points",
                                  "aucinf.obs", "aucinf.pred", "cl.pred", "vz.pred", "nca_interval", "auctau",
                                  "lambdaz_fail"))
  expect_equal(round(sum(nca_data$auclast), 1), 393.9)
})

test_that("NCA works properly and returns expected results when requesting two intervals and partial auc", {
  nca_data <- run_nca(
    dat[dat$USUBJID %in% ids[1:4],], 
    no_dots = FALSE,
    intervals = data.frame(
      start = c(0, 0), 
      end = c(24, Inf)
    ),
    partial_auc = data.frame(
      start = 0,
      end = 12
    ),
    verbose = F
  )
  expect_equal(class(nca_data), "data.frame")
  expect_equal(nrow(nca_data), 4 * 2)
  expect_equal(ncol(nca_data), 22)
  expect_equal(names(nca_data), c("USUBJID", "ACTARM", "GROUP", "ADM", "STUDYID", "EXROUTE",
                                  "nca_start", "nca_end",
                                  "auclast", "aucall", "cmax", "tmax", "half.life", "lambda.z.n.points",
                                  "aucinf.obs", "aucinf.pred", "cl.pred", "vz.pred",
                                  "auc_partial_0_12", "nca_interval", "auctau", "lambdaz_fail"))
  expect_equal(round(sum(nca_data$auclast), 1), 473.1)
  expect_equal(round(sum(nca_data$auc_partial_0_12), 1), 255.8)
})

test_that("NCA works without dose info", {
  suppressWarnings(
    nca_data <- run_nca(
      data = dat[dat$USUBJID %in% ids[1:2],] |>
        dplyr::select(-EXDOSE), 
      no_dots = F,
      verbose = F
    )
  )
  expect_equal(class(nca_data), "data.frame")
  expect_equal(nrow(nca_data), 2)
  expect_equal(ncol(nca_data), 20)
  expect_equal(names(nca_data), c("USUBJID", "ACTARM", "GROUP", "ADM", "STUDYID", "EXROUTE",
                                  "nca_start", "nca_end",
                                  "auclast", "aucall", "cmax", "tmax", "half.life", "lambda.z.n.points",
                                  "aucinf.obs", "aucinf.pred", "cl.pred", "vz.pred", "nca_interval",
                                  "lambdaz_fail"))
  expect_equal(round(sum(nca_data$half.life), 1), 25.8)
  expect_equal(round(sum(nca_data$aucinf.obs), 1), 117.2)
  expect_true(all(is.na(nca_data$cl.pred)))
  expect_true(all(is.na(nca_data$vz.pred)))
})

test_that("NCA gives warning when partial AUC specifies duplicate interva but doesn't error", {
  expect_message(
    nca_data <- run_nca(
      dat[dat$USUBJID %in% ids[1:4],],
      no_dots = FALSE,
      intervals = data.frame(
        start = c(0, 0), 
        end = c(24, Inf)
      ),
      partial_auc = data.frame(
        start = 0,
        end = 24 # duplicate from `intervals`
      ),
      verbose = F
    )
    , "Partial AUC interval"
  )
  expect_equal(class(nca_data), "data.frame")
  expect_equal(nrow(nca_data), 4 * 2)
  expect_equal(ncol(nca_data), 21)
  expect_equal(names(nca_data), c("USUBJID", "ACTARM", "GROUP", "ADM", "STUDYID", "EXROUTE",
                                  "nca_start", "nca_end",
                                  "auclast", "aucall", "cmax", "tmax", "half.life", "lambda.z.n.points",
                                  "aucinf.obs", "aucinf.pred", "cl.pred", "vz.pred", "nca_interval", "auctau",
                                  "lambdaz_fail"))
  expect_equal(round(sum(nca_data$auclast), 1), 473.1)
  expect_equal(nca_data$nca_interval, rep(c("0 - 24", "0 - Inf"), 4))
})

describe("Test `check_grouping` argument", {
  
  test_that("run_nca with check_grouping=TRUE passes when groups have sufficient data", {
    # Create data with sufficient points per group
    data <- dat[dat$USUBJID %in% ids[1:10], ]
    
    # Should pass without error
    expect_no_error(
      nca_data <- run_nca(
        data,
        groups = "ACTARM",
        check_grouping = TRUE,
        verbose = FALSE
      )
    )
  })
  
  test_that("run_nca with check_grouping=TRUE fails when groups have insufficient data", {
    # Create data where most subjects have too few points
    data <- dat[dat$USUBJID %in% ids[1:10], ]
    
    # Should error due to insufficient data points
    expect_error(
      expect_warning(
        run_nca(
          data,
          groups = c("ACTARM", "PCTPTNUM"),
          check_grouping = TRUE,
          verbose = FALSE
        )
      ),
      "The grouping is likely incorrect"
    )
  })
  
  test_that("run_nca check_grouping works with multiple grouping variables", {
    data <- dat[dat$USUBJID %in% ids[1:20], ]

    # Should pass with multiple groups
    expect_no_error(
      nca_data <- run_nca(
        data,
        groups = c("ACTARM", "EXROUTE"),
        check_grouping = TRUE,
        verbose = FALSE
      )
    )
  })

})

describe("Test `exclude_lambda_z` and `include_lambda_z` arguments", {

  test_that("exclude_lambda_z excludes specific points from lambda-z fit only", {
    nca_base <- run_nca(
      dat[dat$USUBJID %in% ids[1:2], ],
      no_dots = FALSE,
      verbose = FALSE
    )
    # Exclude the 36h timepoint for subject 1 from the lambda-z fit
    nca_excl <- run_nca(
      dat[dat$USUBJID %in% ids[1:2], ],
      no_dots = FALSE,
      verbose = FALSE,
      exclude_lambda_z = list(
        SAMPLEID = list(
          id = "017011028-20130720T120000",
          reason = "test exclusion"
        )
      )
    )
    # Subject 1: half-life and n.points change because one point is excluded from lambda-z
    expect_false(nca_excl$half.life[1] == nca_base$half.life[1])
    expect_equal(round(nca_excl$half.life[1], 4), 15.3731)
    expect_equal(nca_excl$lambda.z.n.points[1], 4)  # was 3, now 4 (different window)
    # Subject 2: unaffected (exclusion only applied to subject 1's sample)
    expect_equal(nca_excl$half.life[2], nca_base$half.life[2])
  })

  test_that("exclude_lambda_z does not affect Cmax or AUClast", {
    nca_base <- run_nca(
      dat[dat$USUBJID %in% ids[1:2], ],
      no_dots = FALSE,
      verbose = FALSE
    )
    nca_excl <- run_nca(
      dat[dat$USUBJID %in% ids[1:2], ],
      no_dots = FALSE,
      verbose = FALSE,
      exclude_lambda_z = list(
        SAMPLEID = list(
          id = "017011028-20130720T120000",
          reason = "test exclusion"
        )
      )
    )
    # Summary parameters (Cmax, AUClast) should be unchanged
    expect_equal(nca_excl$cmax, nca_base$cmax)
    expect_equal(nca_excl$auclast, nca_base$auclast)
    expect_equal(nca_excl$tmax, nca_base$tmax)
  })

  test_that("include_lambda_z only affects the specified subject, not others", {
    nca_base <- run_nca(
      dat[dat$USUBJID %in% ids[1:3], ],
      no_dots = FALSE,
      verbose = FALSE
    )
    nca_incl <- suppressWarnings(run_nca(
      dat[dat$USUBJID %in% ids[1:3], ],
      no_dots = FALSE,
      verbose = FALSE,
      include_lambda_z = list(
        SAMPLEID = list(
          id = c("017011028-20130720T120000", "017011028-20130721T000000"),
          reason = "test"
        )
      )
    ))
    # Subject 1: half-life and n.points change (only 2 forced points used for lambda-z)
    expect_false(nca_incl$half.life[1] == nca_base$half.life[1])
    expect_equal(round(nca_incl$half.life[1], 4), 21.5365)
    expect_equal(nca_incl$lambda.z.n.points[1], 2)
    # Subjects 2 and 3: unaffected — inclusion only specified for subject 1's samples
    expect_equal(nca_incl$half.life[2], nca_base$half.life[2])
    expect_equal(nca_incl$half.life[3], nca_base$half.life[3])
  })

  test_that("include_lambda_z forces specific points into the lambda-z fit", {
    # For subject 2, force only the 3 latest timepoints into the lambda-z fit
    nca_base <- run_nca(
      dat[dat$USUBJID == ids[2], ],
      no_dots = FALSE,
      verbose = FALSE
    )
    nca_incl <- suppressWarnings(run_nca(
      dat[dat$USUBJID == ids[2], ],
      no_dots = FALSE,
      verbose = FALSE,
      include_lambda_z = list(
        SAMPLEID = list(
          id = c(
            "017011033-20140319T000000",
            "017011033-20140319T120000",
            "017011033-20140320T000000"
          ),
          reason = "forced inclusion"
        )
      )
    ))
    # Half-life differs from baseline (baseline uses 7 points; forced uses only 3)
    expect_false(nca_incl$half.life == nca_base$half.life)
    expect_equal(round(nca_incl$half.life, 4), 15.1423)
    expect_equal(nca_incl$lambda.z.n.points, 3)  # exactly the 3 forced points
  })

  test_that("exclude_lambda_z gives warning when specified IDs not found in data", {
    expect_message(
      run_nca(
        dat[dat$USUBJID %in% ids[1:2], ],
        no_dots = FALSE,
        verbose = FALSE,
        exclude_lambda_z = list(
          SAMPLEID = list(
            id = "NONEXISTENT-SAMPLE-ID",
            reason = "test"
          )
        )
      ),
      "lambda-z datapoints to be excluded not found"
    )
  })

  test_that("units argument passes through to PKNCA and attaches units attribute", {
    dat <- nca_admiral; ids <- unique(dat$USUBJID)

    # Form 1: named list - should build units table internally
    res_list <- run_nca(
      dat[dat$USUBJID %in% ids[1:2], ],
      units = list(concu = "ng/mL", timeu = "h", doseu = "mg"),
      verbose = FALSE
    )
    pknca_obj <- attr(res_list, "PKNCA_object")
    expect_true("PPORRESU" %in% names(pknca_obj$result))
    units_attr <- attr(res_list, "units")
    expect_true(is.data.frame(units_attr))
    expect_true(all(c("name", "unit") %in% names(units_attr)))
    expect_true(nrow(units_attr) > 0)
    expect_true(any(units_attr$unit != ""))

    # Form 2: pre-built data frame - should pass through unchanged
    units_tbl <- PKNCA::pknca_units_table(concu = "ng/mL", timeu = "h", doseu = "mg")
    res_tbl <- run_nca(
      dat[dat$USUBJID %in% ids[1:2], ],
      units = units_tbl,
      verbose = FALSE
    )
    expect_true("PPORRESU" %in% names(attr(res_tbl, "PKNCA_object")$result))
    expect_true(is.data.frame(attr(res_tbl, "units")))

    # NULL units (default) - no units attribute attached
    res_null <- run_nca(
      dat[dat$USUBJID %in% ids[1:2], ],
      verbose = FALSE
    )
    expect_null(attr(res_null, "units"))
    expect_false("PPORRESU" %in% names(attr(res_null, "PKNCA_object")$result))
  })

  test_that("conversions argument passes manual conversion table to pknca_units_table", {
    dat <- nca_admiral; ids <- unique(dat$USUBJID)
    manual_conv <- data.frame(
      PPORRESU = c("mg/(ng/mL)", "mg/(h*ng/mL)"),
      PPSTRESU = c("L", "L/h"),
      conversion_factor = c(1000, 1000),
      stringsAsFactors = FALSE
    )
    res <- run_nca(
      dat[dat$USUBJID %in% ids[1:2], ],
      units = list(concu = "ng/mL", timeu = "h", doseu = "mg"),
      conversions = manual_conv,
      verbose = FALSE
    )
    u <- attr(res, "units")
    expect_true("PPSTRESU" %in% names(attr(res, "PKNCA_object")$result))
    expect_equal(u$unit[u$name == "vz_pred"], "L")
    expect_equal(u$unit[u$name == "cl_pred"], "L/h")
    ## get_nca_individual_fits() must not crash when PPSTRES is present
    fits <- get_nca_individual_fits(attr(res, "PKNCA_object"))
    expect_true(is.data.frame(fits))
    expect_true("used_in_fit" %in% names(fits))
  })

  test_that("include_lambda_z gives warning when specified IDs not found in data", {
    expect_message(
      run_nca(
        dat[dat$USUBJID %in% ids[1:2], ],
        no_dots = FALSE,
        verbose = FALSE,
        include_lambda_z = list(
          SAMPLEID = list(
            id = "NONEXISTENT-SAMPLE-ID",
            reason = "test"
          )
        )
      ),
      "lambda-z datapoints to be included not found"
    )
  })

  describe("min.hl.time setting", {

    test_that("min.hl.time has no effect when all terminal points are above the threshold", {
      dat <- nca_admiral; ids <- unique(dat$USUBJID)
      # Subject 1 default terminal fit uses 24h, 36h, 48h; threshold of 12h leaves those unchanged
      nca_base <- run_nca(dat[dat$USUBJID == ids[1], ], no_dots = FALSE, verbose = FALSE)
      nca_min <- run_nca(
        dat[dat$USUBJID == ids[1], ],
        no_dots = FALSE, verbose = FALSE,
        settings = list("min.hl.time" = 12)
      )
      expect_equal(nca_min$half.life, nca_base$half.life)
      expect_equal(nca_min$lambda.z.n.points, nca_base$lambda.z.n.points)
    })

    test_that("min.hl.time excludes early time points from lambda-z fit", {
      dat <- nca_admiral; ids <- unique(dat$USUBJID)
      # Subject 1 default 3-point fit uses 24h, 36h, 48h.
      # Setting min.hl.time = 30 excludes 24h, leaving only 36h + 48h (< min.hl.points=3) -> NA
      nca_min <- suppressWarnings(run_nca(
        dat[dat$USUBJID == ids[1], ],
        no_dots = FALSE, verbose = FALSE,
        settings = list("min.hl.time" = 30)
      ))
      expect_true(is.na(nca_min$half.life))
      expect_true(is.na(nca_min$lambda.z.n.points))
    })

    test_that("min.hl.time does not affect Cmax or AUClast", {
      dat <- nca_admiral; ids <- unique(dat$USUBJID)
      nca_base <- run_nca(dat[dat$USUBJID == ids[1], ], no_dots = FALSE, verbose = FALSE)
      nca_min <- suppressWarnings(run_nca(
        dat[dat$USUBJID == ids[1], ],
        no_dots = FALSE, verbose = FALSE,
        settings = list("min.hl.time" = 30)
      ))
      expect_equal(nca_min$cmax, nca_base$cmax)
      expect_equal(nca_min$auclast, nca_base$auclast)
      expect_equal(nca_min$tmax, nca_base$tmax)
    })

    test_that("min.hl.time can be combined with exclude_lambda_z", {
      dat <- nca_admiral; ids <- unique(dat$USUBJID)
      # For subject 1: min.hl.time = 5 alone has no effect (terminal points are 24h+)
      # Combining with exclude_lambda_z for the 36h point should give the same result
      # as exclude_lambda_z alone (both masks are OR'd together)
      nca_excl_only <- run_nca(
        dat[dat$USUBJID == ids[1], ],
        no_dots = FALSE, verbose = FALSE,
        exclude_lambda_z = list(SAMPLEID = list(id = "017011028-20130720T120000", reason = "test"))
      )
      nca_combined <- run_nca(
        dat[dat$USUBJID == ids[1], ],
        no_dots = FALSE, verbose = FALSE,
        settings = list("min.hl.time" = 5),
        exclude_lambda_z = list(SAMPLEID = list(id = "017011028-20130720T120000", reason = "test"))
      )
      # Both should yield the same result: min.hl.time=5 adds no additional exclusions
      expect_equal(nca_combined$half.life, nca_excl_only$half.life)
      expect_equal(nca_combined$lambda.z.n.points, nca_excl_only$lambda.z.n.points)
    })

    test_that("minHalfLifeTime (alias) works the same as min.hl.time", {
      dat <- nca_admiral; ids <- unique(dat$USUBJID)
      nca_name <- suppressWarnings(run_nca(
        dat[dat$USUBJID == ids[1], ],
        no_dots = FALSE, verbose = FALSE,
        settings = list("min.hl.time" = 30)
      ))
      nca_alias <- suppressWarnings(run_nca(
        dat[dat$USUBJID == ids[1], ],
        no_dots = FALSE, verbose = FALSE,
        settings = list("minHalfLifeTime" = 30)
      ))
      expect_equal(nca_alias$half.life, nca_name$half.life)
    })

  })

})

describe("lambdaz_fail column", {

  test_that("lambdaz_fail column is present in wide output", {
    dat <- nca_admiral; ids <- unique(dat$USUBJID)
    nca <- suppressWarnings(run_nca(dat[dat$USUBJID %in% ids[1:5], ], verbose = FALSE))
    expect_true("lambdaz_fail" %in% names(nca))
    expect_type(nca$lambdaz_fail, "character")
  })

  test_that("lambdaz_fail is NA when lambda_z succeeds", {
    dat <- nca_admiral; ids <- unique(dat$USUBJID)
    # ids[3] = "01-701-1097" has a successful lambda_z fit in nca_admiral
    nca <- suppressWarnings(run_nca(dat[dat$USUBJID == ids[3], ], verbose = FALSE))
    expect_false(is.na(nca$half_life))
    expect_true(is.na(nca$lambdaz_fail))
  })

  test_that("lambdaz_fail contains failure reason when lambda_z fails", {
    dat <- nca_admiral; ids <- unique(dat$USUBJID)
    # Force failure by excluding all terminal-phase points via min.hl.time > max time
    nca <- suppressWarnings(run_nca(
      dat[dat$USUBJID == ids[3], ],
      settings = list(min.hl.time = 100),
      verbose = FALSE
    ))
    expect_true(is.na(nca$half_life))
    expect_false(is.na(nca$lambdaz_fail))
    expect_match(nca$lambdaz_fail, "Too few points")
  })

  test_that("lambdaz_fail is a static column in long format, not pivoted", {
    dat <- nca_admiral; ids <- unique(dat$USUBJID)
    nca <- suppressWarnings(run_nca(
      dat[dat$USUBJID %in% ids[1:5], ],
      format = "long",
      verbose = FALSE
    ))
    # lambdaz_fail should be a column, not a value in the 'name' column
    expect_true("lambdaz_fail" %in% names(nca))
    expect_false("lambdaz_fail" %in% nca$name)
    # value column should be numeric (not coerced to character by lambdaz_fail)
    expect_type(nca$value, "double")
  })

  test_that("lambdaz_fail is all NA when all lambda_z calculations succeed", {
    dat <- nca_admiral; ids <- unique(dat$USUBJID)
    nca <- suppressWarnings(run_nca(dat, verbose = FALSE))
    expect_true(all(is.na(nca$lambdaz_fail)))
  })

  test_that("lambdaz_fail mixes NA and non-NA when some subjects fail and some succeed", {
    dat <- nca_admiral; ids <- unique(dat$USUBJID)
    # Use two subjects: force ids[1] to fail via min.hl.time, keep ids[3] (which
    # succeeds under default settings) without the restriction
    nca_fail <- suppressWarnings(run_nca(
      dat[dat$USUBJID == ids[1], ],
      settings = list(min.hl.time = 100),
      verbose = FALSE
    ))
    nca_ok <- suppressWarnings(run_nca(
      dat[dat$USUBJID == ids[3], ],
      verbose = FALSE
    ))
    expect_false(is.na(nca_fail$lambdaz_fail))
    expect_true(is.na(nca_ok$lambdaz_fail))
  })

})

test_that("run_nca works when column names contain special characters like parentheses and slashes", {
  dat <- nca_admiral; ids <- unique(dat$USUBJID)
  dat2 <- dat[dat$USUBJID %in% ids[1:3], ]
  ## Rename columns to names with special characters
  names(dat2)[names(dat2) == "ACONC"] <- "concentration_(mg/L)"
  names(dat2)[names(dat2) == "NTIME"] <- "time (h)"
  nca_data <- suppressWarnings(run_nca(
    dat2,
    dictionary = list(
      conc = "concentration_(mg/L)",
      time = "time (h)"
    ),
    verbose = FALSE
  ))
  expect_equal(class(nca_data), "data.frame")
  expect_equal(nrow(nca_data), 3)
})


test_that("PKNCAconc errors other than non-unique times surface the PKNCA message", {
  d <- data.frame(
    ID = 1, TIME = c(0, 1, 2), DV = c(0, 5, 3),
    EVID = c(1, 0, 0), AMT = c(100, 0, 0)
  )
  err <- expect_error(
    run_nca(d, verbose = FALSE),
    "An error occurred parsing the concentration data"
  )
  expect_false(grepl("argument is of length zero", conditionMessage(err)))
  expect_false(grepl("not unique for each NCA", conditionMessage(err)))
})

## 1-cmt IV bolus data with closed-form AUC, where the sample at the end of the
## interval (nominal 12 h) is drawn late.
make_late_boundary_data <- function(dose_times = 0) {
  subj <- data.frame(ID = 1:5, CL = c(4, 5, 6, 4.5, 5.5), V = c(18, 20, 22, 19, 21))
  late <- c(0.25, 0.40, 0.15, 0.55, 0.30)
  nom  <- c(0, 0.5, 1, 2, 4, 6, 8, 10, 12, 16, 24)
  conc <- do.call(rbind, lapply(1:5, function(i) {
    t <- nom
    t[nom == 12] <- 12 + late[i]
    k <- subj$CL[i] / subj$V[i]
    dv <- sapply(t, function(tt) sum(100 / subj$V[i] * exp(-k * (tt - dose_times[dose_times <= tt]))))
    data.frame(ID = i, TIME = t, DV = dv, AMT = 0, EVID = 0)
  }))
  dose <- expand.grid(ID = 1:5, TIME = dose_times)
  dose <- data.frame(ID = dose$ID, TIME = dose$TIME, DV = 0, AMT = 100, EVID = 1)
  d <- rbind(dose, conc)
  d <- d[order(d$ID, d$TIME, -d$EVID), ]
  list(data = d, subj = subj)
}
dict_late_boundary <- list(subject_id = "ID", time = "TIME", conc = "DV", dose = "AMT", evid = "EVID")

test_that("partial AUC interpolates to end of interval when boundary sample is late", {
  tmp <- make_late_boundary_data()
  true_auc <- with(tmp$subj, 100 / CL * (1 - exp(-CL / V * 12)))
  nca_data <- run_nca(
    tmp$data,
    dictionary = dict_late_boundary,
    partial_auc = data.frame(start = 0, end = 12),
    verbose = FALSE
  )
  expect_equal(nca_data$auc_partial_0_12, true_auc, tolerance = 1e-6)
  expect_false("aucint_last_dose" %in% names(nca_data))
})

test_that("auctau interpolates to end of interval when boundary sample is late", {
  tmp <- make_late_boundary_data()
  true_auc <- with(tmp$subj, 100 / CL * (1 - exp(-CL / V * 12)))
  nca_data <- run_nca(
    tmp$data,
    dictionary = dict_late_boundary,
    intervals = data.frame(start = 0, end = 12, auclast = TRUE),
    add_auctau = TRUE,
    verbose = FALSE
  )
  expect_equal(nca_data$auctau, true_auc, tolerance = 1e-6)
  expect_true(all(nca_data$auclast < true_auc))
  expect_false("aucint_last_dose" %in% names(nca_data))
  expect_false("aucint_last_dose" %in% attr(nca_data, "parameters"))
})

test_that("auctau for multi-dose data is correct when the trough sample is drawn after the next dose", {
  ## Doses at 0 and 12 h, IV bolus. The nominal 12 h sample is drawn after the
  ## second dose, so PKNCA itself would not select the 0-12 h dosing interval.
  ## Interpolating with that post-dose sample would overstate AUCtau, while
  ## `auclast` stops at the 10 h sample and understates it.
  tmp <- make_late_boundary_data(dose_times = c(0, 12))
  true_auc <- with(tmp$subj, 100 / CL * (1 - exp(-CL / V * 12)))
  nca_data <- run_nca(
    tmp$data,
    dictionary = dict_late_boundary,
    post = list(),
    verbose = FALSE
  )
  first <- nca_data[nca_data$nca_start == 0, ]
  expect_equal(nrow(first), 5)
  expect_equal(first$nca_end, rep(12, 5))
  expect_equal(first$auctau, true_auc, tolerance = 1e-4)
  expect_true(all(first$auclast < true_auc))
  ## second dosing interval is still selected, and auctau is computed for it
  second <- nca_data[nca_data$nca_start == 12, ]
  expect_equal(second$nca_end, rep(24, 5))
  expect_true(all(!is.na(second$auctau)))
  expect_false(any(c("aucint_last_dose", "aucint_inf_obs_dose", ".auctau_aucint") %in% names(nca_data)))
})

test_that("partial AUC is computed for multi-dose data when the boundary sample is drawn after the next dose", {
  tmp <- make_late_boundary_data(dose_times = c(0, 12))
  true_auc <- with(tmp$subj, 100 / CL * (1 - exp(-CL / V * 12)))
  nca_data <- run_nca(
    tmp$data,
    dictionary = dict_late_boundary,
    partial_auc = data.frame(start = 0, end = 12),
    post = list(),
    verbose = FALSE
  )
  expect_true("auc_partial_0_12" %in% names(nca_data))
  first <- nca_data[nca_data$nca_start == 0, ]
  expect_equal(first$auc_partial_0_12, true_auc, tolerance = 1e-4)
})

test_that("auctau is NA with a warning when extrapolation to the next dose is not possible", {
  tmp <- make_late_boundary_data(dose_times = c(0, 12))
  ## too few points in the first dosing interval to estimate lambda-z
  d <- tmp$data[!(tmp$data$EVID == 0 & tmp$data$TIME %in% c(0.5, 1, 2, 4, 6)), ]
  expect_message(
    nca_data <- run_nca(
      d,
      dictionary = dict_late_boundary,
      post = list(),
      verbose = FALSE
    ),
    "not computed for 5 interval"
  )
  expect_true(all(is.na(nca_data$auctau[nca_data$nca_start == 0])))
  expect_true(all(!is.na(nca_data$auctau[nca_data$nca_start == 12])))
})

test_that("auctau uses observed concentration at the next dose without needing lambda-z", {
  tmp <- make_late_boundary_data(dose_times = c(0, 12))
  d <- tmp$data
  ## add a pre-dose sample exactly at the time of the second dose
  pre <- d[d$EVID == 0 & d$TIME == 10, ]
  pre$TIME <- 12
  pre$DV <- with(tmp$subj, 100 / V * exp(-CL / V * 12))
  d <- rbind(d, pre)
  d <- d[order(d$ID, d$TIME, d$EVID == 0), ]
  ## remove points so lambda-z cannot be estimated
  d <- d[!(d$EVID == 0 & d$TIME %in% c(0.5, 1, 2, 4, 6)), ]
  nca_data <- run_nca(
    d,
    dictionary = dict_late_boundary,
    post = list(),
    verbose = FALSE
  )
  true_auc <- with(tmp$subj, 100 / CL * (1 - exp(-CL / V * 12)))
  first <- nca_data[nca_data$nca_start == 0, ]
  expect_equal(first$auctau, true_auc, tolerance = 1e-4)
})

test_that("partial AUC and auctau extrapolate when end is after the last sample", {
  tmp <- make_late_boundary_data()
  true_auc <- with(tmp$subj, 100 / CL * (1 - exp(-CL / V * 12)))
  ## sampled only through 10 h
  d <- tmp$data[tmp$data$EVID == 1 | tmp$data$TIME <= 10, ]
  nca_data <- run_nca(
    d,
    dictionary = dict_late_boundary,
    intervals = data.frame(start = 0, end = 12, auclast = TRUE),
    partial_auc = data.frame(start = 0, end = 12.0001),
    verbose = FALSE
  )
  expect_equal(nca_data$auctau, true_auc, tolerance = 1e-4)
  expect_true(all(nca_data$auclast < true_auc))
  ## without dose data
  nca_nodose <- suppressWarnings(run_nca(
    d[d$EVID == 0, c("ID", "TIME", "DV")],
    dictionary = list(subject_id = "ID", time = "TIME", conc = "DV"),
    partial_auc = data.frame(start = 0, end = 12),
    verbose = FALSE
  ))
  expect_equal(nca_nodose$auc_partial_0_12, true_auc, tolerance = 1e-4)
})

test_that("excluded sample at end of interval is not used as boundary sample", {
  tmp <- make_late_boundary_data()
  true_auc <- with(tmp$subj, 100 / CL * (1 - exp(-CL / V * 12)))
  d <- tmp$data[tmp$data$EVID == 1 | tmp$data$TIME <= 10, ]
  ## add a (bad) sample exactly at 12 h, and exclude it
  extra <- d[d$EVID == 0 & d$TIME == 10, ]
  extra$TIME <- 12
  extra$DV <- 100
  d <- rbind(d, extra)
  d <- d[order(d$ID, d$TIME, -d$EVID), ]
  d$SAMPLEID <- paste(d$ID, d$TIME, d$EVID, sep = "_")
  nca_data <- run_nca(
    d,
    dictionary = dict_late_boundary,
    partial_auc = data.frame(start = 0, end = 12),
    exclude_points = list(SAMPLEID = list(id = paste(1:5, 12, 0, sep = "_"), reason = "assay error")),
    verbose = FALSE
  )
  expect_equal(nca_data$auc_partial_0_12, true_auc, tolerance = 1e-4)
})

test_that("partial AUC warns about ignored interval columns", {
  dat <- nca_admiral; ids <- unique(dat$USUBJID)
  dat <- dat[dat$USUBJID %in% ids[1:2], ]
  expect_message(
    nca_data <- run_nca(
      dat,
      partial_auc = data.frame(start = 0, end = 4, cmax = TRUE, impute = "start_conc0"),
      verbose = FALSE
    ),
    "ignoring columns: cmax, impute"
  )
  expect_true("auc_partial_0_4" %in% names(nca_data))
})

test_that("auctau and partial AUC use C0 back-extrapolation at an IV bolus dose without a sample", {
  ## With the route specified, the start of the 12-24 h interval (IV bolus, no
  ## sample at 12 h) uses the back-extrapolated C0 from the imputation, and the
  ## end of the 0-12 h interval is extrapolated to the dose.
  tmp <- make_late_boundary_data(dose_times = c(0, 12))
  d <- tmp$data
  d$ROUTE <- "IV"
  auc_0_12 <- with(tmp$subj, 100 / CL * (1 - exp(-CL / V * 12)))
  auc_12_24 <- with(tmp$subj, 100 / CL * (1 - exp(-CL / V * 12)) * (1 + exp(-CL / V * 12)))
  nca_data <- run_nca(
    d,
    dictionary = c(dict_late_boundary, route = "ROUTE"),
    partial_auc = data.frame(start = 12, end = 24.0001),
    post = list(),
    verbose = FALSE
  )
  first <- nca_data[nca_data$nca_start == 0, ]
  second <- nca_data[nca_data$nca_start == 12, ]
  expect_equal(first$auctau, auc_0_12, tolerance = 1e-4)
  expect_equal(second$auctau, auc_12_24, tolerance = 1e-4)
  expect_equal(second$auc_partial_12_24.0001, auc_12_24, tolerance = 1e-3)
})

test_that("add_missing_dosing_intervals only adds dosing intervals PKNCA skipped", {
  tmp <- make_late_boundary_data(dose_times = c(0, 12))
  d <- tmp$data
  conc_obj <- PKNCA::PKNCAconc(d[d$EVID == 0, ], DV ~ TIME | ID)
  dose_obj <- PKNCA::PKNCAdose(d[d$EVID == 1, ], AMT ~ TIME | ID)
  obj <- PKNCA::PKNCAdata(conc_obj, dose_obj)
  expect_equal(unique(obj$intervals$start), 12)
  obj2 <- add_missing_dosing_intervals(obj)
  expect_equal(nrow(obj2$intervals), 10)
  expect_equal(sort(unique(paste(obj2$intervals$start, obj2$intervals$end))), c("0 12", "12 24"))
  expect_true(all(obj2$intervals$auclast[obj2$intervals$start == 0]))
  ## nothing to add for single-dose data
  dose_sd <- PKNCA::PKNCAdose(d[d$EVID == 1 & d$TIME == 0, ], AMT ~ TIME | ID)
  obj_sd <- PKNCA::PKNCAdata(conc_obj, dose_sd)
  expect_equal(add_missing_dosing_intervals(obj_sd)$intervals, obj_sd$intervals)
})

test_that("nca_add_auctau prefers aucint columns over auclast", {
  out <- data.frame(
    nca_end = c(12, Inf),
    auclast = c(10, 20),
    aucint_last_dose = c(11, NA)
  )
  res <- nca_add_auctau(out, dictionary = list())
  expect_equal(res$auctau, c(11, NA))
  ## no `auclast` fallback, since it is biased low
  expect_message(
    res <- nca_add_auctau(out[, c("nca_end", "auclast")], dictionary = list()),
    "setting `auctau` to NA"
  )
  expect_equal(res$auctau, c(NA_real_, NA_real_))
  ## column is added even if all values are NA
  out$aucint_last_dose <- NA_real_
  res <- nca_add_auctau(out, dictionary = list())
  expect_equal(res$auctau, c(NA_real_, NA_real_))
  ## not added without bounded intervals
  res <- nca_add_auctau(out[2, ], dictionary = list())
  expect_false("auctau" %in% names(res))
})

test_that("final dosing interval is added and extrapolated when sampling stops before its end", {
  tmp <- make_late_boundary_data(dose_times = c(0, 12))
  ## subjects 1-2 sampled through 22 h only (no 24 h sample)
  d <- tmp$data
  d$TIME[d$EVID == 0 & d$TIME == 24 & d$ID %in% 1:2] <- 22
  d$DV[d$EVID == 0 & d$TIME == 22] <- with(
    tmp$subj[1:2, ],
    100 / V * (exp(-CL / V * 22) + exp(-CL / V * 10))
  )
  ## extra samples in second interval so lambda-z can be estimated
  extra <- d[d$EVID == 0 & d$TIME == 16, ]
  extra$TIME <- 19
  extra$DV <- with(tmp$subj, 100 / V * (exp(-CL / V * 19) + exp(-CL / V * 7)))
  d <- rbind(d, extra)
  d <- d[order(d$ID, d$TIME, -d$EVID), ]
  ## (AUCtau over 12-24 h is not compared to the analytical value here, since
  ## no route is specified, so C0 is not back-extrapolated for the IV bolus)
  nca_data <- run_nca(
    d,
    dictionary = dict_late_boundary,
    post = list(),
    verbose = FALSE
  )
  second <- nca_data[nca_data$nca_start == 12, ]
  expect_equal(second$ID, 1:5)
  expect_true(all(second$nca_end == 24))
  expect_true(all(!is.na(second$auctau)))
})
