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
  expect_equal(ncol(nca_data), 19)
  expect_equal(names(nca_data), c("USUBJID", "ACTARM", "GROUP", "ADM", "STUDYID", "EXROUTE", 
                                  "nca_start", "nca_end",
                                      "auclast", "aucall", "cmax", "tmax", "half.life", "lambda.z.n.points", 
                                      "aucinf.obs", "aucinf.pred", "cl.pred", "vz.pred", "nca_interval"))
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
  expect_equal(ncol(nca_data), 23)
  expect_equal(names(nca_data), c("USUBJID", "ACTARM", "GROUP", "ADM", "STUDYID", "EXROUTE", 
                                  "nca_start", "nca_end", "AUCLAST", "AUCALL", "CMAX", "TMAX", 
                                  "cl.all", "THALF", "adj.r.squared", "lambda.z", "KELNOPT", "aucinf.obs", 
                                  "AUCINF", "CLF", "VZF", "kel.pred", "nca_interval"))
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
  expect_equal(ncol(nca_data), 19)
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
  expect_equal(ncol(nca_data), 20)
  expect_equal(names(nca_data), c("USUBJID", "ACTARM", "GROUP", "ADM", "STUDYID", "EXROUTE", 
                                  "nca_start", "nca_end",
                                  "auclast", "aucall", "cmax", "tmax", "half.life", "lambda.z.n.points", 
                                  "aucinf.obs", "aucinf.pred", "cl.pred", "vz.pred",
                                  "auc_partial_0_4", "nca_interval"))
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
  expect_equal(ncol(nca_data), 20)
  expect_equal(names(nca_data), c("USUBJID", "ACTARM", "GROUP", "ADM", "STUDYID", "EXROUTE", 
                                  "nca_start", "nca_end",
                                  "auclast", "aucall", "cmax", "tmax", "half.life", "lambda.z.n.points", 
                                  "aucinf.obs", "aucinf.pred", "cl.pred", "vz.pred", "nca_interval", "auctau"))
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
  expect_equal(ncol(nca_data), 20)
  expect_equal(names(nca_data), c("USUBJID", "ACTARM", "GROUP", "ADM", "STUDYID", "EXROUTE", 
                                  "nca_start", "nca_end",
                                  "auclast", "aucall", "cmax", "tmax", "half.life", "lambda.z.n.points", 
                                  "aucinf.obs", "aucinf.pred", "cl.pred", "vz.pred", "nca_interval", "auctau"))
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
  expect_equal(ncol(nca_data), 21)
  expect_equal(names(nca_data), c("USUBJID", "ACTARM", "GROUP", "ADM", "STUDYID", "EXROUTE", 
                                  "nca_start", "nca_end",
                                  "auclast", "aucall", "cmax", "tmax", "half.life", "lambda.z.n.points", 
                                  "aucinf.obs", "aucinf.pred", "cl.pred", "vz.pred",
                                  "auc_partial_0_12", "nca_interval", "auctau"))
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
  expect_equal(ncol(nca_data), 19)
  expect_equal(names(nca_data), c("USUBJID", "ACTARM", "GROUP", "ADM", "STUDYID", "EXROUTE", 
                                  "nca_start", "nca_end",
                                  "auclast", "aucall", "cmax", "tmax", "half.life", "lambda.z.n.points", 
                                  "aucinf.obs", "aucinf.pred", "cl.pred", "vz.pred", "nca_interval"))
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
  expect_equal(ncol(nca_data), 20)
  expect_equal(names(nca_data), c("USUBJID", "ACTARM", "GROUP", "ADM", "STUDYID", "EXROUTE",
                                  "nca_start", "nca_end",
                                  "auclast", "aucall", "cmax", "tmax", "half.life", "lambda.z.n.points", 
                                  "aucinf.obs", "aucinf.pred", "cl.pred", "vz.pred", "nca_interval", "auctau"))
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
    expect_true(all(c("PPTESTCD", "PPORRESU") %in% names(units_attr)))
    expect_true(nrow(units_attr) > 0)
    expect_true(any(units_attr$PPORRESU != ""))

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

})
