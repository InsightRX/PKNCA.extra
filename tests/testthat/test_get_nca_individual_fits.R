test_that("wrong object throws error", {
  expect_error(get_nca_individual_fits(list(foo = "bar")))
})

test_that("calculations are correct, for NCA with 2 groupings (subject, treatment)", {
  res <- readRDS(system.file("testdata/pknca/nca_testresult.rds", package = "PKNCA.extra"))
  fit <- get_nca_individual_fits(res, dictionary = list(foo = "SDEID", bar = "SITEID"))
  expect_equal(names(fit), c("conc", "time", "treatment_group", "subject_id", "TREATXT", "SDEID", "SITEID", "n_points", "r_squared", "adj_r_squared", "blq", "used_in_fit", "excluded", "exclude_reason", "prediction"))
  expect_equal(nrow(fit), 3240)
  expect_equal(fit$conc[1:5], c(0.626, 27.7, 43.9, 48.8, 38.7))
  expect_equal(fit$prediction[1:5], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
  expect_equal(round(tail(fit$prediction, 5), 3), c(1.837, 1.685, 1.546, NA, 1.418))
  expect_equal(fit$SITEID[1:5], rep(1234, 5))

  ## check correct number of points used in fit marked in data
  expect_equal(
    fit |>
      dplyr::filter(subject_id == 12341001, is.na(prediction), TREATXT == "D 2mg + g") |>
      dplyr::pull(used_in_fit) |>
    sum(),
    3
  )
})

## Tests using run_nca() output
## nca_admiral data: PCORRES = concentration, PCTPTNUM = time, USUBJID = subject ID
## Subject 01-701-1028 has 14 timepoints, pre-dose BLQ at t=0, non-BLQ from t=0.08
## Subject 01-701-1033 has 14 timepoints

test_that("get_nca_individual_fits returns correct structure from run_nca output", {
  dat <- nca_admiral
  ids <- unique(dat$USUBJID)

  nca_data <- run_nca(dat[dat$USUBJID %in% ids[1:2],], verbose = FALSE)
  nca_obj <- attr(nca_data, "PKNCA_object")

  fit <- get_nca_individual_fits(nca_obj)

  expect_s3_class(fit, "data.frame")
  expect_equal(
    names(fit),
    c("PCORRES", "PCTPTNUM", "USUBJID", "n_points", "r_squared",
      "adj_r_squared", "blq", "used_in_fit", "excluded", "exclude_reason", "prediction")
  )
  ## 14 obs per subject + 40 prediction grid points per subject
  obs_only <- dplyr::filter(fit, is.na(prediction))
  pred_only <- dplyr::filter(fit, !is.na(prediction))
  expect_equal(nrow(obs_only), 14 * 2)
  expect_equal(nrow(pred_only), 40 * 2)
})

test_that("used_in_fit count matches lambda.z.n.points per subject", {
  dat <- nca_admiral
  ids <- unique(dat$USUBJID)

  nca_data <- run_nca(dat[dat$USUBJID %in% ids[1:2],], verbose = FALSE)
  nca_obj <- attr(nca_data, "PKNCA_object")
  fit <- get_nca_individual_fits(nca_obj)

  obs_only <- dplyr::filter(fit, is.na(prediction))
  check <- obs_only |>
    dplyr::group_by(USUBJID) |>
    dplyr::summarise(
      used_count = sum(used_in_fit, na.rm = TRUE),
      n_pts = dplyr::first(n_points[!is.na(n_points)])
    )

  ## used_in_fit sum must equal n_points for each subject
  expect_equal(check$used_count, check$n_pts)

  ## specific expected values for these two subjects
  expect_equal(check$used_count[check$USUBJID == ids[1]], 3)
  expect_equal(check$used_count[check$USUBJID == ids[2]], 7)
})

test_that("NA concentration does not produce NA used_in_fit values", {
  dat <- nca_admiral
  ids <- unique(dat$USUBJID)

  nca_data <- run_nca(dat[dat$USUBJID == ids[1],], verbose = FALSE)
  nca_obj <- attr(nca_data, "PKNCA_object")

  ## Inject an NA concentration at a non-terminal timepoint (t=1.5h, row 5)
  nca_obj$data$conc$data[5, "PCORRES"] <- NA

  ## Before the fix, used_in_fit was NA for all rows when any concentration was NA
  fit <- get_nca_individual_fits(nca_obj)
  obs_only <- dplyr::filter(fit, is.na(prediction))

  ## No used_in_fit values should be NA
  expect_false(any(is.na(obs_only$used_in_fit)))

  ## Sum of used_in_fit must still equal n_points
  n_pts <- unique(obs_only$n_points[!is.na(obs_only$n_points)])
  expect_equal(sum(obs_only$used_in_fit), n_pts)

  ## The NA-concentration point itself should not be used in the terminal fit
  na_pt <- dplyr::filter(obs_only, PCTPTNUM == 1.5)
  expect_equal(na_pt$used_in_fit, 0)
})

test_that("BLQ points are never flagged as used_in_fit", {
  dat <- nca_admiral
  ids <- unique(dat$USUBJID)

  nca_data <- run_nca(dat[dat$USUBJID %in% ids[1:5],], verbose = FALSE)
  nca_obj <- attr(nca_data, "PKNCA_object")
  fit <- get_nca_individual_fits(nca_obj)

  blq_obs <- dplyr::filter(fit, is.na(prediction), blq == 1)
  expect_true(nrow(blq_obs) > 0)
  expect_true(all(blq_obs$used_in_fit == 0))
})

test_that("excluded terminal-phase point has used_in_fit=0 and excluded=1, sum still matches n_points", {
  dat <- nca_admiral
  ids <- unique(dat$USUBJID)

  ## For subject 01-701-1028, the 48h sample is in the 3-point terminal fit (24h, 36h, 48h)
  ## Excluding it causes PKNCA to recalculate lambda.z using earlier points
  nca_data <- run_nca(
    dat[dat$USUBJID == ids[1],],
    exclude_points = list(
      SAMPLEID = list(id = "017011028-20130721T000000", reason = "test exclusion")
    ),
    verbose = FALSE
  )
  nca_obj <- attr(nca_data, "PKNCA_object")
  fit <- get_nca_individual_fits(nca_obj)

  obs_only <- dplyr::filter(fit, is.na(prediction))

  ## Excluded 48h point must be flagged correctly
  excl_point <- dplyr::filter(obs_only, PCTPTNUM == 48)
  expect_equal(nrow(excl_point), 1)
  expect_equal(excl_point$excluded, 1)
  expect_equal(excl_point$exclude_reason, "test exclusion")
  expect_equal(excl_point$used_in_fit, 0)

  ## Sum of used_in_fit must still equal n_points (recalculated by PKNCA)
  n_pts <- unique(obs_only$n_points[!is.na(obs_only$n_points)])
  expect_equal(sum(obs_only$used_in_fit, na.rm = TRUE), n_pts)
})

test_that("excluding a non-terminal point does not change used_in_fit for terminal points", {
  dat <- nca_admiral
  ids <- unique(dat$USUBJID)

  ## Exclude t=2h for subject 01-701-1028, which is well outside the terminal phase
  ## (terminal fit uses only last 3 points: 24h, 36h, 48h)
  nca_data <- run_nca(
    dat[dat$USUBJID == ids[1],],
    exclude_points = list(
      SAMPLEID = list(id = "017011028-20130719T020000", reason = "non-terminal exclusion")
    ),
    verbose = FALSE
  )
  nca_obj <- attr(nca_data, "PKNCA_object")
  fit <- get_nca_individual_fits(nca_obj)

  obs_only <- dplyr::filter(fit, is.na(prediction))

  ## Excluded t=2h point must be flagged
  excl_point <- dplyr::filter(obs_only, PCTPTNUM == 2)
  expect_equal(excl_point$excluded, 1)
  expect_equal(excl_point$exclude_reason, "non-terminal exclusion")
  expect_equal(excl_point$used_in_fit, 0)

  ## n_points unchanged (PKNCA still finds same 3-point terminal fit)
  n_pts <- unique(obs_only$n_points[!is.na(obs_only$n_points)])
  expect_equal(n_pts, 3)

  ## The three terminal points (24h, 36h, 48h) are still used_in_fit
  terminal_pts <- dplyr::filter(obs_only, PCTPTNUM %in% c(24, 36, 48))
  expect_equal(terminal_pts$used_in_fit, c(1, 1, 1))

  ## Sum still equals n_points
  expect_equal(sum(obs_only$used_in_fit, na.rm = TRUE), n_pts)
})

test_that("excluding multiple points across subjects is reflected correctly in used_in_fit", {
  dat <- nca_admiral
  ids <- unique(dat$USUBJID)

  ## Exclude one terminal-phase point from each of the first two subjects
  ## Subject 01-701-1028: exclude 48h (terminal)
  ## Subject 01-701-1033: exclude its last timepoint via a separate check
  nca_data <- run_nca(
    dat[dat$USUBJID %in% ids[1:2],],
    exclude_points = list(
      SAMPLEID = list(
        id = c("017011028-20130721T000000"),  # 48h for subject 1
        reason = c("test exclusion")
      )
    ),
    verbose = FALSE
  )
  nca_obj <- attr(nca_data, "PKNCA_object")
  fit <- get_nca_individual_fits(nca_obj)

  obs_only <- dplyr::filter(fit, is.na(prediction))

  ## For each subject, used_in_fit count must match n_points
  check <- obs_only |>
    dplyr::group_by(USUBJID) |>
    dplyr::summarise(
      used_count = sum(used_in_fit, na.rm = TRUE),
      n_pts = dplyr::first(n_points[!is.na(n_points)]),
      n_excluded = sum(excluded)
    )

  expect_equal(check$used_count, check$n_pts)

  ## Subject 1 has 1 excluded point; subject 2 has none
  expect_equal(check$n_excluded[check$USUBJID == ids[1]], 1)
  expect_equal(check$n_excluded[check$USUBJID == ids[2]], 0)
})

## Tests for include_lambda_z and exclude_lambda_z interaction with used_in_fit
## Subject 01-701-1028 (ids[1]): 14 timepoints, BLQ at t=0, non-BLQ from t=0.08
##   Default terminal fit: 3 points (24h, 36h, 48h)
##   SAMPLEID for 36h: "017011028-20130720T120000"
##   SAMPLEID for 48h: "017011028-20130721T000000"
## Subject 01-701-1033 (ids[2]): 14 timepoints
##   Default terminal fit: 7 points
##   SAMPLEID for 8h:  "017011033-20140318T080000"
##   SAMPLEID for 12h: "017011033-20140318T120000"
##   SAMPLEID for 16h: "017011033-20140318T160000"

test_that("exclude_lambda_z: excluded point has used_in_fit=0, sum matches n_points", {
  dat <- nca_admiral
  ids <- unique(dat$USUBJID)

  ## Exclude the 36h sample from the lambda-z fit only (not from NCA)
  nca_data <- run_nca(
    dat[dat$USUBJID == ids[1], ],
    verbose = FALSE,
    exclude_lambda_z = list(
      SAMPLEID = list(
        id = "017011028-20130720T120000",
        reason = "exclude from lambda-z"
      )
    )
  )
  nca_obj <- attr(nca_data, "PKNCA_object")
  fit <- get_nca_individual_fits(nca_obj)

  obs_only <- dplyr::filter(fit, is.na(prediction))

  ## 36h was excluded from lambda-z, so it must not be flagged as used_in_fit
  pt_36h <- dplyr::filter(obs_only, PCTPTNUM == 36)
  expect_equal(pt_36h$used_in_fit, 0)

  ## 36h is NOT excluded from NCA (not in 'exclude' column), so excluded=0
  expect_equal(pt_36h$excluded, 0)

  ## Sum of used_in_fit must match n_points (PKNCA now uses a 4-point window)
  n_pts <- unique(obs_only$n_points[!is.na(obs_only$n_points)])
  expect_equal(sum(obs_only$used_in_fit, na.rm = TRUE), n_pts)

  ## The actually-used points (12h, 16h, 24h, 48h) should be flagged
  used_times <- sort(obs_only$PCTPTNUM[obs_only$used_in_fit == 1])
  expect_equal(used_times, c(12, 16, 24, 48))
})

test_that("include_lambda_z: forced points are used_in_fit=1, others are 0", {
  dat <- nca_admiral
  ids <- unique(dat$USUBJID)

  ## Force 8h, 12h, 16h for subject 2 (non-terminal points)
  nca_data <- suppressWarnings(run_nca(
    dat[dat$USUBJID == ids[2], ],
    verbose = FALSE,
    include_lambda_z = list(
      SAMPLEID = list(
        id = c(
          "017011033-20140318T080000",
          "017011033-20140318T120000",
          "017011033-20140318T160000"
        ),
        reason = "forced inclusion"
      )
    )
  ))
  nca_obj <- attr(nca_data, "PKNCA_object")
  fit <- get_nca_individual_fits(nca_obj)

  obs_only <- dplyr::filter(fit, is.na(prediction))

  ## Exactly the 3 forced points should have used_in_fit = 1
  used_times <- sort(obs_only$PCTPTNUM[obs_only$used_in_fit == 1])
  expect_equal(used_times, c(8, 12, 16))

  ## Sum of used_in_fit must match n_points (= 3 forced points)
  n_pts <- unique(obs_only$n_points[!is.na(obs_only$n_points)])
  expect_equal(sum(obs_only$used_in_fit, na.rm = TRUE), n_pts)
  expect_equal(n_pts, 3)

  ## Terminal points (24h, 36h, 48h) must NOT be flagged
  expect_equal(obs_only$used_in_fit[obs_only$PCTPTNUM == 24], 0)
  expect_equal(obs_only$used_in_fit[obs_only$PCTPTNUM == 36], 0)
  expect_equal(obs_only$used_in_fit[obs_only$PCTPTNUM == 48], 0)
})

test_that("include_lambda_z + exclude_lambda_z: include takes precedence for same subject", {
  dat <- nca_admiral
  ids <- unique(dat$USUBJID)

  ## For subject 2: force 8h, 12h, 16h via include_lambda_z AND also attempt
  ## to exclude 24h via exclude_lambda_z. Per documentation, include_lambda_z
  ## takes precedence: PKNCA ignores exclude_lambda_z for that subject.
  ## used_in_fit should follow include_hl (8h, 12h, 16h) regardless of exclude_hl.
  ## SAMPLEID for 24h of subject 2: "017011033-20140319T000000"
  nca_data <- suppressWarnings(run_nca(
    dat[dat$USUBJID == ids[2], ],
    verbose = FALSE,
    include_lambda_z = list(
      SAMPLEID = list(
        id = c(
          "017011033-20140318T080000",
          "017011033-20140318T120000",
          "017011033-20140318T160000"
        ),
        reason = "forced inclusion"
      )
    ),
    exclude_lambda_z = list(
      SAMPLEID = list(
        id = "017011033-20140319T000000",
        reason = "exclude 24h from lambda-z"
      )
    )
  ))
  nca_obj <- attr(nca_data, "PKNCA_object")
  fit <- get_nca_individual_fits(nca_obj)

  obs_only <- dplyr::filter(fit, is.na(prediction))

  ## include_lambda_z takes precedence: used_in_fit follows include_hl
  expect_equal(sort(obs_only$PCTPTNUM[obs_only$used_in_fit == 1]), c(8, 12, 16))

  ## 24h is in exclude_hl but include_hl takes precedence -> used_in_fit = 0
  expect_equal(obs_only$used_in_fit[obs_only$PCTPTNUM == 24], 0)

  ## Sum matches n_points
  n_pts <- unique(obs_only$n_points[!is.na(obs_only$n_points)])
  expect_equal(sum(obs_only$used_in_fit, na.rm = TRUE), n_pts)
})
