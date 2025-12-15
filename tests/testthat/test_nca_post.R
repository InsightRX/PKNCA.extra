test_that("nca_post_accumulation handles multiple intervals correctly", {
  # Test data with multiple intervals
  data <- data.frame(
    USUBJID = rep(c("001", "002"), each = 3),
    nca_start = c(0, 24, 48,    # Subject 001
                  0, 24, 48),     # Subject 002
    nca_end = c(24, 48, 72,     # Subject 001
                24, 48, 72),     # Subject 002
    auctau = c(100, 200, 300,  # Subject 001
                150, 225, 450),  # Subject 002
    cmax = c(10, 15, 20,        # Subject 001
             15, 20, 30)         # Subject 002
  )
  
  dictionary <- list(subject_id = "USUBJID")
  
  result <- nca_post_accumulation(data, dictionary)
  
  # Check structure
  expect_equal(ncol(result), ncol(data) + 2)  # Original cols + 2 accumulation cols
  expect_true(all(c("accum_auctau", "accum_cmax") %in% names(result)))
  
  # Check calculations for first subject
  subj1 <- result[result$USUBJID == "001", ]
  expect_equal(subj1$accum_auctau, c(1, 2, 3))          # 100/100, 200/100, 300/100
  expect_equal(subj1$accum_cmax, c(1, 1.5, 2))           # 10/10, 15/10, 20/10
  
  # Check calculations for second subject
  subj2 <- result[result$USUBJID == "002", ]
  expect_equal(subj2$accum_auctau, c(1, 1.5, 3))        # 150/150, 225/150, 450/150
  expect_equal(subj2$accum_cmax, c(1, 1.333, 2), tolerance = 0.01) # 15/15, 20/15, 30/15
})

test_that("nca_post_accumulation handles single interval correctly", {
  # Test data with single interval
  data <- data.frame(
    USUBJID = c("001", "002"),
    nca_start = c(0, 0),
    nca_end = c(24, 24),
    auctau = c(100, 150),
    cmax = c(10, 15)
  )
  
  dictionary <- list(subject_id = "USUBJID")
  
  result <- nca_post_accumulation(data, dictionary)
  
  # Should return original data unchanged
  expect_equal(result, data)
})

test_that("nca_post_accumulation handles grouping correctly", {
  # Test data with treatment groups
  data <- data.frame(
    USUBJID = rep(c("001", "002"), each = 4),
    ACTARM = rep(c("A", "B"), each = 2, times = 2),
    nca_start = rep(c(0, 24), 4),
    nca_end = rep(c(24, 48), 4),
    auctau = c(100, 200, 150, 300, 100, 250, 120, 360),
    cmax = c(10, 20, 15, 30, 10, 25, 12, 36)
  )
  
  dictionary <- list(subject_id = "USUBJID")
  
  result <- nca_post_accumulation(
    data, 
    dictionary,
    groups = "ACTARM"
  )
  
  # Check that ratios are calculated within treatment groups
  subj1_armA <- result %>% 
    dplyr::filter(USUBJID == "001", ACTARM == "A")
  expect_equal(subj1_armA$accum_auctau, c(1, 2))
  
  subj1_armB <- result %>% 
    dplyr::filter(USUBJID == "001", ACTARM == "B")
  expect_equal(subj1_armB$accum_auctau, c(1, 2))
})

test_that("nca_post_accumulation handles custom parameters correctly", {
  data <- data.frame(
    USUBJID = rep("001", 2),
    nca_start = c(0, 24),
    nca_end = c(24, 48),
    auclast = c(100, 200),
    cmax = c(10, 20),
    custom_param = c(5, 15)
  )
  
  dictionary <- list(subject_id = "USUBJID")
  
  result <- nca_post_accumulation(
    data, 
    dictionary,
    options = list(parameters = c("custom_param"))
  )
  
  expect_true("accum_custom_param" %in% names(result))
  expect_equal(result$accum_custom_param, c(1, 3))  # 5/5, 15/5
})

test_that("nca_post_accumulation handles errors correctly", {
  
  # Missing parameter column
  data_missing_param <- data.frame(
    USUBJID = rep("001", 2),
    nca_start = c(0, 24),
    nca_end = c(24, 48)
    # Missing auclast and cmax
  )
  
  expect_error(
    nca_post_accumulation(data_missing_param, dictionary)
  )
  
})

test_that("nca_post_accumulation handles mixed interval counts correctly", {
  # Test data where some subjects have multiple intervals and others don't
  data <- data.frame(
    USUBJID = c(rep("001", 3), "002"),
    nca_start = c(0, 24, 48, 0),
    nca_end = c(24, 48, 72, 24),
    auctau = c(100, 200, 300, 150),
    cmax = c(10, 20, 30, 15)
  )
  
  dictionary <- list(subject_id = "USUBJID")
  
  result <- nca_post_accumulation(data, dictionary)
  
  # Check subject with multiple intervals
  subj1 <- result[result$USUBJID == "001", ]
  expect_equal(subj1$accum_auctau, c(1, 2, 3))
  
  # Check subject with single interval
  subj2 <- result[result$USUBJID == "002", ]
  expect_equal(subj2$accum_auctau, 1)
})

test_that("nca_post_bioavailability calculates ratios correctly", {
  # Test data
  data <- data.frame(
    USUBJID = rep(c("51", "52"), each = 3),
    ARM = rep(c("fasted", "fed", "semi-fed"), 2),
    auclast = c(100, 200, 150,  # Subject 51
                150, 300, 225),  # Subject 52
    cmax = c(10, 20, 15,        # Subject 51
             15, 30, 22.5)      # Subject 52
  )
  
  dictionary <- list(subject_id = "USUBJID")
  
  result <- nca_post_bioavailability(
    data,
    dictionary = dictionary,
    options = list(
      parameters = c("auclast", "cmax"),
      arm = "ARM",
      reference = "fasted"
    )
  )
  
  # Check structure
  expect_equal(ncol(result), ncol(data) + 2)  # Original cols + 2 ratio cols
  expect_true(all(c("relbio_auclast", "relbio_cmax") %in% names(result)))
  
  # Check calculations for first subject
  subj1 <- result %>% 
    dplyr::filter(USUBJID == "51")
  expect_equal(subj1$relbio_auclast, c(1, 2, 1.5))  # 100/100, 200/100, 150/100
  expect_equal(subj1$relbio_cmax, c(1, 2, 1.5))     # 10/10, 20/10, 15/10
})

test_that("nca_post_bioavailability handles missing columns gracefully", {
  # Missing parameter column
  data1 <- data.frame(
    USUBJID = c("51", "52"),
    ARM = c("fasted", "fed"),
    auclast = c(100, 200)
    # Missing cmax
  )
  
  dictionary <- list(subject_id = "USUBJID")
  
  result1 <- nca_post_bioavailability(
    data1,
    dictionary = dictionary,
    options = list(
      parameters = c("auclast", "cmax"),
      arm = "ARM",
      reference = "fasted"
    )
  )
  
  # Should return original data unchanged
  expect_equal(result1, data1)
  
  # Missing arm column
  data2 <- data.frame(
    USUBJID = c("51", "52"),
    auclast = c(100, 200),
    cmax = c(10, 20)
  )
  
  result2 <- nca_post_bioavailability(
    data2,
    dictionary = dictionary,
    options = list(
      parameters = c("auclast", "cmax"),
      arm = "ARM",
      reference = "fasted"
    )
  )
  
  # Should return original data unchanged
  expect_equal(result2, data2)
})

test_that("nca_post_bioavailability handles missing reference treatment gracefully", {
  data <- data.frame(
    USUBJID = c("51", "52"),
    ARM = c("test1", "test2"),
    auclast = c(100, 200),
    cmax = c(10, 20)
  )
  
  dictionary <- list(subject_id = "USUBJID")
  
  result <- nca_post_bioavailability(
    data,
    dictionary = dictionary,
    options = list(
      parameters = c("auclast", "cmax"),
      arm = "ARM",
      reference = "fasted"  # Doesn't exist in data
    )
  )
  
  # Should return original data unchanged
  expect_equal(result, data)
})

test_that("nca_post_bioavailability handles empty parameters gracefully", {
  data <- data.frame(
    USUBJID = c("51", "52"),
    ARM = c("fasted", "fed"),
    auclast = c(100, 200),
    cmax = c(10, 20)
  )
  
  dictionary <- list(subject_id = "USUBJID")
  
  # Empty parameters list
  result1 <- nca_post_bioavailability(
    data,
    dictionary = dictionary,
    options = list(
      parameters = character(0),
      arm = "ARM",
      reference = "fasted"
    )
  )
  
  # Should return original data unchanged
  expect_equal(result1, data)
  
  # NULL parameters
  result2 <- nca_post_bioavailability(
    data,
    dictionary = dictionary,
    options = list(
      parameters = NULL,
      arm = "ARM",
      reference = "fasted"
    )
  )
  
  # Should return original data unchanged
  expect_equal(result2, data)
})

test_that("nca_post_bioavailability handles NULL arm specification gracefully", {
  data <- data.frame(
    USUBJID = c("51", "52"),
    ARM = c("fasted", "fed"),
    auclast = c(100, 200),
    cmax = c(10, 20)
  )
  
  dictionary <- list(subject_id = "USUBJID")
  
  result <- nca_post_bioavailability(
    data,
    dictionary = dictionary,
    options = list(
      parameters = c("auclast", "cmax"),
      arm = NULL,
      reference = "fasted"
    )
  )
  
  # Should return original data unchanged
  expect_equal(result, data)
})

test_that("nca_post_bioavailability preserves additional columns", {
  data <- data.frame(
    USUBJID = rep(c("51", "52"), each = 2),
    ARM = rep(c("fasted", "fed"), 2),
    auclast = c(100, 200, 150, 300),
    cmax = c(10, 20, 15, 30),
    extra_col = rep("test", 4),
    group = rep("A", 4)
  )
  
  dictionary <- list(subject_id = "USUBJID")
  
  result <- nca_post_bioavailability(
    data,
    dictionary = dictionary,
    options = list(
      parameters = c("auclast", "cmax"),
      arm = "ARM",
      reference = "fasted"
    ),
    groups = "group"
  )
  
  # Check that additional columns are preserved
  expect_true(all(c("extra_col", "group") %in% names(result)))
  expect_equal(result$extra_col, data$extra_col)
  expect_equal(result$group, data$group)
})

