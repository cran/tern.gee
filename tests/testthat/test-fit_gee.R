# build_formula ----

test_that("build_formula builds the correct formula", {
  vars <- list(response = "AVAL", id = "USUBJID", arm = "ARMCD", visit = "AVISIT", covariates = c("RACE", "SEX"))
  result <- as.character(build_formula(vars))
  expected <- as.character(AVAL ~ ARMCD + RACE + SEX)
  expect_identical(result, expected)
})

test_that("build_formula works without covariates", {
  result <- expect_silent(build_formula(vars_gee()))
  expected <- AVAL ~ ARM
  expect_equal(result, expected, ignore_attr = TRUE)
})

# build_family ----

test_that("build_family returns the correct class", {
  result <- build_family("logistic")
  expected <- "tern_gee_logistic"
  expect_identical(result$class, expected)
  expect_class(result$object, "family")
})

test_that("build_family gives error message if incorrect regression type is given", {
  expect_error(build_family("BLAHHH"), "regression type BLAHHH not supported")
})

# build_cor_details ----

test_that("build_cor_details returns the correct correlation details", {
  result <- as.character(build_cor_details("unstructured", fev_vars, fev_data))
  expected <- c("unstructured", "1")
  expect_identical(result, expected)
})

test_that("build_cor_details gives error message if incorrect correlation structure is given", {
  expect_error(build_cor_details("BLAHHH", fev_vars, fev_data), "correlation structure BLAHHH not available")
})

# order_data ----

test_that("order_data works as expected", {
  vars <- list(id = "USUBJID", visit = "AVISIT")
  df <- fev_data[c(7, 2, 9, 3, 1, 5, 8, 6, 4, 10), ]
  expect_factor(df$USUBJID)
  result <- expect_silent(order_data(data = df, vars = vars))
  expect_integer(result$USUBJID, sorted = TRUE)
  result_order <- order(result$USUBJID, result$AVISIT)
  expect_identical(result_order, seq_len(nrow(result)))
})

test_that("order_data messages are as expected and can be suppressed for character visit variable", {
  vars <- list(id = "USUBJID", visit = "AVISIT")
  df <- fev_data[c(7, 2, 9, 3, 1, 5, 8, 6, 4, 10), ]
  df$AVISIT <- as.character(df$AVISIT)
  expect_message(
    expect_message(
      expect_message(
        result <- order_data(data = df, vars = vars),
        "visit variable AVISIT will be coerced to factor for ordering"
      ),
      "order is:"
    ),
    "VIS1, VIS2, VIS3, VIS4"
  )
  expect_integer(result$USUBJID, sorted = TRUE)
  result_order <- order(result$USUBJID, result$AVISIT)
  expect_identical(result_order, seq_len(nrow(result)))
})

# fit_gee ----

## logistic ----

test_that("fit_gee works as expected for unstructured correlation structure", {
  result <- expect_silent(fit_gee(
    vars = fev_vars,
    data = fev_data,
    regression = "logistic",
    cor_struct = "unstructured"
  ))
  expect_class(result, c("tern_gee_logistic", "tern_gee", "geelm"))
})

test_that("fit_gee works as expected for AR1 correlation structure", {
  result <- expect_silent(fit_gee(
    vars = fev_vars,
    data = fev_data,
    regression = "logistic",
    cor_struct = "auto-regressive"
  ))
  expect_class(result, c("tern_gee_logistic", "tern_gee", "geelm"))
})

test_that("fit_gee works as expected for compound symmetry correlation structure", {
  result <- expect_silent(fit_gee(
    vars = fev_vars,
    data = fev_data,
    regression = "logistic",
    cor_struct = "compound symmetry"
  ))
  expect_class(result, c("tern_gee_logistic", "tern_gee", "geelm"))
})

test_that("fit_gee works as expected for Toeplitz correlation structure", {
  result <- expect_silent(fit_gee(
    vars = fev_vars,
    data = fev_data,
    regression = "logistic",
    cor_struct = "toeplitz"
  ))
  expect_class(result, c("tern_gee_logistic", "tern_gee", "geelm"))
})
