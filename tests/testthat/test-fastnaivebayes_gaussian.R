context("Test Gaussian")

test_that("Predict", {
  y <- as.factor(c("Ham", "Ham", "Spam", "Spam", "Spam"))
  x <- matrix(
    c(2, 3, 2, 1, 2,
      5, 3, 4, 2, 4,
      0, 1, 3, 1, 0,
      3, 4, 4, 3, 5),
    nrow = 5,
    ncol = 4,
    dimnames = list(NULL, c("wo", "mo", "bo", "so")))
  df <- as.data.frame(x)

  # Data frame casting
  mod <- fnb.gaussian(x, y)
  df_mod <- fnb.gaussian(df, y)

  expect_error(fnb.gaussian(x[1:3,], y[1:3]))

  predictions <- predict(mod, x, type = "raw")
  risky_predictions <- predict(mod, x, type = "raw", check=FALSE)
  df_predictions <- predict(df_mod, df, type = "raw")

  expect_equal(sum(round(abs(predictions-df_predictions), digits = 12)), 0)
  expect_equal(sum(round(abs(predictions-risky_predictions), digits = 12)), 0)

  classification <- predict(mod, x, type = "class")
  expect_equal(as.factor(mod$levels[max.col(predictions)]), classification)

  # Column padding
  expect_warning(predict(mod, x[,1:3], type = "raw"))
  dropped_predictions <- predict(mod, x[,1:3], type = "raw", silent = TRUE)

  dropped_x <- x[,1:3]
  mod <- fnb.gaussian(dropped_x, y)
  alt_predictions <- predict(mod, x, type = "raw", silent=TRUE)

  expect_equal(sum(round(abs(dropped_predictions-alt_predictions), digits = 12)), 0)

  # Ignore new column
  mod <- fnb.gaussian(x, y)
  predictions <- predict(mod, x, type = "raw")

  x <- cbind(x, x[,1, drop=FALSE])
  colnames(x)[5] <- "womo"

  new_predictions <- predict(mod, x, type = "raw", silent=TRUE)
  expect_equal(sum(round(abs(predictions-new_predictions), digits = 12)), 0)

  # All new columns is same as all 0
  all_new_columns_predictions <- predict(mod, x[,5,drop=FALSE], type="raw", silent = TRUE)
  predictions <- matrix(
    c(2/5,2/5,2/5,2/5,2/5,
      3/5,3/5,3/5,3/5,3/5),
    nrow = 5,
    ncol = 2
  )
  expect_equal(sum(round(abs(predictions-all_new_columns_predictions), digits = 12)), 0)
})

test_that("Standard 3 classes", {
  y <- as.factor(c("Ham", "Ham", "Spam", "Spam", "Spam"))
  x <- matrix(
    c(2, 3, 2, 1, 2,
      5, 3, 4, 2, 4,
      0, 1, 3, 1, 0,
      3, 4, 4, 3, 5),
    nrow = 5,
    ncol = 4,
    dimnames = list(NULL, c("wo", "mo", "bo", "so")))

  actuals <- matrix(
    c(
      0.668632030, 0.3313680,
      0.793288288, 0.2067117,
      0.002007792, 0.9979922,
      0.092781300, 0.9072187,
      0.127527893, 0.8724721
    ),
    nrow = 5,
    ncol = 2,
    byrow=TRUE,
    dimnames = list(NULL, c("Ham", "Spam"))
  )

  mod <- fnb.gaussian(x, y, priors = c(1/4, 3/4))

  predictions <- predict(mod, x, type="raw")

  expect_equal(sum(round(abs(predictions-actuals), digits = 7)), 0)

  # Test Sparse Matrices
  sparse_mod <- fnb.gaussian(Matrix(x, sparse = TRUE), y, priors = c(1/4, 3/4))
  sparse_cast_mod <- fnb.gaussian(x, y, sparse = TRUE, priors = c(1/4, 3/4))

  sparse_predictions <- predict(sparse_mod, x, type = "raw")
  sparse_cast_predictions <- predict(sparse_cast_mod, x, type = "raw")

  expect_equal(sum(round(abs(predictions-sparse_predictions), digits = 12)), 0)
  expect_equal(sum(round(abs(predictions-sparse_cast_predictions), digits = 12)), 0)

})

test_that("Single column",{
  y <- as.factor(c( "Spam", "Spam", "Ham", "Ham", "Ham"))
  x <- matrix(
    c(2, 3, 2, 1, 2),
    nrow = 5,
    ncol = 1,
    dimnames = list(NULL, c("ho"))
  )

  actuals_ho_only <- matrix(
    c(
      0.6663074, 0.3336926,
      0.1408233, 0.8591767,
      0.6663074, 0.3336926,
      0.8994864, 0.1005136,
      0.6663074, 0.3336926
    ),
    nrow = 5,
    ncol = 2,
    byrow = TRUE,
    dimnames = list(NULL, c("Ham", "Spam"))
  )

  mod <- fnb.gaussian(x[, 1, drop=FALSE], y)
  predictions <- predict(mod, x[, 1, drop=FALSE], type="raw")

  expect_equal(sum(round(abs(predictions-actuals_ho_only), digits = 7)), 0)

  # Test Sparse Matrices
  sparse_mod <- fnb.gaussian(Matrix(x[, 1, drop=FALSE], sparse = TRUE), y)
  sparse_cast_mod <- fnb.gaussian(x[, 1, drop=FALSE], y, sparse = TRUE)

  sparse_predictions <- predict(sparse_mod, x[, 1, drop=FALSE], type = "raw")
  sparse_cast_predictions <- predict(sparse_cast_mod, x[, 1, drop=FALSE], type = "raw")

  expect_equal(sum(round(abs(predictions-sparse_predictions), digits = 12)), 0)
  expect_equal(sum(round(abs(predictions-sparse_cast_predictions), digits = 12)), 0)

})
