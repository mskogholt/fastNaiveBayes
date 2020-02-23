context("Test Poisson")

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
  mod <- fnb.poisson(x, y)
  df_mod <- fnb.poisson(df, y)

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
  mod <- fnb.poisson(dropped_x, y)
  alt_predictions <- predict(mod, x, type = "raw", silent=TRUE)

  expect_equal(sum(round(abs(dropped_predictions-alt_predictions), digits = 12)), 0)

  # Ignore new column
  mod <- fnb.poisson(x, y)
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
      0.67913774, 0.3208623,
      0.41976782, 0.5802322,
      0.07526245, 0.9247376,
      0.23443299, 0.7655670,
      0.57454707, 0.4254529
    ),
    nrow = 5,
    ncol = 2,
    byrow=TRUE,
    dimnames = list(NULL, c("Ham", "Spam"))
  )

  mod <- fnb.poisson(x, y, priors = c(2/5, 3/5))

  predictions <- predict(mod, x, type="raw")

  expect_equal(sum(round(abs(predictions-actuals), digits = 7)), 0)

  # Test Sparse Matrices
  sparse_mod <- fnb.poisson(Matrix(x, sparse = TRUE), y, priors = c(2/5, 3/5))
  sparse_cast_mod <- fnb.poisson(x, y, sparse = TRUE, priors = c(2/5, 3/5))

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
        0.6053645, 0.3946355,
        0.5056005, 0.4943995,
        0.6053645, 0.3946355,
        0.6970593, 0.3029407,
        0.6053645, 0.3946355
    ),
    nrow = 5,
    ncol = 2,
    byrow = TRUE,
    dimnames = list(NULL, c("Ham", "Spam"))
  )

  mod <- fnb.poisson(x[, 1, drop=FALSE], y)
  predictions <- predict(mod, x[, 1, drop=FALSE], type="raw")

  expect_equal(sum(round(abs(predictions-actuals_ho_only), digits = 7)), 0)

  # Test Sparse Matrices
  sparse_mod <- fnb.poisson(Matrix(x[, 1, drop=FALSE], sparse = TRUE), y)
  sparse_cast_mod <- fnb.poisson(x[, 1, drop=FALSE], y, sparse = TRUE)

  sparse_predictions <- predict(sparse_mod, x[, 1, drop=FALSE], type = "raw")
  sparse_cast_predictions <- predict(sparse_cast_mod, x[, 1, drop=FALSE], type = "raw")

  expect_equal(sum(round(abs(predictions-sparse_predictions), digits = 12)), 0)
  expect_equal(sum(round(abs(predictions-sparse_cast_predictions), digits = 12)), 0)

})
