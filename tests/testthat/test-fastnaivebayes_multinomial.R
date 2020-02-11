context("Test Multinomial")

test_that("Predict", {
  # Test 1
  y <- as.factor(c("Ham", "Ham", "Spam", "Spam", "Spam"))
  x <- matrix(
    c(1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1),
    nrow = 5,
    ncol = 4,
    dimnames = list(NULL, c("wo", "mo", "bo", "so")))
  df <- as.data.frame(x)

  # Data frame casting
  mod <- fnb.multinomial(x, y, laplace = 1)
  df_mod <- fnb.multinomial(df, y, laplace = 1)

  predictions <- predict(mod, x, type = "raw")
  df_predictions <- predict(df_mod, df, type = "raw")

  expect_equal(sum(round(abs(predictions-df_predictions), digits = 12)), 0)

  classification <- predict(mod, x, type = "class")
  expect_equal(as.factor(mod$levels[max.col(predictions)]), classification)

  # Column padding
  expect_warning(predict(mod, x[,1:3], type = "raw"))
  dropped_predictions <- predict(mod, x[,1:3], type = "raw", silent = TRUE)
  dropped_alt_predictions <- predict(mod, x[,1:3], type = "raw", silent = TRUE, sparse = TRUE)

  x[,4] <- 0
  alt_predictions <- predict(mod, x, type = "raw")

  expect_equal(sum(round(abs(dropped_predictions-alt_predictions), digits = 12)), 0)
  expect_equal(sum(round(abs(dropped_predictions-dropped_alt_predictions), digits = 12)), 0)

  # Ignore new column
  mod <- fnb.multinomial(x, y, laplace = 1)
  predictions <- predict(mod, x, type = "raw")

  x <- cbind(x, x[,1, drop=FALSE])
  colnames(x)[5] <- "womo"

  new_predictions <- predict(mod, x, type = "raw")
  expect_equal(sum(round(abs(predictions-new_predictions), digits = 12)), 0)

  # All new columns is same as all 0
  all_new_columns_predictions <- predict(mod, x[,5,drop=FALSE], type="raw", silent = TRUE)

  x[,1:4] <- 0
  predictions <- predict(mod, x[,1:4], type = "raw")
  expect_equal(sum(round(abs(predictions-all_new_columns_predictions), digits = 12)), 0)
})

test_that("Standard 3 classes", {
  y <- as.factor(c( "Spam", "Spam", "Ham", "Ham", "Lamb", "Lamb"))
  x <- matrix(
    c(1,0,1,1,0,1,
      1,1,0,1,1,0),
    nrow = 6,
    ncol = 2,
    dimnames = list(NULL, c("ho", "mo"))
  )

  actuals <- matrix(
    c(
      0.3200000000000, 0.3600000000000, 0.3200000000000,
      0.2222222222222, 0.3333333333333, 0.4444444444444,
      0.4444444444444, 0.3333333333333, 0.2222222222222,
      0.3200000000000, 0.3600000000000, 0.3200000000000,
      0.2222222222222, 0.3333333333333, 0.4444444444444,
      0.4444444444444, 0.3333333333333, 0.2222222222222
    ),
    nrow = 6,
    ncol = 3,
    byrow = TRUE,
    dimnames = list(NULL, c("Ham", "Lamb", "Spam"))
  )

  mod <- fnb.multinomial(x, y)

  predictions <- predict(mod, x, type="raw")

  expect_equal(sum(round(abs(predictions-actuals), digits = 12)), 0)

  # Test Sparse Matrices
  sparse_mod <- fnb.multinomial(Matrix(x, sparse = TRUE), y)
  sparse_cast_mod <- fnb.multinomial(x, y, sparse = TRUE)

  sparse_predictions <- predict(sparse_mod, x, type = "raw")
  sparse_cast_predictions <- predict(sparse_cast_mod, x, type = "raw")

  expect_equal(sum(round(abs(predictions-sparse_predictions), digits = 12)), 0)
  expect_equal(sum(round(abs(predictions-sparse_cast_predictions), digits = 12)), 0)

})

test_that("Laplace and priors",{
  actuals_laplace <- matrix(
    c(1/4, 9/21,
      3/4, 12/21),
    nrow = 2,
    ncol = 2,
    dimnames = list(NULL, c("Ham", "Spam"))
  )

  y <- as.factor(c( "Spam", "Ham"))
  x <- matrix(
    c(0,1,
      1,1),
    nrow = 2,
    ncol = 2,
    dimnames = list(NULL, c("ho", "mo"))
  )

  mod <- fnb.bernoulli(x, y, laplace = 2, priors = c(1/3, 2/3))

  predictions <- predict(mod, x, type="raw")

  expect_equal(sum(round(abs(predictions-actuals_laplace), digits = 12)), 0)

  # Test Sparse Matrices
  sparse_mod <- fnb.bernoulli(Matrix(x, sparse = TRUE), y, laplace = 2, priors = c(1/3, 2/3))
  sparse_cast_mod <- fnb.bernoulli(x, y, sparse = TRUE, laplace = 2, priors = c(1/3, 2/3))

  sparse_predictions <- predict(sparse_mod, x, type = "raw")
  sparse_cast_predictions <- predict(sparse_cast_mod, x, type = "raw")

  expect_equal(sum(round(abs(predictions-sparse_predictions), digits = 12)), 0)
  expect_equal(sum(round(abs(predictions-sparse_cast_predictions), digits = 12)), 0)
})

test_that("Single column",{
  y <- as.factor(c( "Spam", "Spam", "Ham", "Ham", "Lamb", "Lamb"))
  x <- matrix(
    c(1,0,1,1,0,1,
      1,1,0,1,1,0),
    nrow = 6,
    ncol = 2,
    dimnames = list(NULL, c("ho", "mo"))
  )

  actuals_ho_only <- matrix(
    c(1/3,1/3,1/3,1/3,1/3,1/3,
      1/3,1/3,1/3,1/3,1/3,1/3,
      1/3,1/3,1/3,1/3,1/3,1/3),
    nrow = 6,
    ncol = 3,
    dimnames = list(NULL, c("Ham", "Lamb", "Spam"))
  )

  mod <- fnb.multinomial(x[, 1, drop=FALSE], y)
  predictions <- predict(mod, x[, 1, drop=FALSE], type="raw")

  expect_equal(sum(round(abs(predictions-actuals_ho_only), digits = 12)), 0)

  # Test Sparse Matrices
  sparse_mod <- fnb.multinomial(Matrix(x[, 1, drop=FALSE], sparse = TRUE), y)
  sparse_cast_mod <- fnb.multinomial(x[, 1, drop=FALSE], y, sparse = TRUE)

  sparse_predictions <- predict(sparse_mod, x[, 1, drop=FALSE], type = "raw")
  sparse_cast_predictions <- predict(sparse_cast_mod, x[, 1, drop=FALSE], type = "raw")

  expect_equal(sum(round(abs(predictions-sparse_predictions), digits = 12)), 0)
  expect_equal(sum(round(abs(predictions-sparse_cast_predictions), digits = 12)), 0)

})


test_that("Single row",{
  actuals_single_row_spam <- matrix(
    c(
      0.3720930, 0.4186047, 0.2093023,
      0.4705882, 0.3529412, 0.1764706,
      0.3720930, 0.4186047, 0.2093023,
      0.3076923, 0.4615385, 0.2307692,
      0.4705882, 0.3529412, 0.1764706

    ),
    byrow = TRUE,
    nrow = 5,
    ncol = 3,
    dimnames = list(NULL, c("Ham", "Lamb", "Spam"))
  )

  y <- as.factor(c( "Spam", "Ham", "Ham", "Lamb", "Lamb"))
  x <- matrix(
    c(1,1,1,0,1,
      1,0,1,1,0),
    nrow = 5,
    ncol = 2,
    dimnames = list(NULL, c("ho", "mo"))
  )

  mod <- fnb.multinomial(x, y)

  predictions <- predict(mod, x, type="raw")

  expect_equal(sum(round(abs(predictions-actuals_single_row_spam), digits = 7)), 0)

  # Test Sparse Matrices
  sparse_mod <- fnb.multinomial(Matrix(x, sparse = TRUE), y)
  sparse_cast_mod <- fnb.multinomial(x, y, sparse = TRUE)

  sparse_predictions <- predict(sparse_mod, x, type = "raw")
  sparse_cast_predictions <- predict(sparse_cast_mod, x, type = "raw")

  expect_equal(sum(round(abs(predictions-sparse_predictions), digits = 12)), 0)
  expect_equal(sum(round(abs(predictions-sparse_cast_predictions), digits = 12)), 0)
})



