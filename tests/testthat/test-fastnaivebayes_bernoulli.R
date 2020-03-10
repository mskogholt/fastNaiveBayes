context("Test Bernoulli")

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
  mod <- fnb.bernoulli(x, y, laplace = 1)
  df_mod <- fnb.bernoulli(df, y, laplace = 1)

  predictions <- predict(mod, x, type = "raw")
  df_predictions <- predict(df_mod, df, type = "raw", sparse = TRUE)

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
  mod <- fnb.bernoulli(x, y, laplace = 1)
  predictions <- predict(mod, x, type = "raw")

  x <- cbind(x, x[,1, drop=FALSE])
  colnames(x)[5] <- "womo"

  new_predictions <- predict(mod, x, type = "raw", silent=TRUE)
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
    c(2/5, 0, 2/3, 2/5, 0, 2/3,
      1/5, 1/3, 1/3, 1/5, 1/3, 1/3,
      2/5, 2/3, 0, 2/5, 2/3, 0),
    nrow = 6,
    ncol = 3,
    dimnames = list(NULL, c("Ham", "Lamb", "Spam"))
  )

  mod <- fnb.bernoulli(x, y)
  expect_error(fnb.bernoulli(x, y, laplace = -1))

  predictions <- predict(mod, x, type="raw")

  expect_equal(sum(round(abs(predictions-actuals), digits = 12)), 0)

  # Test Sparse Matrices
  sparse_mod <- fnb.bernoulli(Matrix(x, sparse = TRUE), y)
  sparse_cast_mod <- fnb.bernoulli(x, y, sparse = TRUE)

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
    c(1/2, 0, 1/2, 1/2, 0, 1/2,
      1/4, 1/2, 1/4, 1/4, 1/2, 1/4,
      1/4, 1/2, 1/4, 1/4, 1/2, 1/4),
    nrow = 6,
    ncol = 3,
    dimnames = list(NULL, c("Ham", "Lamb", "Spam"))
  )

  mod <- fnb.bernoulli(x[, 1, drop=FALSE], y)
  predictions <- predict(mod, x[, 1, drop=FALSE], type="raw")

  expect_equal(sum(round(abs(predictions-actuals_ho_only), digits = 12)), 0)

  # Test Sparse Matrices
  sparse_mod <- fnb.bernoulli(Matrix(x[, 1, drop=FALSE], sparse = TRUE), y)
  sparse_cast_mod <- fnb.bernoulli(x[, 1, drop=FALSE], y, sparse = TRUE)

  sparse_predictions <- predict(sparse_mod, x[, 1, drop=FALSE], type = "raw")
  sparse_cast_predictions <- predict(sparse_cast_mod, x[, 1, drop=FALSE], type = "raw")

  expect_equal(sum(round(abs(predictions-sparse_predictions), digits = 12)), 0)
  expect_equal(sum(round(abs(predictions-sparse_cast_predictions), digits = 12)), 0)

})

test_that("Single row",{
  actuals_single_row_spam <- matrix(
    c(2/5, 2/3, 2/5, 0, 2/3,
      1/5, 1/3, 1/5, 1, 1/3,
      2/5, 0, 2/5, 0, 0),
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

  mod <- fnb.bernoulli(x, y)

  predictions <- predict(mod, x, type="raw")

  expect_equal(sum(round(abs(predictions-actuals_single_row_spam), digits = 12)), 0)

  # Test Sparse Matrices
  sparse_mod <- fnb.bernoulli(Matrix(x, sparse = TRUE), y)
  sparse_cast_mod <- fnb.bernoulli(x, y, sparse = TRUE)

  sparse_predictions <- predict(sparse_mod, x, type = "raw")
  sparse_cast_predictions <- predict(sparse_cast_mod, x, type = "raw")

  expect_equal(sum(round(abs(predictions-sparse_predictions), digits = 12)), 0)
  expect_equal(sum(round(abs(predictions-sparse_cast_predictions), digits = 12)), 0)
})

test_that("Class prediction matches probability", {
  ## Example from 13.1 Manning, Christopher D et al. 2008. Introduction to
  ## Information Retrieval. Cambridge University Press.
  x <- structure(c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                   1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1), .Dim = 5:6,
                 .Dimnames = list(
                   docs = c("d1", "d2", "d3", "d4", "d5"),
                   features = c("chinese",
                                "beijing", "shanghai", "macao", "tokyo", "japan")))
  y <- factor(c("Y", "Y", "Y", "N", NA), ordered = TRUE)

  tmod_fnb <- fnb.bernoulli(x[1:4, ], y[1:4], laplace = 1)
  pred_class <- predict(tmod_fnb, newdata = x, type = "class")
  pred_probs <- predict(tmod_fnb, newdata = x, type = "raw")
  expect_identical(
    as.character(pred_class),
    colnames(pred_probs)[max.col(pred_probs)]
  )
})
