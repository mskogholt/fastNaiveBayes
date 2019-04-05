context("Test fastNaiveBayes Gaussian Training Function")

test_that("Gaussian estimation gives expected results", {
  y <- as.factor(c("Ham", "Ham", "Spam", "Spam", "Spam"))
  x <- matrix(c(2, 3, 2, 1, 2, 5, 3, 4, 2, 4, 0, 1, 3, 1, 0, 3, 4, 4, 3, 5),
    nrow = 5, ncol = 4
  )
  col_names <- c("wo", "mo", "bo", "so")
  colnames(x) <- col_names
  x <- as.data.frame(x)

  # Standard Multinomial model test with laplace = 0
  mod <- fastNaiveBayes.gaussian(x, y, std_threshold = 0, sparse = FALSE)
  sparse_cast_mod <- fastNaiveBayes.gaussian(x, y, std_threshold = 0, sparse = TRUE)
  sparse_mod <- fastNaiveBayes.gaussian(Matrix(as.matrix(x), sparse = TRUE), y, std_threshold = 0)

  preds <- predict(mod, newdata = x, type = "raw")
  sparse_preds <- predict(sparse_mod, newdata = x, type = "raw")
  sparse_cast_preds <- predict(sparse_cast_mod, newdata = x, type = "raw")

  expect_equal(sum(abs(preds - sparse_preds)), 0)
  expect_equal(sum(abs(preds - sparse_cast_preds)), 0)

  real_preds <- matrix(c(
    0.8014134, 0.8847304, 0.004007538, 0.1698076, 0.226208,
    0.1985866, 0.1152696, 0.9959925, 0.8301924, 0.773792
  ),
  nrow = 5, ncol = 2
  )
  expect_equal(sum(round(abs(preds - real_preds), digits = 7)), 0)
  expect_equal(sum(y != predict(mod, newdata = x, type = "class")), 0)

  x <- x[, 1:3]
  frame_preds <- predict(mod, newdata = x, type = "raw")

  x <- Matrix(as.matrix(x), sparse = TRUE)
  newframe_preds <- predict(mod, newdata = x, type = "raw")

  expect_equal(sum(abs(newframe_preds - frame_preds)), 0)

  x <- as.matrix(x[, 1])
  colnames(x) <- col_names[1]

  mod <- fastNaiveBayes.gaussian(x, y, std_threshold = 0, sparse = FALSE)
  preds <- predict(mod, newdata = x, type = "raw")

  real_preds <- matrix(c(
    0.3336926, 0.8591767, 0.3336926, 0.1005136, 0.3336926,
    0.6663074, 0.1408233, 0.6663074, 0.8994864, 0.6663074
  ),
  nrow = 5, ncol = 2
  )

  expect_equal(sum(round(abs(preds - real_preds), digits = 7)), 0)
  expect_error(fastNaiveBayes.gaussian(x[1:3, ], y))
})
