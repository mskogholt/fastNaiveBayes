context("Test fastNaiveBayes Multinomial Training Function")

test_that("Multinomial estimation gives expected results", {
  y <- as.factor(c("Ham", "Spam"))
  x <- matrix(c(1, 0, 0, 1),
    nrow = 2, ncol = 2
  )
  col_names <- c("wo", "so")
  colnames(x) <- col_names
  x <- as.data.frame(x)

  real_probs <- matrix(c(1, 0, 0, 1),
    nrow = 2, ncol = 2
  )

  # Bernoulli model test with laplace = 1
  mod <- fastNaiveBayes.multinomial(x, y, laplace = 0, sparse = FALSE)
  sparse_mod <- fastNaiveBayes.multinomial(x, y, laplace = 0, sparse = TRUE)
  sparse_cast_mod <- fastNaiveBayes.multinomial(Matrix(as.matrix(x), sparse = TRUE),
    y,
    laplace = 0
  )

  mod_preds <- predict(mod, newdata = x, type = "raw")
  sparse_preds <- predict(sparse_mod, newdata = Matrix(as.matrix(x), sparse = TRUE), type = "raw")
  sparse_cast_preds <- predict(sparse_cast_mod, newdata = x, type = "raw", sparse = TRUE)

  expect_equal(sum(abs(round(mod_preds - real_probs, digits = 8))), 0)
  expect_equal(sum(abs(mod_preds - sparse_preds)), 0)
  expect_equal(sum(abs(mod_preds - sparse_cast_preds)), 0)

  # Test Predictions
  y <- as.factor(c("Ham", "Ham", "Spam", "Spam", "Spam"))
  x <- matrix(c(2, 3, 0, 1, 0, 5, 3, 0, 2, 0, 0, 1, 3, 1, 0, 0, 0, 4, 3, 5),
    nrow = 5, ncol = 4
  )
  col_names <- c("wo", "mo", "bo", "so")
  colnames(x) <- col_names
  x <- as.data.frame(x)

  real_probs <- matrix(c(
    0.9998767,
    0.9990759,
    0.000008308048,
    0.01789969,
    0.000006115979,
    0.0001233109,
    0.0009240914,
    0.9999917,
    0.9821003,
    0.9999939
  ), nrow = 5, ncol = 2)

  # Standard Multinomial model test with laplace = 0
  mod <- fastNaiveBayes.multinomial(x, y, laplace = 1, sparse = FALSE)
  sparse_cast_mod <- fastNaiveBayes.multinomial(x, y, laplace = 1, sparse = TRUE)
  sparse_mod <- fastNaiveBayes.multinomial(Matrix(as.matrix(x), sparse = TRUE), y, laplace = 1, sparse = TRUE)

  preds <- predict(mod, newdata = x, type = "raw")
  sparse_preds <- predict(sparse_mod, newdata = x, type = "raw", sparse = TRUE)
  sparse_cast_preds <- predict(sparse_cast_mod, newdata = Matrix(as.matrix(x), sparse = TRUE), type = "raw")

  expect_equal(sum(round(abs(real_probs - preds), digits = 7)), 0)
  expect_equal(sum(abs(preds - sparse_preds)), 0)
  expect_equal(sum(abs(preds - sparse_cast_preds)), 0)
  expect_equal(sum(y != predict(mod, newdata = x, type = "class")), 0)

  x <- x[, 1:3]
  frame_preds <- predict(mod, newdata = x, type = "raw")

  x <- Matrix(as.matrix(x), sparse = TRUE)
  newframe_preds <- predict(mod, newdata = x, type = "raw")

  expect_equal(sum(abs(newframe_preds - frame_preds)), 0)
  expect_error(fastNaiveBayes.multinomial(x[1:3, ], y))

  x <- as.matrix(x[, 1])
  colnames(x) <- col_names[1]
  real_probs <- matrix(c(
    0.4,
    0.4,
    0.4,
    0.4,
    0.4,
    0.6,
    0.6,
    0.6,
    0.6,
    0.6
  ), nrow = 5, ncol = 2)
  # Standard Multinomial model test with laplace = 0
  mod <- fastNaiveBayes.multinomial(x, y, laplace = 1, sparse = FALSE)
  probs <- predict(mod, newdata = x, type = "raw")

  expect_equal(sum(round(abs(real_probs - probs), digits = 7)), 0)
})
