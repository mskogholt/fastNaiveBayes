context("Test fastNaiveBayes Bernoulli Training Function")

test_that("Bernoulli estimation gives expected results", {
  # Test 1
  y <- as.factor(c("Ham", "Spam"))
  x <- matrix(c(1, 0), nrow = 2, ncol = 1)
  colnames(x) <- c("wo")
  x <- as.data.frame(x)

  real_probs <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)

  mod <- fnb.bernoulli(x, y, laplace = 0, sparse = FALSE)
  sparse_mod <- fnb.bernoulli(x, y, laplace = 0, sparse = TRUE)
  sparse_cast_mod <- fnb.bernoulli(Matrix(as.matrix(x), sparse = TRUE), y, laplace = 0)

  mod_preds <- predict(mod, newdata = x, type = "raw")
  sparse_preds <- predict(sparse_mod, newdata = Matrix(as.matrix(x), sparse = TRUE), type = "raw")
  sparse_cast_preds <- predict(sparse_cast_mod, newdata = x, type = "raw", sparse = TRUE)

  expect_equal(sum(abs(round(mod_preds - real_probs, digits = 8))), 0)
  expect_equal(sum(abs(mod_preds - sparse_preds)), 0)
  expect_equal(sum(abs(mod_preds - sparse_cast_preds)), 0)

  # Test 2
  y <- as.factor(c("Ham", "Ham", "Spam", "Spam", "Spam"))
  x <- matrix(c(1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1),
    nrow = 5, ncol = 4
  )
  colnames(x) <- c("wo", "mo", "bo", "so")
  x <- as.data.frame(x)

  real_probs <- matrix(c(
    0.93609586, 0.70942111, 0.04325559, 0.16905599, 0.06350981, 0.06390414,
    0.29057889, 0.95674441, 0.83094401, 0.93649019
  ), nrow = 5, ncol = 2 )

  # Bernoulli model test with laplace = 1
  mod <- fnb.bernoulli(x, y, laplace = 1, sparse = FALSE)
  sparse_mod <- fnb.bernoulli(x, y, laplace = 1, sparse = TRUE)
  sparse_cast_mod <- fnb.bernoulli(Matrix(as.matrix(x), sparse = TRUE), y, laplace = 1)

  mod_preds <- predict(mod, newdata = x, type = "raw")
  sparse_preds <- predict(sparse_mod, newdata = Matrix(as.matrix(x), sparse = TRUE), type = "raw")
  sparse_cast_preds <- predict(sparse_cast_mod, newdata = x, type = "raw", sparse = TRUE)

  expect_equal(sum(abs(round(mod_preds - real_probs, digits = 8))), 0)
  expect_equal(sum(abs(mod_preds - sparse_preds)), 0)
  expect_equal(sum(abs(mod_preds - sparse_cast_preds)), 0)
  expect_equal(sum(y != predict(mod, newdata = x, type = "class")), 0)

  x <- as.matrix(x[, 3])
  colnames(x) <- "wo"

  real_probs <- matrix(c(
    0.4545455,
    0.3571429,
    0.3571429,
    0.3571429,
    0.4545455,
    0.5454545,
    0.6428571,
    0.6428571,
    0.6428571,
    0.5454545
  ), nrow = 5, ncol = 2)
  # Standard Multinomial model test with laplace = 0
  mod <- fnb.bernoulli(x, y, laplace = 1, sparse = FALSE)
  probs <- predict(mod, newdata = x, type = "raw")

  expect_equal(sum(round(abs(real_probs - probs), digits = 7)), 0)
})
