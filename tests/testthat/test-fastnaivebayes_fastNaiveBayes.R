context("Test fastNaiveBayes")

test_that("fastNaiveBayes wraps", {
  # Test 1
  real_probs <- matrix(c(
    0.9535044256,
    0.9999633454,
    0.0009301696,
    0.1435557348,
    0.0009301696,
    1 - 0.9535044256,
    1 - 0.9999633454,
    1 - 0.0009301696,
    1 - 0.1435557348,
    1 - 0.0009301696
  ), nrow = 5, ncol = 2)

  y <- as.factor(c("Ham", "Ham", "Spam", "Spam", "Spam"))

  x1 <- matrix(c(2, 3, 2, 4, 3), nrow = 5, ncol = 1)
  colnames(x1) <- c("wo")

  x2 <- matrix(c(1, 0, 1, 0, 1), nrow = 5, ncol = 1)
  colnames(x2) <- c("no")

  x3 <- matrix(c(2.8, 2.7, 3.0, 2.9, 3.0), nrow = 5, ncol = 1)
  colnames(x3) <- c("go")

  x <- cbind(x1, x2, x3)
  col_names <- c("wo", "no", "go")
  colnames(x) <- col_names

  mixed_mod <- fnb.train(x, y, laplace = 0, sparse = FALSE)
  fastNaiveBayesMod <- fastNaiveBayes(x, y, laplace = 0, sparse = FALSE)

  expect_equal(mixed_mod, fastNaiveBayesMod)
})
