context("Test fastNaiveBayes Train Function")

test_that("Mixed event models estimation gives expected results when mixed", {
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
  x <- as.data.frame(x)

  mixed_mod <- fnb.train(x, y, laplace = 0, sparse = FALSE)
  mixed_sparse_mod <- fnb.train(Matrix(as.matrix(x), sparse = TRUE), y, laplace = 0)
  mixed_sparse_cast_mod <- fnb.train(x, y, laplace = 0, sparse = TRUE)

  preds <- predict(mixed_mod, newdata = x, type = "raw")
  sparse_preds <- predict(mixed_sparse_mod, newdata = x, sparse = TRUE, type = "raw")
  sparse_cast_preds <- predict(mixed_sparse_cast_mod, newdata = Matrix(as.matrix(x), sparse = TRUE), type = "raw")

  expect_equal(sum(round(abs(real_probs - preds), digits = 7)), 0)
  expect_equal(sum(abs(preds - sparse_preds)), 0)
  expect_equal(sum(abs(preds - sparse_cast_preds)), 0)
  expect_equal(sum(y != predict(mixed_mod, newdata = x, type = "class")), 0)
})
