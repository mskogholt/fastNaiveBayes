context("Test fastNaiveBayes Mixed Training Function")

test_that("Mixed event models estimation gives expected results when mixed", {
  real_probs <- matrix(c(
    0.9535044256,
    0.9999633454,
    0.0009301696,
    0.1435557348,
    0.0009301696,
    1-0.9535044256,
    1-0.9999633454,
    1-0.0009301696,
    1-0.1435557348,
    1-0.0009301696
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

  mixed_mod <- fastNaiveBayes.mixed(x, y, laplace = 0, sparse = FALSE)
  mixed_sparse_mod <- fastNaiveBayes.mixed(Matrix(as.matrix(x), sparse = TRUE), y, laplace = 0)
  mixed_sparse_cast_mod <- fastNaiveBayes.mixed(x, y, laplace = 0, sparse = TRUE)

  preds <- predict(mixed_mod, newdata = x, type = "raw")
  sparse_preds <- predict(mixed_sparse_mod, newdata = x, sparse = TRUE, type = "raw")
  sparse_cast_preds <- predict(mixed_sparse_cast_mod, newdata = Matrix(as.matrix(x), sparse = TRUE), type = "raw")

  expect_equal(sum(round(abs(real_probs - preds), digits = 7)), 0)
  expect_equal(sum(abs(preds - sparse_preds)), 0)
  expect_equal(sum(abs(preds - sparse_cast_preds)), 0)
  expect_equal(sum(y!=predict(mixed_mod, newdata = x, type = "class")),0)

  x <- x[,1:3]
  frame_preds <- predict(mixed_mod, newdata = x, type = 'raw')

  x <- Matrix(as.matrix(x), sparse = TRUE)
  newframe_preds <- predict(mixed_mod, newdata = x, type = 'raw')

  expect_equal(sum(abs(newframe_preds-frame_preds)),0)
  expect_error(mixed_mod(x[1:3,], y))

  y <- as.factor(c("Ham", "Ham", "Ham", "Spam", "Spam", "Spam"))

  x1 <- matrix(c(2, 3, 2, 2, 3, 2,
                 2, 3, 2, 2, 3, 2), nrow = 6, ncol = 2)
  colnames(x1) <- c("wo","lo")

  x2 <- matrix(c(1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1), nrow = 6, ncol = 2)
  colnames(x2) <- c("no","mo")

  x3 <- matrix(c(2.8, 2.7, 2.9, 2.9, 2.7, 2.8,
                 2.8, 2.7, 2.9, 2.9, 2.7, 2.8), nrow = 6, ncol = 2)
  colnames(x3) <- c("go","ho")

  x <- cbind(x1, x2, x3)

  mod <- fastNaiveBayes.mixed(x, y, laplace = 0.00001, sparse = TRUE)

  new_preds <- predict(mod, newdata=x[,c(1,3,5)], type = "raw")
  other_preds <- predict(mod, newdata = x[,c(1,3,5)], type = "raw", sparse = TRUE)

  expect_equal(sum(abs(new_preds-other_preds)), 0)
  expect_warning(predict(mod, newdata=x, type = "class"))
})
