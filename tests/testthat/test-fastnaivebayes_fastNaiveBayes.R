context("Test fastNaiveBayes Training Function")

test_that("fastNaiveBayes estimation gives expected results", {
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
  mixed_sparse_mod <- fnb.train(Matrix(as.matrix(x), sparse = TRUE), y, laplace = 0)
  mixed_sparse_cast_mod <- fnb.train(x, y, laplace = 0, sparse = TRUE)

  preds <- predict(mixed_mod, newdata = x, type = "raw")
  sparse_preds <- predict(mixed_sparse_mod, newdata = x, sparse = TRUE, type = "raw")
  sparse_cast_preds <- predict(mixed_sparse_cast_mod, newdata = Matrix(as.matrix(x), sparse = TRUE), type = "raw")

  fastNaiveBayesMod <- fastNaiveBayes(x, y, laplace = 0, sparse = FALSE)
  fastNaiveBayesModDF <- fastNaiveBayes(as.data.frame(x), y, laplace = 0, sparse = FALSE)
  fastNaiveBayes_sparse_mod <- fastNaiveBayes(Matrix(as.matrix(x), sparse = TRUE), y, laplace = 0)
  fastNaiveBayes_sparse_cast_mod <- fastNaiveBayes(x, as.character(y), laplace = 0, sparse = TRUE)


  fastNaiveBayesPreds <- predict(fastNaiveBayesMod, newdata = x, type = "raw")
  fastNaiveBayesPredsDF <- predict(fastNaiveBayesModDF, newdata = x, type = "raw")
  fastNaiveBayes_sparse_preds <- predict(fastNaiveBayes_sparse_mod, newdata = x, sparse = TRUE, type = "raw")
  fastNaiveBayes_sparse_cast_preds <- predict(fastNaiveBayes_sparse_cast_mod, newdata = Matrix(as.matrix(x), sparse = TRUE), type = "raw")

  expect_equal(sum(round(abs(fastNaiveBayesPreds - preds), digits = 7)), 0)
  expect_equal(sum(round(abs(fastNaiveBayesPredsDF - preds), digits = 7)), 0)
  expect_equal(sum(round(abs(fastNaiveBayes_sparse_preds - preds), digits = 7)), 0)
  expect_equal(sum(round(abs(fastNaiveBayes_sparse_cast_preds - preds), digits = 7)), 0)


  x <- cbind(x1, x1, x2, x2, x3, x3)
  col_names <- c("wo", "so", "no", "go", "mo", "po")
  colnames(x) <- col_names

  mixed_mod <- fnb.train(x, y, laplace = 0, sparse = FALSE)
  fastNaiveBayesMod <- fastNaiveBayes(x, y, laplace = 0, sparse = FALSE)

  # expect_warning(predict(fastNaiveBayesMod, newdata = as.data.frame(x[,c(1, 3, 5)]), type = "class"))

  expect_error(fastNaiveBayes(x[1:2,], y, laplace = 0, sparse = FALSE))
  expect_error(fastNaiveBayes(x[1:2,], y[1:2], laplace = 0, sparse = FALSE))
  expect_error(fastNaiveBayes(x, as.character(y[1:2]), laplace = 0, sparse = FALSE))
  expect_error(fastNaiveBayes(as.matrix(data.frame()), laplace = 0, sparse = FALSE))
  x[1,2] <- NA
  expect_warning(fastNaiveBayes(x, y, laplace = 0, sparse = FALSE))
  y[3] <- NA
  expect_warning(fastNaiveBayes(x, y, laplace = 0, sparse = FALSE))
})
