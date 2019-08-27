context("Test fastNaiveBayes Multinomial Training Function")

test_that("Multinomial estimation gives expected results", {
  # Test 2
  y <- as.factor(c("Ham", "Ham", "Spam", "Spam", "Spam"))
  x <- matrix(c(2, 3, 0, 1, 0, 5, 3, 0, 2, 0, 0, 1, 3, 1, 0, 0, 0, 4, 3, 5),
    nrow = 5, ncol = 4)
  colnames(x) <- c("wo", "mo", "bo", "so")
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
  mod <- fnb.multinomial(x, y, laplace = 1, sparse = FALSE)
  sparse_cast_mod <- fnb.multinomial(x, y, laplace = 1, sparse = TRUE)
  sparse_mod <- fnb.multinomial(Matrix(as.matrix(x), sparse = TRUE), y, laplace = 1, sparse = TRUE)

  preds <- predict(mod, newdata = x, type = "raw")
  sparse_preds <- predict(sparse_mod, newdata = x, type = "raw", sparse = TRUE)
  sparse_cast_preds <- predict(sparse_cast_mod, newdata = Matrix(as.matrix(x), sparse = TRUE), type = "raw")

  expect_equal(sum(round(abs(real_probs - preds), digits = 7)), 0)
  expect_equal(sum(abs(preds - sparse_preds)), 0)
  expect_equal(sum(abs(preds - sparse_cast_preds)), 0)
  expect_equal(sum(y != predict(mod, newdata = x, type = "class")), 0)

  # Test 3
  x <- as.matrix(x[, 1])
  colnames(x) <- "wo"
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
  mod <- fnb.multinomial(x, y, laplace = 1, sparse = FALSE)
  probs <- predict(mod, newdata = x, type = "raw")

  expect_equal(sum(round(abs(real_probs - probs), digits = 7)), 0)


  ho <- c(0,0,0,0,0)
  mod <- fnb.multinomial(cbind(x, ho), y, laplace = 1, sparse = FALSE)
  probs <- predict(mod, newdata = x, type = "raw", silent = TRUE)
  real_probs <- matrix(c(
    0.5242718,
    0.5862485,
    0.4000000,
    0.4615385,
    0.4000000,
    0.4757282,
    0.4137515,
    0.6000000,
    0.5384615,
    0.6000000
  ), nrow = 5, ncol = 2)

  expect_equal(sum(round(abs(real_probs - probs), digits = 7)), 0)



})
