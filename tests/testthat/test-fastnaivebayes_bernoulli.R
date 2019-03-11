context("Test fastNaiveBayes Bernoulli Training Function")

test_that("Bernoulli estimation gives expected results", {
  y <- as.factor(c("Ham", "Ham", "Spam", "Spam", "Spam"))
  x <- matrix(c(1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1),
    nrow = 5, ncol = 4
  )
  col_names <- c("wo", "mo", "bo", "so")
  colnames(x) <- col_names

  # Standard bernoulli model test with laplace = 0
  mod <- fastNaiveBayes.bernoulli(x, y, laplace = 0, sparse = FALSE)

  priors <- mod$priors
  expect_equal(priors[[1]], 0.4)
  expect_equal(priors[[2]], 0.6)

  expect_equal(mod$names, col_names)

  prob_table <- mod$probability_table
  expect_equal(sum(abs(prob_table$present - (1 - prob_table$non_present))), 0)

  real_present <- matrix(c(1 / 2, 0, 1, 1 / 3, 1 / 2, 2 / 3, 0, 1), nrow = 2, ncol = 4)
  expect_equal(sum(abs(prob_table$present - real_present)), 0)

  # Bernoulli model test with laplace = 1
  mod <- fastNaiveBayes.bernoulli(x, y, laplace = 1, sparse = FALSE)

  priors <- mod$priors
  expect_equal(priors[[1]], 0.4)
  expect_equal(priors[[2]], 0.6)

  expect_equal(mod$names, col_names)

  prob_table <- mod$probability_table
  expect_equal(sum(abs(prob_table$present - (1 - prob_table$non_present))), 0)

  real_present <- matrix(c(
    2 / 4, 1 / 5,
    3 / 4, 2 / 5,
    2 / 4, 3 / 5,
    1 / 4, 4 / 5
  ),
  nrow = 2, ncol = 4
  )
  expect_equal(sum(abs(prob_table$present - real_present)), 0)

  real_non_present <- matrix(c(
    2 / 4, 4 / 5,
    1 / 4, 3 / 5,

    2 / 4, 2 / 5,
    3 / 4, 1 / 5
  ),
  nrow = 2, ncol = 4
  )
  expect_equal(sum(abs(prob_table$non_present - real_non_present)), 0)

  # Test sparse casting, should produce same results
  sparse_mod <- fastNaiveBayes.bernoulli(x, y, laplace = 1, sparse = TRUE)
  expect_equal(mod$names, sparse_mod$names)
  expect_equal(mod$priors, sparse_mod$priors)

  expect_equal(
    sum(abs(mod$probability_table$present - sparse_mod$probability_table$present)), 0
  )
  expect_equal(
    sum(abs(mod$probability_table$non_present - sparse_mod$probability_table$non_present)), 0
  )

  mod_preds <- predict(mod, newdata = x, type = "raw")
  sparse_preds <- predict(sparse_mod, newdata = x, type = "raw")

  expect_equal(sum(abs(mod_preds - sparse_preds)), 0)

  # Test Prediction
  y <- as.factor(c("Ham", "Ham", "Spam", "Spam", "Spam"))
  x <- matrix(c(1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1),
    nrow = 5, ncol = 4
  )
  col_names <- c("wo", "mo", "bo", "so")
  colnames(x) <- col_names

  # Standard bernoulli model test with laplace = 0
  mod <- fastNaiveBayes.bernoulli(x, y, laplace = 1, sparse = FALSE)
  probs <- predict(mod, newdata = x, type = "raw")

  real_probs <- matrix(c(
    0.93609586, 0.70942111, 0.04325559, 0.16905599, 0.06350981, 0.06390414,
    0.29057889, 0.95674441, 0.83094401, 0.93649019
  ),
  nrow = 5, ncol = 2
  )

  expect_equal(sum(abs(round(probs - real_probs, digits = 8))), 0)


  x <- as.matrix(x[, 3])
  colnames(x) <- col_names[3]

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
  mod <- fastNaiveBayes.bernoulli(x, y, laplace = 1, sparse = FALSE)
  probs <- predict(mod, newdata = x, type = "raw")

  expect_equal(sum(round(abs(real_probs - probs), digits = 7)), 0)
})
