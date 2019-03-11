context("Test fastNaiveBayes Multinomial Training Function")

test_that("Multinomial estimation gives expected results", {
  y <- as.factor(c("Ham", "Ham", "Spam", "Spam", "Spam"))
  x <- matrix(c(2, 3, 0, 1, 0, 5, 3, 0, 2, 0, 0, 1, 3, 1, 0, 0, 0, 4, 3, 5),
    nrow = 5, ncol = 4
  )
  col_names <- c("wo", "mo", "bo", "so")
  colnames(x) <- col_names

  # Standard Multinomial model test with laplace = 0
  mod <- fastNaiveBayes.multinomial(x, y, laplace = 0, sparse = FALSE)

  priors <- mod$priors
  expect_equal(priors[[1]], 0.4)
  expect_equal(priors[[2]], 0.6)

  expect_equal(mod$names, col_names)

  prob_table <- mod$probability_table

  real_present <- matrix(c(
    5 / 14, 1 / 19,
    8 / 14, 2 / 19,
    1 / 14, 4 / 19,
    0, 12 / 19
  ),
  nrow = 2, ncol = 4
  )
  expect_equal(sum(abs(prob_table$present - real_present)), 0)

  # Multinomial model test with laplace = 1
  mod <- fastNaiveBayes.multinomial(x, y, laplace = 1, sparse = FALSE)

  priors <- mod$priors
  expect_equal(priors[[1]], 0.4)
  expect_equal(priors[[2]], 0.6)

  expect_equal(mod$names, col_names)

  prob_table <- mod$probability_table

  real_present <- matrix(c(
    6 / 18, 2 / 23,
    9 / 18, 3 / 23,
    2 / 18, 5 / 23,
    1 / 18, 13 / 23
  ),
  nrow = 2, ncol = 4
  )
  expect_equal(sum(abs(prob_table$present - real_present)), 0)

  # Test sparse casting, should produce same results
  sparse_mod <- fastNaiveBayes.multinomial(x, y, laplace = 1, sparse = TRUE)
  expect_equal(mod$names, sparse_mod$names)
  expect_equal(mod$priors, sparse_mod$priors)

  # Does not work with lists
  expect_equal(
    sum(abs(mod$probability_table$present - sparse_mod$probability_table$present)), 0
  )

  expect_equal(sum(abs(predict(mod, newdata = x, type = "raw") -
    predict(sparse_mod, newdata = x, type = "raw"))), 0)

  # Test Predictions
  y <- as.factor(c("Ham", "Ham", "Spam", "Spam", "Spam"))
  x <- matrix(c(2, 3, 0, 1, 0, 5, 3, 0, 2, 0, 0, 1, 3, 1, 0, 0, 0, 4, 3, 5),
    nrow = 5, ncol = 4
  )
  col_names <- c("wo", "mo", "bo", "so")
  colnames(x) <- col_names

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
  probs <- predict(mod, newdata = x, type = "raw")

  expect_equal(sum(round(abs(real_probs - probs), digits = 7)), 0)

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
