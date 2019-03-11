context("Test fastNaiveBayes Mixed Training Function")

test_that("Mixed event models estimation gives expected results when mixed", {
  y <- as.factor(c("Ham", "Ham", "Spam", "Spam", "Spam"))

  x1 <- matrix(c(2, 3, 0, 1, 0, 5, 3, 0, 2, 0, 0, 1, 3, 1, 0, 0, 0, 4, 3, 5),
    nrow = 5, ncol = 4
  )
  colnames(x1) <- c("wo", "mo", "bo", "so")

  x2 <- matrix(c(1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1),
    nrow = 5, ncol = 4
  )
  colnames(x2) <- c("no", "ko", "po", "lo")

  x <- cbind(x1, x2)
  col_names <- c("wo", "mo", "bo", "so", "no", "ko", "po", "lo")
  colnames(x) <- col_names

  # Standard Multinomial model test with laplace = 0
  mixed_mod <- fastNaiveBayes.mixed(x, y,
    laplace = 0, sparse = FALSE,
    distribution = list(
      multinomial = colnames(x1),
      bernoulli = colnames(x2)
    )
  )
  mult_mod <- mixed_mod[[1]][[1]]
  bern_mod <- mixed_mod[[1]][[2]]

  expect_equal(mixed_mod$names, col_names)

  expect_equal(mixed_mod$priors[[1]], 0.4)
  expect_equal(mixed_mod$priors[[2]], 0.6)

  expect_equal(mixed_mod$distribution, list(
    multinomial = colnames(x1),
    bernoulli = colnames(x2)
  ))

  mod <- fastNaiveBayes.bernoulli(x2, y, laplace = 0, sparse = FALSE)

  expect_equal(mod$priors, bern_mod$priors)
  expect_equal(mod$probability_table$present, bern_mod$probability_table$present)
  expect_equal(mod$probability_table$non_present, bern_mod$probability_table$non_present)

  mod <- fastNaiveBayes.multinomial(x1, y, laplace = 0, sparse = FALSE)

  expect_equal(mod$names, mult_mod$names)
  expect_equal(mod$priors, mult_mod$priors)
  expect_equal(mod$probability_table$present, mult_mod$probability_table$present)

  # Prediction Tests
  y <- as.factor(c("Ham", "Ham", "Spam", "Spam", "Spam"))

  x1 <- matrix(c(2, 3, 0, 1, 0, 5, 3, 0, 2, 0, 0, 1, 3, 1, 0, 1, 0, 4, 3, 5),
    nrow = 5, ncol = 4
  )
  colnames(x1) <- c("wo", "mo", "bo", "so")

  x2 <- matrix(c(1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1),
    nrow = 5, ncol = 4
  )
  colnames(x2) <- c("no", "ko", "po", "lo")

  x <- cbind(x1, x2)
  col_names <- c("wo", "mo", "bo", "so", "no", "ko", "po", "lo")
  colnames(x) <- col_names


  mixed_mod <- fastNaiveBayes.mixed(x, y,
    laplace = 1, sparse = FALSE,
    distribution = list(
      multinomial = colnames(x1),
      bernoulli = colnames(x2)
    )
  )

  sparse_input_mod <- fastNaiveBayes.mixed(Matrix(x, sparse = TRUE), y,
    laplace = 1,
    distribution = list(
      multinomial = colnames(x1),
      bernoulli = colnames(x2)
    )
  )

  sparse_mod <- fastNaiveBayes.mixed(x, y,
    laplace = 1, sparse = TRUE,
    distribution = list(
      multinomial = colnames(x1),
      bernoulli = colnames(x2)
    )
  )

  expect_equal(sum(abs(predict(mixed_mod, newdata = x, type = "raw") -
    predict(sparse_mod, newdata = x, type = "raw"))), 0)

  expect_equal(sum(abs(predict(mixed_mod, newdata = x, type = "raw") -
    predict(sparse_input_mod, newdata = x, type = "raw"))), 0)

  expect_equal(sum(abs(predict(mixed_mod, newdata = x, type = "raw") -
    predict(sparse_input_mod, newdata = x, type = "raw", sparse = TRUE))), 0)

  expect_equal(sum(abs(predict(mixed_mod, newdata = x, type = "raw") -
    predict(sparse_input_mod, newdata = Matrix(x, sparse = TRUE), type = "raw"))), 0)

  # Mixed mod prediction test
  x1 <- matrix(c(2, 3, 0, 1, 0, 5, 3, 0, 2, 0, 0, 1, 3, 1, 0, 1, 0, 4, 3, 5),
    nrow = 5, ncol = 4
  )
  colnames(x1) <- c("wo", "mo", "bo", "so")

  x2 <- matrix(c(1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1),
    nrow = 5, ncol = 4
  )
  colnames(x2) <- c("no", "ko", "po", "lo")

  x3 <- matrix(c(2.5, 3.2, 4.5, 4.1, 4.8),
    nrow = 5, ncol = 1
  )
  colnames(x3) <- c("ga")

  x <- cbind(x1, x2, x3)
  col_names <- c("wo", "mo", "bo", "so", "no", "ko", "po", "lo", "ga")
  colnames(x) <- col_names

  mixed_mod <- fastNaiveBayes.mixed(x, y,
    laplace = 1,
    std_threshold = 0.01, sparse = FALSE,
    distribution = list(
      multinomial = colnames(x1),
      bernoulli = colnames(x2),
      gaussian = c("ga")
    )
  )

  preds <- predict(mixed_mod, newdata = x, type = "raw")

  bern <- fastNaiveBayes.bernoulli(x2, y, laplace = 1, sparse = FALSE)
  mult <- fastNaiveBayes.multinomial(x1, y, laplace = 1, sparse = FALSE)
  gauss <- fastNaiveBayes.gaussian(x3, y, std_threshold = 0.01, sparse = FALSE)

  temp_probs <- exp(predict(bern, newdata = x2, type = "rawprob") +
    predict(mult, newdata = x1, type = "rawprob") +
    predict(gauss, newdata = x3, type = "rawprob"))
  temp_probs[, 1] <- temp_probs[, 1] * 0.4
  temp_probs[, 2] <- temp_probs[, 2] * 0.6
  expect_equal(sum(abs(temp_probs / rowSums(temp_probs) - preds)), 0)
})

test_that("Mixed event models estimation gives expected results with Gaussian model", {
  y <- as.factor(c("Ham", "Ham", "Spam", "Spam", "Spam"))
  x <- matrix(c(2, 3, 0, 1, 0, 5, 3, 0, 2, 0, 0, 1, 3, 1, 0, 0, 0, 4, 3, 5),
    nrow = 5, ncol = 4
  )
  col_names <- c("wo", "mo", "bo", "so")
  colnames(x) <- col_names

  # Standard Multinomial model test with laplace = 0
  mod <- fastNaiveBayes.gaussian(x, y, std_threshold = 0, sparse = FALSE)
  mixed_mod <- fastNaiveBayes.mixed(x, y, std_threshold = 0, sparse = FALSE, distribution = list(gaussian = colnames(x)))
  gaussian_mod <- mixed_mod[[1]][[1]]

  expect_equal(mod$names, gaussian_mod$names)
  expect_equal(mod$priors, gaussian_mod$priors)

  expect_equal(mod$probability_table[[1]]$level, gaussian_mod$probability_table[[1]]$level)
  expect_equal(mod$probability_table[[1]]$means, gaussian_mod$probability_table[[1]]$means)
  expect_equal(mod$probability_table[[1]]$stddev, gaussian_mod$probability_table[[1]]$stddev)

  expect_equal(mod$probability_table[[2]]$level, gaussian_mod$probability_table[[2]]$level)
  expect_equal(mod$probability_table[[2]]$means, gaussian_mod$probability_table[[2]]$means)
  expect_equal(mod$probability_table[[2]]$stddev, gaussian_mod$probability_table[[2]]$stddev)

  # Prediction Tests
  preds <- predict(mod, newdata = x, type = "raw")
  gaussian_preds <- predict(gaussian_mod, newdata = x, type = "raw")

  expect_equal(sum(abs(preds - gaussian_preds), na.rm = TRUE), 0)

  # Standard bernoulli model test with laplace = 1
  mod <- fastNaiveBayes.gaussian(x, y, std_threshold = 1, sparse = FALSE)
  mixed_mod <- fastNaiveBayes.mixed(x, y, std_threshold = 1, sparse = FALSE, distribution = list(gaussian = colnames(x)))
  gaussian_mod <- mixed_mod[[1]][[1]]

  expect_equal(mod$names, gaussian_mod$names)
  expect_equal(mod$priors, gaussian_mod$priors)

  expect_equal(mod$probability_table[[1]]$level, gaussian_mod$probability_table[[1]]$level)
  expect_equal(mod$probability_table[[1]]$means, gaussian_mod$probability_table[[1]]$means)
  expect_equal(mod$probability_table[[1]]$stddev, gaussian_mod$probability_table[[1]]$stddev)

  expect_equal(mod$probability_table[[2]]$level, gaussian_mod$probability_table[[2]]$level)
  expect_equal(mod$probability_table[[2]]$means, gaussian_mod$probability_table[[2]]$means)
  expect_equal(mod$probability_table[[2]]$stddev, gaussian_mod$probability_table[[2]]$stddev)

  # Prediction Tests
  preds <- predict(mod, newdata = x, type = "raw")
  gaussian_preds <- predict(gaussian_mod, newdata = x, type = "raw")

  expect_equal(sum(abs(preds - gaussian_preds), na.rm = TRUE), 0)

  # Standard bernoulli model test with laplace = 1 & Sparse
  mod <- fastNaiveBayes.gaussian(x, y, std_threshold = 1, sparse = TRUE)
  mixed_mod <- fastNaiveBayes.mixed(x, y, std_threshold = 1, sparse = TRUE, distribution = list(gaussian = colnames(x)))
  gaussian_mod <- mixed_mod[[1]][[1]]

  expect_equal(mod$names, gaussian_mod$names)
  expect_equal(mod$priors, gaussian_mod$priors)

  expect_equal(mod$probability_table[[1]]$level, gaussian_mod$probability_table[[1]]$level)
  expect_equal(mod$probability_table[[1]]$means, gaussian_mod$probability_table[[1]]$means)
  expect_equal(mod$probability_table[[1]]$stddev, gaussian_mod$probability_table[[1]]$stddev)

  expect_equal(mod$probability_table[[2]]$level, gaussian_mod$probability_table[[2]]$level)
  expect_equal(mod$probability_table[[2]]$means, gaussian_mod$probability_table[[2]]$means)
  expect_equal(mod$probability_table[[2]]$stddev, gaussian_mod$probability_table[[2]]$stddev)

  # Prediction Tests
  preds <- predict(mod, newdata = x, type = "raw")
  gaussian_preds <- predict(gaussian_mod, newdata = x, type = "raw")

  expect_equal(sum(abs(preds - gaussian_preds), na.rm = TRUE), 0)
})

test_that("Mixed event models estimation gives expected results with Multinomial model", {
  y <- as.factor(c("Ham", "Ham", "Spam", "Spam", "Spam"))
  x <- matrix(c(2, 3, 0, 1, 0, 5, 3, 0, 2, 0, 0, 1, 3, 1, 0, 0, 0, 4, 3, 5),
    nrow = 5, ncol = 4
  )
  col_names <- c("wo", "mo", "bo", "so")
  colnames(x) <- col_names

  # Standard Multinomial model test with laplace = 0
  mod <- fastNaiveBayes.multinomial(x, y, laplace = 0, sparse = FALSE)
  mixed_mod <- fastNaiveBayes.mixed(x, y, laplace = 0, sparse = FALSE, distribution = list(multinomial = colnames(x)))
  mult_mod <- mixed_mod[[1]][[1]]

  expect_equal(mod$names, mult_mod$names)
  expect_equal(mod$priors, mult_mod$priors)
  expect_equal(mod$probability_table$present, mult_mod$probability_table$present)

  # Prediction Tests
  preds <- predict(mod, newdata = x, type = "raw")
  mult_preds <- predict(mult_mod, newdata = x, type = "raw")

  expect_equal(sum(abs(preds - mult_preds), na.rm = TRUE), 0)

  # Standard bernoulli model test with laplace = 1
  mod <- fastNaiveBayes.multinomial(x, y, laplace = 1, sparse = FALSE)
  mixed_mod <- fastNaiveBayes.mixed(x, y, laplace = 1, sparse = FALSE, distribution = list(multinomial = colnames(x)))
  mult_mod <- mixed_mod[[1]][[1]]

  expect_equal(mod$names, mult_mod$names)
  expect_equal(mod$priors, mult_mod$priors)
  expect_equal(mod$probability_table$present, mult_mod$probability_table$present)

  # Prediction Tests
  preds <- predict(mod, newdata = x, type = "raw")
  mult_preds <- predict(mult_mod, newdata = x, type = "raw")

  expect_equal(sum(abs(preds - mult_preds)), 0)

  # Standard bernoulli model test with laplace = 1 & Sparse
  mod <- fastNaiveBayes.multinomial(x, y, laplace = 1, sparse = TRUE)
  mixed_mod <- fastNaiveBayes.mixed(x, y, laplace = 1, sparse = TRUE, distribution = list(multinomial = colnames(x)))
  mult_mod <- mixed_mod[[1]][[1]]

  expect_equal(mod$names, mult_mod$names)
  expect_equal(mod$priors, mult_mod$priors)
  expect_equal(mod$probability_table$present, mult_mod$probability_table$present)

  # Prediction Tests
  preds <- predict(mod, newdata = x, type = "raw")
  mult_preds <- predict(mult_mod, newdata = x, type = "raw")

  expect_equal(sum(abs(preds - mult_preds)), 0)
})

test_that("Mixed event models estimation gives expected results with Bernoulli model", {
  y <- as.factor(c("Ham", "Ham", "Spam", "Spam", "Spam"))
  x <- matrix(c(1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1),
    nrow = 5, ncol = 4
  )
  col_names <- c("wo", "mo", "bo", "so")
  colnames(x) <- col_names

  # Standard bernoulli model test with laplace = 0
  mod <- fastNaiveBayes.bernoulli(x, y, laplace = 0, sparse = FALSE)
  mixed_mod <- fastNaiveBayes.mixed(x, y, laplace = 0, sparse = FALSE, distribution = list(bernoulli = colnames(x)))
  bern_mod <- mixed_mod[[1]][[1]]

  expect_equal(mod$names, bern_mod$names)
  expect_equal(mod$priors, bern_mod$priors)
  expect_equal(mod$probability_table$present, bern_mod$probability_table$present)
  expect_equal(mod$probability_table$non_present, bern_mod$probability_table$non_present)

  # Prediction Tests
  preds <- predict(mod, newdata = x, type = "raw")
  bern_preds <- predict(bern_mod, newdata = x, type = "raw")

  expect_equal(sum(abs(preds - bern_preds), na.rm = TRUE), 0)

  # Standard bernoulli model test with laplace = 1
  mod <- fastNaiveBayes.bernoulli(x, y, laplace = 1, sparse = FALSE)
  mixed_mod <- fastNaiveBayes.mixed(x, y, laplace = 1, sparse = FALSE, distribution = list(bernoulli = colnames(x)))
  bern_mod <- mixed_mod[[1]][[1]]

  expect_equal(mod$names, bern_mod$names)
  expect_equal(mod$priors, bern_mod$priors)
  expect_equal(mod$probability_table$present, bern_mod$probability_table$present)
  expect_equal(mod$probability_table$non_present, bern_mod$probability_table$non_present)

  # Prediction Tests
  preds <- predict(mod, newdata = x, type = "raw")
  bern_preds <- predict(bern_mod, newdata = x, type = "raw")

  expect_equal(sum(abs(preds - bern_preds)), 0)

  # Standard bernoulli model test with laplace = 1 & Sparse
  mod <- fastNaiveBayes.bernoulli(x, y, laplace = 1, sparse = TRUE)
  mixed_mod <- fastNaiveBayes.mixed(x, y, laplace = 1, sparse = TRUE, distribution = list(bernoulli = colnames(x)))
  bern_mod <- mixed_mod[[1]][[1]]

  expect_equal(mod$names, bern_mod$names)
  expect_equal(mod$priors, bern_mod$priors)
  expect_equal(mod$probability_table$present, bern_mod$probability_table$present)
  expect_equal(mod$probability_table$non_present, bern_mod$probability_table$non_present)

  # Prediction Tests
  preds <- predict(mod, newdata = x, type = "raw")
  bern_preds <- predict(bern_mod, newdata = x, type = "raw")

  expect_equal(sum(abs(preds - bern_preds)), 0)
})
