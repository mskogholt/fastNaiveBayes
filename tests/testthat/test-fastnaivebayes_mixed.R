context("Test fastNaiveBayes Mixed Training Function")

test_that("Mixed event models estimation gives expected results when mixed", {
  y <- as.factor(c('Ham','Ham','Spam','Spam','Spam'))

  x1 <- matrix(c(2,3,0,1,0,5,3,0,2,0,0,1,3,1,0,0,0,4,3,5),
              nrow = 5, ncol = 4)
  colnames(x1) <- c('wo','mo','bo','so')

  x2 <- matrix(c(1,0,0,0,0,1,1,0,1,0,0,1,1,1,0,0,0,1,1,1),
              nrow = 5, ncol = 4)
  colnames(x2) <- c('no','ko','po','lo')

  x <- cbind(x1,x2)
  col_names <- c('wo','mo','bo','so','no','ko','po','lo')
  colnames(x) <- col_names

  # Standard Multinomial model test with laplace = 0
  mixed_mod <- fastNaiveBayes.mixed(x, y, laplace = 0, sparse = FALSE,
                                    distribution = list(multinomial = colnames(x1),
                                                        bernoulli = colnames(x2)))
  mult_mod <- mixed_mod[[1]][[1]]
  bern_mod <- mixed_mod[[1]][[2]]

  expect_equal(mixed_mod$names, col_names)

  expect_equal(mixed_mod$priors[[1]], 0.4)
  expect_equal(mixed_mod$priors[[2]], 0.6)

  expect_equal(mixed_mod$distribution, list(multinomial = colnames(x1),
                                            bernoulli = colnames(x2)))

  mod <- fastNaiveBayes.bernoulli(x2, y, laplace = 0, sparse = FALSE)

  expect_equal(mod$priors,bern_mod$priors)
  expect_equal(mod$probability_table$present, bern_mod$probability_table$present)
  expect_equal(mod$probability_table$non_present, bern_mod$probability_table$non_present)

  mod <- fastNaiveBayes.multinomial(x1, y, laplace = 0, sparse = FALSE)

  expect_equal(mod$names,mult_mod$names)
  expect_equal(mod$priors,mult_mod$priors)
  expect_equal(mod$probability_table$present, mult_mod$probability_table$present)
})

test_that("Mixed event models estimation gives expected results with Gaussian model", {
  y <- as.factor(c('Ham','Ham','Spam','Spam','Spam'))
  x <- matrix(c(2,3,0,1,0,5,3,0,2,0,0,1,3,1,0,0,0,4,3,5),
              nrow = 5, ncol = 4)
  col_names <- c('wo','mo','bo','so')
  colnames(x) <- col_names

  # Standard Multinomial model test with laplace = 0
  mod <- fastNaiveBayes.gaussian(x, y, laplace = 0, sparse = FALSE)
  mixed_mod <- fastNaiveBayes.mixed(x, y, laplace = 0, sparse = FALSE, distribution = list(gaussian=colnames(x)))
  bern_mod <- mixed_mod[[1]][[1]]

  expect_equal(mod$names,bern_mod$names)

  expect_equal(mod$priors,bern_mod$priors)

  expect_equal(mod$probability_table[[1]]$level, bern_mod$probability_table[[1]]$level)
  expect_equal(mod$probability_table[[1]]$means, bern_mod$probability_table[[1]]$means)
  expect_equal(mod$probability_table[[1]]$stddev, bern_mod$probability_table[[1]]$stddev)

  expect_equal(mod$probability_table[[2]]$level, bern_mod$probability_table[[2]]$level)
  expect_equal(mod$probability_table[[2]]$means, bern_mod$probability_table[[2]]$means)
  expect_equal(mod$probability_table[[2]]$stddev, bern_mod$probability_table[[2]]$stddev)

  # Standard bernoulli model test with laplace = 1
  mod <- fastNaiveBayes.gaussian(x, y, laplace = 1, sparse = FALSE)
  mixed_mod <- fastNaiveBayes.mixed(x, y, laplace = 1, sparse = FALSE, distribution = list(gaussian=colnames(x)))
  bern_mod <- mixed_mod[[1]][[1]]

  expect_equal(mod$names,bern_mod$names)

  expect_equal(mod$priors,bern_mod$priors)

  expect_equal(mod$probability_table[[1]]$level, bern_mod$probability_table[[1]]$level)
  expect_equal(mod$probability_table[[1]]$means, bern_mod$probability_table[[1]]$means)
  expect_equal(mod$probability_table[[1]]$stddev, bern_mod$probability_table[[1]]$stddev)

  expect_equal(mod$probability_table[[2]]$level, bern_mod$probability_table[[2]]$level)
  expect_equal(mod$probability_table[[2]]$means, bern_mod$probability_table[[2]]$means)
  expect_equal(mod$probability_table[[2]]$stddev, bern_mod$probability_table[[2]]$stddev)

  # Standard bernoulli model test with laplace = 1 & Sparse
  mod <- fastNaiveBayes.gaussian(x, y, laplace = 1, sparse = TRUE)
  mixed_mod <- fastNaiveBayes.mixed(x, y, laplace = 1, sparse = TRUE, distribution = list(gaussian=colnames(x)))
  bern_mod <- mixed_mod[[1]][[1]]

  expect_equal(mod$names,bern_mod$names)

  expect_equal(mod$priors,bern_mod$priors)

  expect_equal(mod$probability_table[[1]]$level, bern_mod$probability_table[[1]]$level)
  expect_equal(mod$probability_table[[1]]$means, bern_mod$probability_table[[1]]$means)
  expect_equal(mod$probability_table[[1]]$stddev, bern_mod$probability_table[[1]]$stddev)

  expect_equal(mod$probability_table[[2]]$level, bern_mod$probability_table[[2]]$level)
  expect_equal(mod$probability_table[[2]]$means, bern_mod$probability_table[[2]]$means)
  expect_equal(mod$probability_table[[2]]$stddev, bern_mod$probability_table[[2]]$stddev)
})

test_that("Mixed event models estimation gives expected results with Multinomial model", {
  y <- as.factor(c('Ham','Ham','Spam','Spam','Spam'))
  x <- matrix(c(2,3,0,1,0,5,3,0,2,0,0,1,3,1,0,0,0,4,3,5),
              nrow = 5, ncol = 4)
  col_names <- c('wo','mo','bo','so')
  colnames(x) <- col_names

  # Standard Multinomial model test with laplace = 0
  mod <- fastNaiveBayes.multinomial(x, y, laplace = 0, sparse = FALSE)
  mixed_mod <- fastNaiveBayes.mixed(x, y, laplace = 0, sparse = FALSE, distribution = list(multinomial=colnames(x)))
  bern_mod <- mixed_mod[[1]][[1]]

  expect_equal(mod$names,bern_mod$names)

  expect_equal(mod$priors,bern_mod$priors)

  expect_equal(mod$probability_table$present, bern_mod$probability_table$present)

  # Standard bernoulli model test with laplace = 1
  mod <- fastNaiveBayes.multinomial(x, y, laplace = 1, sparse = FALSE)
  mixed_mod <- fastNaiveBayes.mixed(x, y, laplace = 1, sparse = FALSE, distribution = list(multinomial=colnames(x)))
  bern_mod <- mixed_mod[[1]][[1]]

  expect_equal(mod$names,bern_mod$names)

  expect_equal(mod$priors,bern_mod$priors)

  expect_equal(mod$probability_table$present, bern_mod$probability_table$present)

  # Standard bernoulli model test with laplace = 1 & Sparse
  mod <- fastNaiveBayes.multinomial(x, y, laplace = 1, sparse = TRUE)
  mixed_mod <- fastNaiveBayes.mixed(x, y, laplace = 1, sparse = TRUE, distribution = list(multinomial=colnames(x)))
  bern_mod <- mixed_mod[[1]][[1]]

  expect_equal(mod$names,bern_mod$names)

  expect_equal(mod$priors,bern_mod$priors)

  expect_equal(mod$probability_table$present, bern_mod$probability_table$present)

})

test_that("Mixed event models estimation gives expected results with Bernoulli model", {
  y <- as.factor(c('Ham','Ham','Spam','Spam','Spam'))
  x <- matrix(c(1,0,0,0,0,1,1,0,1,0,0,1,1,1,0,0,0,1,1,1),
              nrow = 5, ncol = 4)
  col_names <- c('wo','mo','bo','so')
  colnames(x) <- col_names

  # Standard bernoulli model test with laplace = 0
  mod <- fastNaiveBayes.bernoulli(x, y, laplace = 0, sparse = FALSE)
  mixed_mod <- fastNaiveBayes.mixed(x, y, laplace = 0, sparse = FALSE, distribution = list(bernoulli=colnames(x)))
  bern_mod <- mixed_mod[[1]][[1]]

  expect_equal(mod$names,bern_mod$names)

  expect_equal(mod$priors,bern_mod$priors)

  expect_equal(mod$probability_table$present, bern_mod$probability_table$present)

  expect_equal(mod$probability_table$non_present, bern_mod$probability_table$non_present)

  # Standard bernoulli model test with laplace = 1
  mod <- fastNaiveBayes.bernoulli(x, y, laplace = 1, sparse = FALSE)
  mixed_mod <- fastNaiveBayes.mixed(x, y, laplace = 1, sparse = FALSE, distribution = list(bernoulli=colnames(x)))
  bern_mod <- mixed_mod[[1]][[1]]

  expect_equal(mod$names,bern_mod$names)

  expect_equal(mod$priors,bern_mod$priors)

  expect_equal(mod$probability_table$present, bern_mod$probability_table$present)

  expect_equal(mod$probability_table$non_present, bern_mod$probability_table$non_present)

  # Standard bernoulli model test with laplace = 1 & Sparse
  mod <- fastNaiveBayes.bernoulli(x, y, laplace = 1, sparse = TRUE)
  mixed_mod <- fastNaiveBayes.mixed(x, y, laplace = 1, sparse = TRUE, distribution = list(bernoulli=colnames(x)))
  bern_mod <- mixed_mod[[1]][[1]]

  expect_equal(mod$names,bern_mod$names)

  expect_equal(mod$priors,bern_mod$priors)

  expect_equal(mod$probability_table$present, bern_mod$probability_table$present)

  expect_equal(mod$probability_table$non_present, bern_mod$probability_table$non_present)

})
