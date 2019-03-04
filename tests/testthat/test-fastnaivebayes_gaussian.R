context("Test fastNaiveBayes Gaussian Training Function")

test_that("Gaussian estimation gives expected results", {

  y <- as.factor(c('Ham','Ham','Spam','Spam','Spam'))
  x <- matrix(c(2,3,0,1,0,5,3,0,2,0,0,1,3,1,0,0,0,4,3,5),
              nrow = 5, ncol = 4)
  col_names <- c('wo','mo','bo','so')
  colnames(x) <- col_names

  # Standard Multinomial model test with laplace = 0
  mod <- fastNaiveBayes.gaussian(x, y, laplace = 0, sparse = FALSE)

  priors <- mod$priors
  expect_equal(priors[[1]], 0.4)
  expect_equal(priors[[2]], 0.6)

  expect_equal(mod$names, col_names)

  prob_table <- mod$probability_table

  real_ham_avg <- c(2.5,4,0.5,0)
  real_ham_sd <- c(sd(c(2,3)),sd(c(5,3)),sd(c(0,1)),sd(c(0,0)))

  expect_equal(sum(abs(prob_table[[1]]$means-real_ham_avg)),0)
  expect_equal(sum(abs(prob_table[[1]]$stddev-real_ham_sd)),0)

  real_spam_avg <- c(1/3,2/3,4/3,4)
  real_spam_sd <- c(sd(c(0,1,0)),sd(c(0,2,0)),sd(c(3,1,0)),sd(c(4,3,5)))

  expect_equal(sum(abs(prob_table[[2]]$means-real_spam_avg)),0)
  expect_equal(sum(abs(prob_table[[2]]$stddev-real_spam_sd)),0)

  # Multinomial model test with laplace = 1
  mod <- fastNaiveBayes.gaussian(x,y,laplace = 1, sparse = FALSE)

  priors <- mod$priors
  expect_equal(priors[[1]], 0.4)
  expect_equal(priors[[2]], 0.6)

  expect_equal(mod$names, col_names)

  prob_table <- mod$probability_table
  real_ham_avg <- c(2.5,4,0.5,0)
  real_ham_sd <- c(sd(c(2,3)),sd(c(5,3)),sd(c(0,1)),sd(c(0,0)))

  expect_equal(sum(abs(prob_table[[1]]$means-real_ham_avg)),0)
  expect_equal(sum(abs(prob_table[[1]]$stddev-real_ham_sd)),0)

  real_spam_avg <- c(1/3,2/3,4/3,4)
  real_spam_sd <- c(sd(c(0,1,0)),sd(c(0,2,0)),sd(c(3,1,0)),sd(c(4,3,5)))

  expect_equal(sum(abs(prob_table[[2]]$means-real_spam_avg)),0)
  expect_equal(sum(abs(prob_table[[2]]$stddev-real_spam_sd)),0)

  # Test sparse casting, should produce same results
  sparse_mod <- fastNaiveBayes.gaussian(x,y,laplace = 1, sparse = TRUE)
  expect_equal(mod$names,sparse_mod$names)
  expect_equal(mod$priors,sparse_mod$priors)

  expect_equal(mod$probability_table[[1]]$level,
               sparse_mod$probability_table[[1]]$level)

  expect_equal(mod$probability_table[[1]]$means,
               sparse_mod$probability_table[[1]]$means)

  expect_equal(mod$probability_table[[1]]$stddev,
               sparse_mod$probability_table[[1]]$stddev)

  expect_equal(mod$probability_table[[2]]$level,
               sparse_mod$probability_table[[2]]$level)

  expect_equal(mod$probability_table[[2]]$means,
               sparse_mod$probability_table[[2]]$means)

  expect_equal(mod$probability_table[[2]]$stddev,
               sparse_mod$probability_table[[2]]$stddev)
})
