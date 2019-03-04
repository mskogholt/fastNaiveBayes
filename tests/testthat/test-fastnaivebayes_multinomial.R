context("Test fastNaiveBayes Multinomial Training Function")

test_that("Multinomial estimation gives expected results", {
  y <- as.factor(c('Ham','Ham','Spam','Spam','Spam'))
  x <- matrix(c(2,3,0,1,0,5,3,0,2,0,0,1,3,1,0,0,0,4,3,5),
              nrow = 5, ncol = 4)
  col_names <- c('wo','mo','bo','so')
  colnames(x) <- col_names

  # Standard Multinomial model test with laplace = 0
  mod <- fastNaiveBayes.multinomial(x, y, laplace = 0, sparse = FALSE)

  priors <- mod$priors
  expect_equal(priors[[1]], 0.4)
  expect_equal(priors[[2]], 0.6)

  expect_equal(mod$names, col_names)

  prob_table <- mod$probability_table

  real_present <- matrix(c(5/14, 1/19,
                           8/14, 2/19,
                           1/14, 4/19,
                           0,    12/19),
                         nrow = 2, ncol = 4)
  expect_equal(sum(abs(prob_table$present-real_present)),0)

  # Multinomial model test with laplace = 1
  mod <- fastNaiveBayes.multinomial(x,y,laplace = 1, sparse = FALSE)

  priors <- mod$priors
  expect_equal(priors[[1]], 0.4)
  expect_equal(priors[[2]], 0.6)

  expect_equal(mod$names, col_names)

  prob_table <- mod$probability_table

  real_present <- matrix(c(6/18, 2/23,
                           9/18, 3/23,
                           2/18, 5/23,
                           1/18, 13/23),
                         nrow = 2, ncol = 4)
  expect_equal(sum(abs(prob_table$present-real_present)),0)

  # Test sparse casting, should produce same results
  sparse_mod <- fastNaiveBayes.multinomial(x,y,laplace = 1, sparse = TRUE)
  expect_equal(mod$names,sparse_mod$names)
  expect_equal(mod$priors,sparse_mod$priors)

  # Does not work with lists
  expect_equal(sum(abs(mod$probability_table$present-sparse_mod$probability_table$present))
               ,0)
})
