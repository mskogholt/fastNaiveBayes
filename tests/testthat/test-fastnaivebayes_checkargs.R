context("Test fastNaiveBayes Multinomial Training Function")

test_that("Multinomial estimation gives expected results", {
  # Test 2
  y <- as.factor(c("Ham", "Ham", "Spam", "Spam", "Spam"))
  x <- matrix(c(2, 3, 0, 1, 0, 5, 3, 0, 2, 0, 0, 1, 3, 1, 0, 0, 0, 4, 3, 5),
    nrow = 5, ncol = 4)
  colnames(x) <- c("wo", "mo", "bo", "so")
  x <- as.data.frame(x)

  expect_equal(fastNaiveBayes:::fnb.check.args.dist(as.data.frame(x), nrow(x)), fastNaiveBayes:::fnb.check.args.dist(x, nrow(x)))
  expect_error(fastNaiveBayes:::fnb.check.args.dist(x, nrows = 0))

  mod <- fnb.multinomial(x, y)
  expect_error(fastNaiveBayes:::fnb.check.args.predict(object=mod, newdata=x, type="raw", sparse = FALSE, threshold = -1))
  expect_error(fastNaiveBayes:::fnb.check.args.predict(object=mod, newdata=x[,1:2], type="raw", sparse = TRUE, threshold = -1))

  expect_error(fastNaiveBayes:::fnb.check.args.model(x[2:5,], y[2:5], priors = NULL, laplace = 0, sparse = FALSE, distribution = NULL))
  expect_error(fastNaiveBayes:::fnb.check.args.model(x, y, priors = NULL, laplace = -1, sparse = FALSE, distribution = NULL))
  expect_error(fastNaiveBayes:::fnb.check.args.model(x, y, priors = list(1,2), laplace = 0, sparse = FALSE, distribution = NULL))
  expect_error(fastNaiveBayes:::fnb.check.args.model(x, y, priors = c(1,2,3), laplace = 0, sparse = FALSE, distribution = NULL))
  expect_error(fastNaiveBayes:::fnb.check.args.model(x, y, priors = c(1,2), laplace = 0, sparse = FALSE, distribution = NULL))
  expect_error(fastNaiveBayes:::fnb.check.args.model(x, y, priors = NULL, laplace = 0, sparse = FALSE,
                                                     distribution = c("bernoulli")))

  expect_error(fastNaiveBayes:::fnb.check.args.model(x, y, priors = NULL, laplace = 0, sparse = FALSE,
                                                     distribution = list("martin"=c("h","a"))))

  expect_warning(fastNaiveBayes:::fnb.check.args.model(x, y, priors = NULL, laplace = 0, sparse = FALSE,
                                                     distribution = list("bernoulli"=c("h","a"),
                                                                         "martin"=c("a","b"))))

  expect_error(fastNaiveBayes:::fnb.check.args.model(x, y, priors = NULL, laplace = 0, sparse = FALSE,
                                                     distribution = c("bernoulli")))

})
