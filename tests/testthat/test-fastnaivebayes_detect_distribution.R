context("test-fastnaivebayes_detect_distribution")

test_that("Check distribution detection function", {
  x <- matrix(c(2, 3, 2, 1, 2, 5, 3, 4, 2, 4, 0, 1, 1, 1, 0, 3, 4, 4, 3, 5),
    nrow = 5, ncol = 4
  )
  x <- cbind(x, rnorm(5))
  col_names <- c("wo", "mo", "bo", "so", "ma")
  colnames(x) <- col_names

  real_distribution <- list(
    bernoulli = c("bo"),
    multinomial = c("wo", "mo", "so"),
    gaussian = c("ma")
  )

  expect_equal(real_distribution, fastNaiveBayes.detect_distribution(x))

  x <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
  colnames(x) <- c("wo", "mo")
  distribution <- fastNaiveBayes.detect_distribution(x)

  real_distribution <- list(
    multinomial = c("wo", "mo")
  )

  expect_equal(real_distribution, distribution)
})
