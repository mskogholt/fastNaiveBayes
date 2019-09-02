context("Testing fnb.update")

test_that("fnb.update gives expected results", {

  data <- tweetsDTM[1:200]
  y <- as.factor(data[,1])
  x <- as.matrix(data[,2:ncol(data)])


  # Bernoulli model test with laplace = 1
  mod <- fnb.bernoulli(x, y, laplace = 1, sparse = FALSE)
  base <- fnb.bernoulli(x[1:100,], y[1:100], laplace = 1, sparse = FALSE)
  altmod <- fnb.update(base, x[101:nrow(x),], y[101:nrow(x)], sparse = FALSE)

  mod_preds <- predict(mod, newdata = x, type = "raw")
  alt_preds <- predict(altmod, newdata = x, type = "raw")

  expect_equal(sum(abs(round(mod_preds - alt_preds, digits = 8))), 0)

})
