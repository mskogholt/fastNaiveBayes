context("Test fnb.io function")

test_that("fnb.load and fnb.save works as expected", {

  # Test
  x <- as.matrix(tweetsDTM[,2:ncol(tweetsDTM)])
  y <- as.factor(tweetsDTM[,1])

  x[x>1] <- 1

  mod <- fnb.bernoulli(x, y, check = FALSE, sparse=TRUE)
  fnb.save(mod, "fnbmod")
  mod2 <- fnb.load("fnbmod")
  expect_equal(identical(mod, mod2), TRUE)

  expect_error(fnb.load("nonexistingfile"))
  expect_error(fnb.save(mod, "fnbmod", overwrite = FALSE))
  expect_error(fnb.save(x, "nonmodelobject"))

  mod2 <- fnb.load(fnb.save(mod, "fnbmod", overwrite = TRUE))
  expect_equal(identical(mod, mod2), TRUE)

  teardown({
    file.remove("fnbmod")
  })

})
