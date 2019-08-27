#' @title Distribution Detection Function
#' @description Determines which distribution to use for which columns in the matrix based
#' on a set of rules.
#'
#' @param x a numeric matrix, or a dgcMatrix
#' @param nrows number of rows to use to detect distributions
#' @param ... Not used.
#'
#' @details A simple utility function to detect the distribution to use for each columns
#'
#' @return A list of distribution names mapped to column names
#'
#' @export
#' @import Matrix
#' @examples
#' rm(list = ls())
#' library(fastNaiveBayes)
#' cars <- mtcars
#' y <- as.factor(ifelse(cars$mpg > 25, "High", "Low"))
#' x <- cars[, 2:ncol(cars)]
#'
#' # Uses default of all rows to determine distributions
#' dist <- fnb.detect_distribution(x)
#' print(dist)
#'
#' # Uses top 10 rows of x to determine distributions. If top 10 rows
#' # are representative this is can be much faster.
#' dist <- fnb.detect_distribution(x, nrows = 10)
#' print(dist)
#' @rdname fnb.detect_distribution
fnb.detect_distribution <- function(x, nrows = nrow(x)) {
  UseMethod("fnb.detect_distribution")
}

#' @export
#' @import Matrix
#' @rdname fnb.detect_distribution
fnb.detect_distribution.default <- function(x, nrows = nrow(x)) {

  x <- fnb.check.args.dist(x, nrows)

  integersums <- colSums(abs(x - round(x)) > .Machine$double.eps)
  ones <- colSums(x > 1)

  distribution <- list(
    bernoulli = names(integersums[integersums == 0 & ones == 0]),
    multinomial = names(integersums[integersums == 0 & ones != 0]),
    gaussian = names(integersums[integersums != 0])
  )
  distribution <- distribution[lengths(distribution) != 0]
  return(distribution)
}
