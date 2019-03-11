#' @export
#' @import Matrix
#' @rdname fastNaiveBayes.detect_distribution
fastNaiveBayes.detect_distribution.default <- function(x, nrows = 10) {
  nrows <- min(nrow(x), nrows)
  x <- x[1:nrows, ]

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
