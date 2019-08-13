#' @export
#' @import Matrix
#' @rdname fastNaiveBayesF
fnb.mixed <- function(x, y, laplace = 0, sparse = FALSE, distribution = NULL, ...) {
  UseMethod("fnb.mixed")
}

#' @export
#' @import Matrix
#' @rdname fastNaiveBayesF
fnb.mixed.default <- function(x, y, laplace = 0, sparse = FALSE, distribution = NULL, ...) {
  if (class(x)[1] != "dgCMatrix") {
    if (!is.matrix(x)) {
      x <- as.matrix(x)
    }
    if (sparse) {
      x <- Matrix(x, sparse = TRUE)
    }
  } else {
    sparse <- TRUE
  }

  if (is.null(distribution)) {
    distribution <- fnb.detect_distribution(x)
  }
  distribution <- distribution[lengths(distribution) != 0]

  models <- lapply(names(distribution), function(dist) {
    switch(dist,
           bernoulli = {
             newx <- x[, distribution[[dist]]]
             if (length(distribution[[dist]]) == 1) {
               newx <- as.matrix(newx)
               colnames(newx) <- distribution[[dist]]
             }
             fnb.bernoulli(newx, y, laplace, sparse)
           },
           multinomial = {
             newx <- x[, distribution[[dist]]]
             if (length(distribution[[dist]]) == 1) {
               newx <- as.matrix(newx)
               colnames(newx) <- distribution[[dist]]
             }
             fnb.multinomial(newx, y, laplace, sparse)
           },
           gaussian = {
             newx <- x[, distribution[[dist]]]
             if (length(distribution[[dist]]) == 1) {
               newx <- as.matrix(newx)
               colnames(newx) <- distribution[[dist]]
             }
             fnb.gaussian(newx, y, sparse)
           }
    )
  })

  n <- tabulate(y)
  priors <- n / nrow(x)

  structure(list(
    models = models,
    priors = priors,
    names = colnames(x),
    distribution = distribution,
    levels = levels(y)),

    class = "fnb.mixed"
  )
}

