#' @export
#' @import Matrix
#' @rdname fastNaiveBayes.mixed
fastNaiveBayes.mixed.default <- function(x,
                                         y,
                                         laplace = 0,
                                         sparse = FALSE,
                                         distribution = NULL, ...) {
  if (nrow(x) != length(y)) {
    stop("X and Y must be equal length")
  }

  if(!is.factor(y)){
    y <- as.factor(y)
  }

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

  if(any(is.na(x))){
    x[is.na(x)] <- 0
  }

  if (is.null(distribution)) {
    distribution <- fastNaiveBayes.detect_distribution(x)
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
             fastNaiveBayes.bernoulli(newx, y, laplace, sparse)
           },
           multinomial = {
             newx <- x[, distribution[[dist]]]
             if (length(distribution[[dist]]) == 1) {
               newx <- as.matrix(newx)
               colnames(newx) <- distribution[[dist]]
             }
             fastNaiveBayes.multinomial(newx, y, laplace, sparse)
           },
           gaussian = {
             newx <- x[, distribution[[dist]]]
             if (length(distribution[[dist]]) == 1) {
               newx <- as.matrix(newx)
               colnames(newx) <- distribution[[dist]]
             }
             fastNaiveBayes.gaussian(newx, y, sparse)
           }
    )
  })

  priors <- table(y) / nrow(x)
  structure(list(
    models = models,
    priors = priors,
    names = colnames(x),
    distribution = distribution
  ),
  class = "fastNaiveBayes.mixed"
  )
}
