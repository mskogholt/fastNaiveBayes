#' @export
#' @import Matrix
#' @rdname fastNaiveBayesF
fnb.train <- function(x, y, priors = NULL, laplace = 0, sparse = FALSE, check = TRUE, distribution = NULL, ...) {
  UseMethod("fnb.train")
}

#' @export
#' @import Matrix
#' @rdname fastNaiveBayesF
fnb.train.default <- function(x, y, priors = NULL, laplace = 0, sparse = FALSE, check = TRUE, distribution = NULL, ...) {
  if(check){
    args <- fnb.check.args.model(x, y, priors, laplace, sparse, distribution)
    x <- args$x
    y <- args$y
    priors <- args$priors
    laplace <- args$laplace
    sparse <- args$sparse
    distribution <- args$distribution
  }

  models <- lapply(names(distribution), function(dist) {
    switch(dist,
           bernoulli = {
             newx <- x[, distribution[[dist]]]
             if (length(distribution[[dist]]) == 1) {
               newx <- as.matrix(newx)
               colnames(newx) <- distribution[[dist]]
             }
             fnb.bernoulli(newx, y, priors, laplace, sparse)
           },
           multinomial = {
             newx <- x[, distribution[[dist]]]
             if (length(distribution[[dist]]) == 1) {
               newx <- as.matrix(newx)
               colnames(newx) <- distribution[[dist]]
             }
             fnb.multinomial(newx, y, priors, laplace, sparse)
           },
           gaussian = {
             newx <- x[, distribution[[dist]]]
             if (length(distribution[[dist]]) == 1) {
               newx <- as.matrix(newx)
               colnames(newx) <- distribution[[dist]]
             }
             fnb.gaussian(newx, y, priors, sparse)
           }
    )
  })

  if(is.null(priors)){
    priors <- tabulate(y) / nrow(x)
  }

  structure(list(
    models = models,
    priors = priors,
    names = colnames(x),
    distribution = distribution,
    levels = levels(y)),

    class = "fastNaiveBayes"
  )
}

