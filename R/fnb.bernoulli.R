#' @export
#' @import Matrix
#' @rdname fastNaiveBayesF
fnb.bernoulli <- function(x, y, priors = NULL, laplace = 0, sparse = FALSE, check = TRUE, ...) {
  UseMethod("fnb.bernoulli")
}

#' @export
#' @import Matrix
#' @rdname fastNaiveBayesF
fnb.bernoulli.default <- function(x, y, priors = NULL, laplace = 0, sparse = FALSE, check = TRUE, ...) {
  if(check){
    args <- fnb.check.args.model(x, y, priors, laplace, sparse)
    x <- args$x
    y <- args$y
    priors <- args$priors
    laplace <- args$laplace
    sparse <- args$sparse
  }

  if (sparse) {
    if (ncol(x) == 1) {
      present <- lapply(levels(y), function(level) {
        Matrix::colSums(Matrix(as.matrix(x[y == level, ]), sparse = TRUE))
      })
      present <- do.call(rbind, present)
    } else {
      present <- lapply(levels(y), function(level) {
        Matrix::colSums(x[y == level, ])
      })
      present <- do.call(rbind, present)
    }
  } else {
    present <- rowsum(x, y)
  }

  n <- tabulate(y)

  present <- present + laplace
  present <- present / (n+2*laplace)



  non_present <- 1-present

  probability_table <- list(
    present = present,
    non_present = non_present
  )

  if(is.null(priors)){
    priors <- n / nrow(x)
  }

  structure(list(
    probability_table = probability_table,
    priors = priors,
    present = present,
    names = colnames(x),
    levels = levels(y)),

    class = "fnb.bernoulli"
  )
}
