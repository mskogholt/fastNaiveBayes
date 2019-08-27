#' @export
#' @import Matrix
#' @rdname fastNaiveBayesF
fnb.multinomial <- function(x, y, priors = NULL, laplace = 0, sparse = FALSE, check = TRUE, ...) {
  UseMethod("fnb.multinomial")
}

#' @export
#' @import Matrix
#' @rdname fastNaiveBayesF
fnb.multinomial.default <- function(x, y, priors = NULL, laplace = 0, sparse = FALSE, check = TRUE, ...) {
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
      if (nrow(x) == nlevels(y)) {
        present <- lapply(levels(y), function(level) {
          Matrix::colSums(Matrix(t(as.matrix(x[y == level, ])), sparse = TRUE))
        })
        present <- do.call(rbind, present)
      } else {
        present <- lapply(levels(y), function(level) {
          Matrix::colSums(x[y == level, ])
        })
        present <- do.call(rbind, present)
      }
    }
  } else {
    present <- rowsum(x, y)
  }
  present <- present + laplace
  total <- rowSums(present)

  present <- present / total

  probability_table <- list(present = present)

  if(is.null(priors)){
    priors <- tabulate(y) / nrow(x)
  }

  structure(list(
    probability_table = probability_table,
    priors = priors,
    names = colnames(x),
    levels = levels(y)),

    class = "fnb.multinomial"
  )
}

