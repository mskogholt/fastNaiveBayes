#' @export
#' @import Matrix
#' @rdname fastNaiveBayes
fnb.bernoulli <- function(x, y, laplace = 0, sparse = FALSE, ...) {
  UseMethod("fnb.bernoulli")
}

#' @export
#' @import Matrix
#' @rdname fastNaiveBayes
fnb.bernoulli.default <- function(x, y, laplace = 0, sparse = FALSE, ...) {
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

  priors <- n / nrow(x)
  structure(list(
    probability_table = probability_table,
    priors = priors,
    present = present,
    names = colnames(x),
    levels = levels(y)),

    class = "fnb.bernoulli"
  )
}
