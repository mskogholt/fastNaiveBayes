#' @export
#' @import Matrix
#' @rdname fastNaiveBayes.multinomial
fastNaiveBayes.multinomial.default <- function(x, y, laplace = 0, sparse = FALSE, ...) {
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

  if (nrow(x) != length(y)) {
    stop("X and Y must be equal length")
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

  priors <- table(y) / nrow(x)
  structure(list(
    probability_table = probability_table,
    priors = priors,
    names = colnames(x)
  ),
  class = "fastNaiveBayes.multinomial"
  )
}
