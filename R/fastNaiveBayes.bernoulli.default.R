#' @export
#' @import Matrix
#' @rdname fastNaiveBayes.bernoulli
fastNaiveBayes.bernoulli.default <- function(x, y, laplace = 0, sparse = FALSE, ...) {
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

      x <- 1 - x

      non_present <- lapply(levels(y), function(level) {
        Matrix::colSums(Matrix(as.matrix(x[y == level, ]), sparse = TRUE))
      })
      non_present <- do.call(rbind, non_present)
    } else {
      present <- lapply(levels(y), function(level) {
        Matrix::colSums(x[y == level, ])
      })
      present <- do.call(rbind, present)

      x <- 1 - x

      non_present <- lapply(levels(y), function(level) {
        Matrix::colSums(x[y == level, ])
      })
      non_present <- do.call(rbind, non_present)
    }
  } else {
    present <- rowsum(x, y)
    non_present <- rowsum(1 - x, y)
  }

  present <- present + laplace
  non_present <- non_present + laplace
  total <- present + non_present

  present <- present / total
  non_present <- non_present / total
  probability_table <- list(
    present = present,
    non_present = non_present
  )

  priors <- table(y) / nrow(x)
  structure(list(
    probability_table = probability_table,
    priors = priors,
    names = colnames(x)
  ),
  class = "fastNaiveBayes.bernoulli"
  )
}
