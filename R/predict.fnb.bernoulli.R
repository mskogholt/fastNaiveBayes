#' @export
#' @import Matrix
#' @rdname predict.fastNaiveBayes
predict.fnb.bernoulli <- function(object, newdata, type = c("class", "raw", "rawprob"),
                                  sparse = FALSE, threshold = .Machine$double.eps, ...) {
  type <- match.arg(type)
  if (class(newdata)[1] != "dgCMatrix") {
    if (!is.matrix(newdata)) {
      newdata <- as.matrix(newdata)
    }
    if (sparse) {
      newdata <- Matrix(newdata, sparse = TRUE)
    }
  } else {
    sparse <- TRUE
  }

  names <- object$names
  data <- object$probability_table

  present <- log(data$present)
  nonpresent <- log(data$non_present)

  present[is.infinite(present)] <- max(-100000, log(threshold))
  nonpresent[is.infinite(nonpresent)] <- max(-100000, log(threshold))

  presence_prob <- newdata %*% t(present)
  nonpresence_prob <- matrix(base::colSums(t(nonpresent)),
                             nrow = nrow(presence_prob),
                             ncol = ncol(presence_prob), byrow = TRUE) - newdata %*% t(nonpresent)

  priors <- object$priors
  if (type == "rawprob") {
    return(presence_prob + nonpresence_prob)
  }

  probs <- exp((presence_prob + nonpresence_prob))
  for(i in 1:length(priors)){
    probs[,i] <- probs[,i]*priors[i]
  }

  denom <- rowSums(probs)
  denom[denom==0] <- 1
  probs <- probs / denom

  if (type == "class") {
    class <- as.factor(object$levels[max.col(probs, ties.method = "first")])
    levels(class) <- object$levels
    return(class)
  }
  return(probs)
}
