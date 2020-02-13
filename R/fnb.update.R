#' @title Update function
#' @param object fitted model
#' @param x numeric matrix
#' @param y factor of classes
#' @param sparse boolean, set to true to use sparse matrices
#' @param check boolean, set to true to enable formal checks on input
#'
#' @export
#' @import Matrix
#' @rdname updateFNB
fnb.update <- function(object, x, y, sparse = FALSE, check = TRUE){
  UseMethod("fnb.update")
}

#' @export
#' @import Matrix
#' @rdname updateFNB
fnb.update.default  <- function(object, x, y, sparse = FALSE, check = TRUE){
  print('Not implemented for this class yet!')
}

#' @export
#' @import Matrix
#' @rdname updateFNB
fnb.update.fnb.bernoulli <- function(object, x, y, sparse = FALSE, check = TRUE){
  if(check){
    args <- fnb.check.args.model(x, y, priors=NULL, sparse)
    x <- args$x
    y <- args$y
    sparse <- args$sparse
  }

  # Determine present
  oldpresent <- object$present
  newpresent <- fnb.utils.rowsum(x, y, sparse)

  complete_names <- unique(c(colnames(oldpresent), colnames(newpresent)))

  # rownames(oldpresent) <- object$levels
  # rownames(newpresent) <- levels(y)

  oldpresent <- fnb.utils.pad_with_zeros(oldpresent, sparse, complete_names)
  newpresent <- fnb.utils.pad_with_zeros(newpresent, sparse, complete_names)

  present <- rbind(oldpresent, newpresent)
  present <- fnb.utils.rowsum(present, as.factor(rownames(present)), sparse)

  # Determine obs
  oldobs <- object$obs
  newobs <- nrow(x)
  obs <- oldobs+newobs

  # Determine n (i.e. obs count per level)
  oldn <- object$n
  newn <- tabulate(y, nbins = nlevels(y))

  n <- rowsum(c(oldn, newn), c(object$levels, levels(y)))[,1]

  structure(list(
    present = present,
    laplace = object$laplace,
    n = n,
    obs = obs,
    priors = object$priors,
    names = colnames(present),
    levels = names(n)),

    class = "fnb.bernoulli"
  )
}
