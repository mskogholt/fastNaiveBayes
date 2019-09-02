#' @title Update function
#' @param object fitted model
#' @param x numeric matrix
#' @param y factor of classes
#' @param sparse boolean, set to true to use sparse matrices
#' @param check boolean, set to true to enable formal checks on input
#' @param ... not used
#'
#' @export
#' @import Matrix
#' @rdname updateFNB
fnb.update <- function(object, x, y, sparse = FALSE, check = TRUE, ...){
  UseMethod("fnb.update")
}

#' @export
#' @import Matrix
#' @rdname updateFNB
fnb.update.default  <- function(object, x, y, sparse = FALSE, check = TRUE, ...){
  print('Hello')
}

#' @export
#' @import Matrix
#' @rdname updateFNB
fnb.update.fnb.bernoulli <- function(object, x, y, sparse = FALSE, check = TRUE, ...){
  if(check){
    args <- fnb.check.args.model(x, y, priors=NULL, laplace=0, sparse)
    x <- args$x
    y <- args$y
    sparse <- args$sparse
  }

  # TODO: If different features or levels are encountered handle it!
  oldpresent <- object$present
  newpresent <- fnb.bernoulli.calculate(x, y, sparse)

  present <- oldpresent + newpresent

  oldn <- object$n
  oldobs <- object$obs

  newn <- tabulate(y)
  newobs <- nrow(x)

  n <- oldn+newn
  obs <- oldobs+newobs

  # TODO: Merge names from x
  # TODO: Merge levels from y
  structure(list(
    present = present,
    laplace = object$laplace,
    n = n,
    obs = obs,
    priors = object$priors,
    names = colnames(x),
    levels = levels(y)),

    class = "fnb.bernoulli"
  )
}
