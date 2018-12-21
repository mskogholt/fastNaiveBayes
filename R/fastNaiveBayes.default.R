#' @export
#' @import Matrix
#' @rdname fastNaiveBayes
fastNaiveBayes.default <- function(x, y, laplace = 0, sparse = FALSE, ...){
  if(nrow(x)!=length(y)){
    stop('X and Y must be equal length')
  }

  if(class(x)[1]!='dgCMatrix'){
    if(!is.matrix(x)){
      x <- as.matrix(x)
    }
    if(sparse){
      x <- Matrix(x, sparse = TRUE)
    }
  }else{
    sparse <- TRUE
  }

  levels <- levels(y)
  names(levels) <- levels
  result <- lapply(levels, function(level){
    x_lev <- x[y==level,]

    if(sparse){
      present <- Matrix::colSums(x_lev, na.rm = TRUE)
    }else{
      present <- base::colSums(x_lev, na.rm = TRUE)
    }
    total <- rep(nrow(x_lev),times = ncol(x_lev))

    non_present <- total-present

    non_present <- non_present + laplace
    present <- present + laplace

    total <- present+non_present
    return(list(present=present/total,non_present=non_present/total,total=total, level_prob = nrow(x_lev)/nrow(x)))
  })

  structure(list(probability_table = result,
                 names = colnames(x)
  ),
  class = 'fastNaiveBayes')
}
