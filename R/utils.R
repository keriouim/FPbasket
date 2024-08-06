
#' Antidiagonal matrix
#'
#' @param k The size of the matrix (kxk)
#'
#' @return A matrix of size kxk with 1 on the antidiagonal and 0 elsewhere
#' @export

#' @examples
#' M<-antidiag(10)
#' print(M)
#'
antidiag<-function(k){
   return(diag(k)[k:1, ])
}


#' Creating the transformation matrix of the pairwise differences of the parameters
#'
#' @param K K is the number of baskets considered
#'
#' @return The transformation matrix of size (K*(K-1)/2+1) x K
#' @export
#'
#' @examples
#' B<-matrixB(5)
#' print(B)
#'
matrixB<-function(K){
  B<-rbind(c(1,rep(0,K-1)), Reduce(rbind,lapply(1:(K-1), function(j) cbind(-antidiag(j), rep(1,j),matrix(0, nrow=(j),ncol=(K-j-1) )))))
  return(B)
}

#' Inverse logit function
#'
#' @param x a numeric value
#'
#' @return the inverse logit transformation of this number
#' @export
#'
#' @examples
#' logit_inv(1)
#'
logit_inv<-function(x){return(exp(x)/(exp(x)+1))}


create_named_list <- function(...) {
  setNames(list(...), as.character(match.call()[-1]))
}


#' stop current execution if element is not between 0 and 1
#'
#' @param x a numeric value
#'
#' @return an error message if x is not between 0 and 1
#' @export
#'
#' @examples
#' stop_proba(0.3)
#'
stop_proba<-function(x){
  if(x>1 | x<0){stop(paste0(x," should be between 0 and 1"))}
}


