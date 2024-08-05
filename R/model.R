#' Run the fusion-penalized logistic regression model
#'
#' @param x is the covariates matrix of size sum(n)xK with element is 1 if observation belongs to the basket, 0 if else
#' @param y is the vector of observations, is 1 if individual responds to trt, 0 if not
#' @param lambda the penalty parameter controlling strength of borrowing
#' @param gamma controls the weigths of L1 and L2 norms
#'
#' @return estimated values for the K response probabilities (natural scale)
#' @export
#'
#' @examples
#' data_simu <- emulate_trial(A=3, K=6, n=c(12,12,10,8,5,11))
#' y <- data_simu$y
#' x <- data_simu$x
#' runFPmodel(x, y, lambda=0.008, gamma=1)
#'
runFPmodel<-function(x,y, lambda, gamma){
  K<-dim(x)[2]
  B<-matrixB(K)
  x.b<-x %*% ginv(B)

  glmnet.fit <- glmnet(x.b, y, family="binomial", alpha=gamma,lambda=lambda,weights=rep(1/K, length(y)), intercept=F, standardize=F,penalty.factor = c(0,rep(1,(dim(B)[1]-1))))
  theta_hat <- drop(ginv(B)%*%drop(coef(glmnet.fit)[-1]))
  pi_hat <- logit_inv(theta_hat)
  return(pi_hat)
}

