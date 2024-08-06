#' Run the fusion-penalized logistic regression model
#'
#' @param x is the covariates matrix of size sum(n)xK with element is 1 if observation belongs to the basket, 0 if else
#' @param y is the vector of observations, is 1 if individual responds to trt, 0 if not
#' @param lambda the penalty parameter controlling strength of borrowing
#' @param gamma controls the weigths of L1 and L2 norms
#' @param pi_true a vector of numeric values corresponding to the real values of the response probability in each basket,
#' each value should be between 0 and 1 and the length of the vector should be the same than the number of columns in x
#'
#' @return estimated values for the K response probabilities (natural scale) and
#'the squared difference between the estimates and the true value when a true value was specified
#' @export
#'
#' @examples
#' data_simu <- emulate_trial(A=3, K=6, n=c(12,12,10,8,5,11))
#' y <- data_simu$y
#' x <- data_simu$x
#' runFPmodel(x, y, lambda=0.008, gamma=1)
#'
runFPmodel<-function(x, y,  lambda = 0.004, gamma = 1, pi_true=NULL){
  K<-dim(x)[2]
  if(length(pi_true)!=K & length(pi_true)!=0 ){
    stop(paste0("pi_true should be of length ", K, " or NULL"))
  }

  B<-matrixB(K)
  x.b<-x %*% MASS::ginv(B)

  glmnet.fit <- glmnet::glmnet(x.b, y, family="binomial", alpha=gamma,lambda=lambda,weights=rep(1/K, length(y)), intercept=F, standardize=F,penalty.factor = c(0,rep(1,(dim(B)[1]-1))))
  theta_hat <- drop(MASS::ginv(B)%*%drop(coef(glmnet.fit)[-1]))
  pi_hat <- logit_inv(theta_hat)

  if(!is.null(pi_true)){
    sq_diff<-(pi_hat-pi_true)^2
    create_named_list(pi_hat, sq_diff)
  }else{return(pi_hat)}

}

