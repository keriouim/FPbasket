#' Emulate a single-stage trial
#'
#' @param p_0 the null probability of response, default is 0.15
#' @param p_A the active probability of response, default is 0.45
#' @param A the number of active baskets
#' @param K the total number of baskets
#' @param n the sample size in each basket (give either a vector of length K or a single numeric value)
#'
#' @return a vector of length sum(n) or n*K with all observations 0 or 1
#' @export
#'
#' @examples
#' emulate_trial(A=3, K=6, n=c(12,12,10,8,5,11))
#'
emulate_trial<-function(p_0 = 0.15, p_A = 0.45, A, K = 5, n = 12){
  prob_true<-c(rep(p_0, (K-A)), rep(p_A, A))
  if(length(n)!=1 & length(n)!= K){print("Warning: n should be length K or 1")}
  if(length(n)==1){n_obs<-rep(n,K)}else{n_obs<-n}
  y <- Reduce(c, sapply(1:K, function(k) rbinom(n_obs[k], 1, prob = prob_true[k])))
  return(y)
}


