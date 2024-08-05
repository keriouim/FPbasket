#' Simulate, analyze and conclude on a simulated single-stage trial
#'
#' @param p_0 the null probability of response, default is 0.15
#' @param p_A the active probability of response, default is 0.45
#' @param A the number of active baskets
#' @param K the total number of baskets
#' @param n the sample size in each basket (give either a vector of length K or a single numeric value)
#' @param lambda the penalty parameter controlling strength of borrowing
#' @param gamma controls the weigths of L1 and L2 norms
#' @param pi_critic the critical value of the final decision
#'
#' @return outcomes is a vector of length K and each element is 1 if basket is declared active, 0 if else
#' @export
#'
#' @examples
#'
single_stage<-function(p_0 = 0.15, p_A = 0.45, A, K = 5, n = 12, lambda = 0.004, gamma = 1, pi_critic = 0.34 ){
  data_simu <- emulate_trial(p_0 = p_0, p_A = p_A, A = A, K = K, n = n)
  estimated <- runFPmodel(data_simu$x, data_simu$y, lambda = lambda, gamma = gamma, pi_true = data_simu$prob_true)
  estimates <- estimated$pi_hat
  outcomes <- estimated$pi_hat>pi_critic

  create_named_list(estimates,  outcomes)
}
