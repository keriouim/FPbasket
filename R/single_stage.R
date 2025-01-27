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
#'single_stage(A=3)
#'
single_stage<-function(p_0 = 0.15, p_A = 0.45, A, K = 5, n = 12, lambda = 0.004, gamma = 1, pi_critic = 0.34 ){
  data_simu <- emulate_trial(p_0 = p_0, p_A = p_A, A = A, K = K, n = n)
  estimated <- runFPmodel(data_simu$x, data_simu$y, lambda = lambda, gamma = gamma, pi_true = data_simu$prob_true)
  estimates <- estimated$pi_hat
  sq_diff <- estimated$sq_diff
  outcomes <- estimated$pi_hat>pi_critic

  create_named_list(estimates, sq_diff, outcomes)
}

#' Select the optimal critical value for the final analysis by targeting a specific FWER when no basket is truly active
#'
#' @param p_0 the null probability of response, default is 0.15
#' @param p_A the active probability of response, default is 0.45
#' @param K the total number of baskets
#' @param n the sample size in each basket (give either a vector of length K or a single numeric value)
#' @param lambda the penalty parameter controlling strength of borrowing
#' @param gamma controls the weigths of L1 and L2 norms
#' @param FWER_target the Family Wise Error Rate targeted
#' @param pi_critic_min the minimum value considered in the grid of optimization for the critical value
#' @param pi_critic_max the maximum value considered in the grid of optimization for the critical value
#' @param pi_critic_step the steps of the grid for the critical values
#' @param nb_replicate the number of simulated trials
#' @param parallel.option boolean, TRUE if simulations should be ran in parallel
#' @param nb_cores integer, number of cores for parallelization
#'
#' @return two numerical values; the first one pi_critic is the optimal critical values among the ones considered
#' the second value is the FWER obtained with this given critical value
#' @export
#'
#' @examples
#'calibrate_finalDecision(pi_critic_step = 0.1, pi_critic_min=0.3, nb_replicate = 100)
#'
calibrate_finalDecision<-function(p_0 = 0.15, p_A = 0.45, K = 5, n = 12, lambda = 0.004, gamma = 1, FWER_target = 0.1, pi_critic_min = 0.1, pi_critic_max = 0.5, pi_critic_step = 0.05, nb_replicate = 10000, parallel.option = TRUE, nb_cores = 2 ){
  stop_proba(FWER_target)#checking that the target FWER is between 0 and 1

  pi_critic_grid <- seq(pi_critic_min, pi_critic_max, pi_critic_step)

  if(parallel.option){
    pbapply::pboptions(use_lb=TRUE)
    cl<-parallel::makeCluster(nb_cores)
    FWER_grid <- pbapply::pbsapply(1:length(pi_critic_grid),cl=cl,
                                              function (j) mean(sapply(1:nb_replicate,
                                              function(l) sum(single_stage(p_0 = p_0, p_A = p_A, A = 0, K = K, n = n,
                                                                           lambda = lambda, gamma = gamma,
                                                                           pi_critic = pi_critic_grid[j])$outcomes)>=1 )))
  }else{
    FWER_grid <- pbapply::pbsapply(1:length(pi_critic_grid),
                                   function (j) mean(sapply(1:nb_replicate,
                                                            function(l) sum(single_stage(p_0 = p_0, p_A = p_A, A = 0, K = K, n = n,
                                                                                         lambda = lambda, gamma = gamma,
                                                                                         pi_critic = pi_critic_grid[j])$outcomes)>=1 )))
  }
  pi_critic <- pi_critic_grid[which.min(abs(FWER_target-FWER_grid))]
  FWER <- FWER_grid[which.min(abs(FWER_target-FWER_grid))]
  create_named_list(pi_critic, FWER)
}





#'  Compute average MSE under a given scenario of activity
#' for a grid of values for lambda and select the lambda that minimizes the average MSE
#'
#' @param p_0 the null probability of response, default is 0.15
#' @param p_A the active probability of response, default is 0.45
#' @param A the number of active baskets
#' @param K the total number of baskets
#' @param n the sample size in each basket (give either a vector of length K or a single numeric value)
#' @param lambda_min minimum value considered for the grid of values of lambda
#' @param lambda_max maximum value considered for the grid of values of lambda
#' @param lambda_step the steps of the grid for of values for lambda
#' @param gamma controls the weigths of L1 and L2 norms
#' @param pi_critic the critical value of the final decision
#' @param nb_replicate the number of simulated trials
#'
#' @return the average MSE for each value of lambda and the lambda minimizing the average MSE
#' @export
#'
#' @examples
#' computeMSE(nb_replicate = 100)
#'
computeMSE<-function(p_0 = 0.15, p_A = 0.45, A = 3, K = 5, n = 12, lambda_min = 0, lambda_max = 0.01, lambda_step = 0.001, gamma = 1, pi_critic = 0.35, nb_replicate = 10000){
  lambda_grid <- seq(lambda_min, lambda_max, lambda_step)
  MSE_grid <- pbapply::pbsapply(1:length(lambda_grid),  function(j) mean(Reduce(rbind, lapply(1:nb_replicate,
                               function(l) single_stage(p_0 = p_0, p_A = p_A, A = A, K = K, n = n,
                                                       lambda = lambda_grid[j], gamma = gamma,
                                                        pi_critic = pi_critic)$sq_diff))))
  lambda <- lambda_grid[which.min(MSE_grid)]
  MSE<-cbind(lambda_grid, MSE_grid)
  colnames(MSE)<- c("lambda", "MSE")

  create_named_list(lambda, MSE)
}







#' Give the probability of declaring drug activity in each basket under all scenarios of activity
#'

#' @param p_0 the null probability of response, default is 0.15
#' @param p_A the active probability of response, default is 0.45
#' @param K the total number of baskets
#' @param n the sample size in each basket (give either a vector of length K or a single numeric value)
#' @param lambda the penalty parameter
#' @param gamma controls the weigths of L1 and L2 norms
#' @param pi_critic the critical value of the final decision
#' @param nb_replicate the number of simulated trials
#'
#' @return a table of dimension (K+1)*K with the probability of rejecting the null
#'hypothesis in each basket under each scenario of activity A=0,...,K
#'and a vector of the Family Wise Error Rate for each scenario of activity
#'
#' @export
#'
#' @examples
#'summaryAllScenarios(nb_replicate =1000, K=3, n=20)

summaryAllScenarios<-function(p_0 = 0.15, p_A = 0.45, K = 5, n = 12, lambda = 0.004, gamma = 1, pi_critic = 0.34, nb_replicate = 10000){

  allScen <-  pbapply::pblapply(0:K, function(a) matrix(Reduce(rbind,
         lapply(1:nb_replicate,
                function(l) as.numeric(single_stage(p_0 = p_0, p_A = p_A, A = a,
                                                    K = K, n = n, lambda = lambda,
                                                    gamma = gamma, pi_critic = pi_critic )$outcomes))),ncol=K))

  tab_prob<-Reduce(rbind, lapply(0:K+1, function(a) colMeans(allScen[[a]])))
  colnames(tab_prob)<-c(paste0("Basket ", 1:K))
  rownames(tab_prob)<-c(paste0("A = ", 0:K))

  FWER=c(sapply( 0:(K-2), function(a) mean(rowSums(allScen[[a+1]][,(1:(K-a))])>=1)), mean(allScen[[K-1]][,1]), NA)
  tab_FWER<-cbind(A=0:K, FWER)

  create_named_list(tab_prob, tab_FWER)
}


