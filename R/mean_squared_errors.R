computeMSE<-function(p_0 = 0.15, p_A = 0.45, A = 3, K = 5, n = 12, lambda_min = 0, lambda_max = 0.2, lambda_step = 0.001, gamma = 1, pi_critic = 0.35, nb_replicate = 10000, parallel.option = TRUE ){
  lambda_grid <- seq(lambda_min, lambda_max, lambda_step)

  if(parallel.option){
    pbapply::pboptions(use_lb=TRUE)
    nb_cores<-min(parallel::detectCores()-2,length(lambda_grid)  )
    cl<-parallel::makeCluster(nb_cores)
    MSE_grid <- pbapply::pbsapply(1:length(lambda_grid),  function(j) mean(Reduce(rbind, lapply(1:nb_replicate,
                                                                function(l) single_stage(p_0 = p_0, p_A = p_A, A = A, K = K, n = n,
                                                                lambda = lambda_grid[j], gamma = gamma,
                                                                pi_critic = pi_critic)$sq_diff))), cl=cl)
  }
}
