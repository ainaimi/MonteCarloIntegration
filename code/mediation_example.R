pacman::p_load(
  rio,          
  here,         
  skimr,        
  tidyverse,     
  lmtest,
  sandwich,
  broom,
  parallel
)

# CREATE EXPIT AND LOGIT FUNCTIONS
expit <- function(x){ exp(x)/(1+exp(x)) }
logit <- function(x){ log(x/(1-x)) }

## FUNCTION
cluster_sim <- function(nsim, sample_size, exposure_value, referent_value, mediator_value){
  
  set.seed(nsim)
  n = sample_size
  
  U <- rnorm(n)
  
  C <- rnorm(n)
  
  A1 <- exposure_value
  A0 <- referent_value
  
  L1 <- rnorm(n, mean = 0 + 1.5*A1 + 1.5*U)
  L0 <- rnorm(n, mean = 0 + 1.5*A0 + 1.5*U)
  
  M <- mediator_value
  
  Q_a1m <- mean(expit(-5 + log(2)*A1 + log(1.5)*M + log(1.75)*C + log(3)*L1 + log(2)*U))
  Q_a0m <- mean(expit(-5 + log(2)*A0 + log(1.5)*M + log(1.75)*C + log(3)*L0 + log(2)*U))
  
  controlled_direct_effect <- Q_a1m - Q_a0m
  
  ## output the marginally adjusted odds
  return(controlled_direct_effect)
  
}

results <- NULL
for (i in c(5,6,7)){
  number_sims = 10
  sample_size = as.numeric(paste0("5e",i))
  
  res <- mclapply(1:number_sims, 
                  function(x) cluster_sim(nsim = x, 
                                          sample_size = sample_size, 
                                          exposure_value = 1, 
                                          referent_value = 0, 
                                          mediator_value = 0),
                  mc.cores = number_sims
  )
  
  results <- cbind(results, 
                   do.call(rbind, res))
}

results

cbind(results[,1] - results[,3],
      results[,2] - results[,3],
      results[,3] - results[,3])