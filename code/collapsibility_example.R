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
cluster_sim <- function(nsim, sample_size, effect, exposure_value, referent_value){

  set.seed(nsim)
  n = sample_size
  
  C <- rnorm(n,0,1)
  
  theta <- c(0,log(2))
  pi <- expit(theta[1]+theta[1]*C)
  
  A1 <- exposure_value # set the exposure to a specific value
  A0 <- referent_value # set the exposure to referent value
  
  beta<-c(-2.75, effect, log(2))
  mu1 <- mean(expit(beta[1] + beta[2]*A1 + beta[3]*C))
  mu0 <- mean(expit(beta[1] + beta[2]*A0 + beta[3]*C))
  
  ## compute the marginal odds ratio from these mus
  odds_ratio <- (mu1/(1-mu1))/(mu0/(1-mu0))
  
  ## output the marginally adjusted odds
  return(odds_ratio)

}

results <- NULL
for (i in c(5,6,7)){
  number_sims = 10
  sample_size = as.numeric(paste0("5e",i))
  
  res <- mclapply(1:number_sims, 
                   function(x) cluster_sim(nsim = x, 
                                           sample_size = sample_size, 
                                           effect = log(2),
                                           exposure_value = 1,
                                           referent_value = 0),
                   mc.cores = number_sims
  )
  
  results <- cbind(results, 
                   do.call(rbind, res))
}

results

cbind(results[,1] - results[,3],
      results[,2] - results[,3],
      results[,3] - results[,3])