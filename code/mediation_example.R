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
  # `nsim` is an index that can be used to track which Monte Carlo simulation is 
  # being run. Because this integration approach uses a single large Monte Carlo
  # sample, it can be set to 1, or some value that can be used as a seed.
  # 
  # `sample_size` is the number of simulated observations that will be used to 
  # integrate. the larger this value is, the more stable the Monte Carlo integral 
  # calculation for the true parameter will be.
  # 
  # `exposure_value`, `referent_value`, and `mediator_value` are the values of the 
  # exposure and mediator that one wants to use to compute the CDE.
  
  
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

## this for loop builds up three sets of results: 
## ## one set with a monte carlo sample of 5e5 = 500,000
## ## one set with a monte carlo sample of 5e6 = 5,000,000
## ## one set with a monte carlo sample of 5e7 = 50,000,000
## 
## additionally, the loop runs this code via parallel processing across ten 
## cores. this allows us to compare how much the results change for each 
## effective sample size over ten runs. 
## 
## note: this loop may not work on Windows systems. Interested users can convert
## to the parLapply function.

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

results <- data.frame(results)

names(results) <- c("MC_5e5", "MC_5e6", "MC_5e7")

## here, the MC_5eX column presents the marginally adjusted odds ratio for the 
## sample size corresponding to 5eX across all ten cores. 
## 
## as with the collapsibility results, we observe variability in the fourth 
## decimal place for the smallest sample size. 
## increasing the sample size to fifty million yields no variability in the fourth decimal place

results