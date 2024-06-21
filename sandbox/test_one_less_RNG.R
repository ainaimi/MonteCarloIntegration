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

thm <- theme_classic() +
  theme(
    legend.position = "top",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)

# a test for the monte carlo integration paper


set.seed(123)
expit <- function(x){1 / (1+exp(-x))}

sim_func <- function(){
  c <- rnorm(n=5000)
  pi_x <- expit(-2 + log(2)*c)
  x <- rbinom(n=5000, size = 1, prob = pi_x)
  res1 <- mean(pi_x)
  res2 <- mean(x)
  
  return(c(res1,res2))
}

results <- do.call(rbind, 
                   mclapply(1:5e6, function(x) sim_func(), 
                            mc.cores = detectCores() - 2)
                   )

## it makes a diff:
the_sd <- apply(results, 2, sd)
the_sd[2]/the_sd[1]