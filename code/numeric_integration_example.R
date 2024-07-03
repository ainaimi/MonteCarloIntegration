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

# numeric integration of a simple logistic regression model
## use the `integrate` function in r
integrand <- function(
  c, trt = 1, beta0 = 1, beta1 = 1, beta2 = 0.5
){
  plogis(beta0 + beta1 * trt + beta2 * c)*dnorm(c, mean = 0, sd = 1) # assume C ~ N(0,1)
}

EY1_integrate <- integrate(integrand, trt = 1, lower = -Inf, upper = Inf)
EY0_integrate <- integrate(integrand, trt = 0, lower = -Inf, upper = Inf)

# true marginally adjusted OR
Odds1 <- EY1_integrate$value/(1-EY1_integrate$value)
Odds0 <- EY0_integrate$value/(1-EY0_integrate$value)

Odds1/Odds0