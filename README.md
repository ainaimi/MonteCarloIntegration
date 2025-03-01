# Monte Carlo Integration to Compute True Parameter Values in Simulations

Simulation studies are often used to evaluate and compare the properties of 
statistical methods in controlled experimental settings. In most cases, performing 
a simulation study requires knowledge of the true value of the parameter, or estimand,
of interest. However, in many simulation designs, the true value of the estimand 
is difficult to compute analytically. 

This repository contains code associated with an article illustrating Monte 
Carlo Integration to compute true estimand values in simulation studies. 

We include three programs that users can explore:

  - A program to implement Monte Carlo integration to compute the marginally adjusted odds ratio
  - A program to implement Monte Carlo integration to compute the controlled direct effect in the presence of exposure-induced mediator outcome confounding
  - A program demonstrating how numerical integration can be used in trivially simple settings
  
These programs were written and run using parallel processing on MacOS. Windows 
users may elect to remove the parallel processing code, or to convert it to code more 
