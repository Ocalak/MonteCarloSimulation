
install.packages("tidymodels")
library(tidymodels)
#First I want to create a function that returns a simulated dataset.

#User defined function
#Example t_test
t_test_user_def <- function(input.data, mu0){#one sample t test
  n    <- length(input.data)
  xbar <- mean(input.data)
  s    <- sd(input.data)
  t    <- (xbar - mu0)/(s / sqrt(n))
  if( t < 0 ){
    p.value <- 2 * pt(t, df=n-1)
  }else{
    p.value <- 2 * (1-pt(t, df=n-1))
  }
  a <- paste( t, p.value)
 return(expand.grid(a))#parameter grid 
}
x <- seq(1,30,1.5)
t_test_user_def(x,2)

library(dials)
install.packages("dials")
library(dials)
install.packages("rlang")
library(rlang)


