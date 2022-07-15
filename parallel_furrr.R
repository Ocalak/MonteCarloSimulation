install.packages("furrr")
library(furrr)



#First I want to see the numbers of cores
library(parllel)
detectCores()
#> 8
#So I want to use minues 1 core of the total cores on my mac 

no_cores <- detectCores() -1 #number of cores that I want to use

#Parallel with furrr
#One approach could be using future_map function

plan(multisession,workers= no_cores)# 
do_nothing <- future_map(c("Hello ","World"), ~.x)#Runs in parallel


#Proof of running in parallel
install.packages("tictoc")
library(tictoc)

plan(sequential)
tic()
do_nothing <- future_map(c(2,2,2), ~Sys.sleep(.x))
toc()
#> result is 6.08 sec

plan(multisession,workers=3)
tic()
do_nothing <- future_map(c(2,2,2), ~Sys.sleep(.x))
toc()
#> result is 2.59 sec
#Look at future_pmap() also.



