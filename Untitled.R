library(parallel)

#learning lapply 
lapply(1:3, function(x) c(x,x^2,x^3))
lapply(1:3/3,round, digits=3)
#lapply take parameter(vector/list), feed the function and returns as a list

nu_cores <- detectCores() -1 

cl_1 <- makeCluster(nu_cores)
cl_1


#Parallel version of lapply
parLapply(cl_1,2:4,function(exponent) 2^exponent)
lapply(2:4,function(exponent) 2^exponent)
library(bench)
bench::mark(parLapply(cl_1,2:4,function(exponent) 2^exponent),
            lapply(2:4,function(exponent) 2^exponent))
stopCluster(cl_1)
