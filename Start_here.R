require(utils)
library(tidyverse)
library(purrr)
library(utils)
library(furrr)
library(future)
library(parallel)


# Part 1:  creating grid --------------------------------------------------
'First step, built a function that creates a parameter grid with all permutations
of the given parameters'

create_grid <- function(parameters, nrep){
  input <- parameters
  storage <- list()
  name_vec <- c()
  
  for(i in 1:length(input)){ #1:3
    a <- as.numeric(input[[i]][[2]])
    b <- as.numeric(input[[i]][[3]])
    c <- as.numeric(input[[i]][[4]])
    output <- seq(from=a, to=b, by=c)
    storage[[i]] <-  output
    name_vec[i] <- input[[i]][[1]]
  }
  
  grid <- expand_grid(unlist(storage[1])
                      , unlist(storage[2])
                      , unlist(storage[3])
                      , unlist(storage[4])
                      , unlist(storage[5])
                      , c(1:nrep))
  
  names(grid) <- c(name_vec, "rep")
  
  return(grid)
  #return(list(storage, grid))
}




# TESTING  create_grid() --------------------------------------------------

#One parameter (works)
param_list1 <- list(c("n", 10, 20, 10))
create_grid(param_list1, nrep=1)
create_grid(param_list1, nrep=1)

#two parameter (works)
param_list2 <- list(c("n", 10, 20, 100)
                    ,c("mu", 0, 1, 0.25))
ctt <- reate_grid(param_list2, nrep=2)

#three parameters (works)
param_list3 <- list(c("n", 10, 20, 10)
                    ,c("mu", 0, 1, 0.25)
                    ,c("sd", 0, 0.3, 0.1))
ak <- create_grid(param_list3, nrep=10)




#four parameters (works)
param_list4 <- list(c("n", 10, 20, 10)
                    ,c("mu", 0, 1, 0.25)
                    ,c("sd", 0, 0.3, 0.1)
                    ,c("gender", 0, 1,6))

create_grid(param_list4, nrep=5)

grid_4 <- create_grid(param_list4, nrep=50)
print(grid_4, n=nrow(grid_4))

'Grid works well.'




# Part 2: Data generation over grid ---------------------------------------


'I decided to implement the data generation and summarisation seperatly, which
allows the user more flexibility, f.e. quickly changing the DGB while keeping the
summary statistics.

simulation = data generation function, f.e rnorm
parameters = list of parameters, f.e. param_list3 

Takes parameter list and some data generating function as a input and returns the
simulated data.'

data_generation <- function(simulation, grid){ #this is for use inside the function
  
  if(ncol(grid)==2){
    var1 <- c(unlist(grid[,1]))
    data <- future_map(var1, simulation,.options = furrr_options(seed = TRUE))
    
  }
  
  if(ncol(grid)==3){
    var1 <- c(unlist(grid[,1]))
    var2 <- c(unlist(grid[,2]))
    data <- future_map2(var1, var2, simulation,.options = furrr_options(seed = TRUE))
  } 
  
  if(ncol(grid)==4){ #need to implement more than 3?!
    var1 <- c(unlist(grid[,1]))
    var2 <- c(unlist(grid[,2]))
    var3 <- c(unlist(grid[,3]))
    list1 <- list(var1,var2,var3)
    data <- future_pmap(list1, .f=simulation,.options = furrr_options(seed = TRUE))
  } 
  
  return(data)
}





# TESTING data_generation() -------------------------------------------------

#for one parameter (works)
grid1 <- create_grid(param_list1, nrep=3)
zt <- data_generation(simulation=rnorm, grid=grid1)

#for two parameter (works)
grid2 <- create_grid(param_list2, nrep=3)
print(grid2, n=nrow(grid2))
data_generation(simulation=rnorm, grid=grid2)

#for different parameter (doesnt work)
param_list2x <- list(c("n", 10, 20, 10)
                     ,c("sd", 0, 100, 10)) #sd instead of mean


grid2x <- create_grid(param_list2x, nrep=3)
print(grid2x, n=nrow(grid2x))
data_generation(simulation=rnorm, grid=grid2x)

'this doesnt work. Our function would use the sd input as mean instead, since 
its at the second position. Thus underlines its important to use the variables in the 
right order and also include variables, that are held constant.

The MonteCarlo() function outputs "argument "loc" is missing, with no default'

#for different data generation function (runif) WORKS WELL!
param_list_runif <- list(c("n", 10, 30, 10)
                         , c("min", 0, 0, 0)
                         ,c("max", 1, 1, 0))


grid_unif <- create_grid(param_list_runif, nrep=3)
data_generation(simulation=runif, grid=grid_unif)

#for different data generation function (rpois) WORKS WELL!
param_list_rpois <- list(c("n", 10, 30, 10)
                         , c("lambda", 0, 10, 1))
print(grid_pois, n=nrow(grid_pois))
grid_pois <- create_grid(param_list_rpois, nrep=3)
data_generation(simulation=rpois, grid=grid_pois)



# Part 3: Summary statistics ----------------------------------------------

'Next we implement the summary function, that analyses the given data point.
sum_fun = the summary function we want to use on the data
data_input = the data points we want to analyse'

#summary function for one input
summary_function <- function(sum_fun, data_input){
  
  count <- length(data_input)
  summary_matrix <- matrix(nrow=count, ncol=1)
  
  for(i in 1:count){
    input <- list(data_input[[i]])
    output <- sapply(sum_fun, do.call, input)
    summary_matrix[i] <- output
  }
  #output <- as.data.frame(summary_matrix)
  #names(output) <- sum_fun
  colnames(summary_matrix) <- sum_fun
  return(summary_matrix)
}



# TESTING summary_function() ---------------------------------------------------

grid_test <- create_grid(param_list3, nrep=3)
test_data <- data_generation(simulation=rnorm, grid=grid_test)
summary_function(sum_fun=list("mean"), data_input=test_data)

'Works like a charm. Next, we want to combine our results with the parameter grid and create
arrays, that structure our simulation input with the given results.'



# Part 5: Create arrays ---------------------------------------------------

'Next, we want to store the summary statistics in an array, where you can easily read
the given parameter constellation. We have to reorder the output, cause arrays gets filled
column wise.'


create_array_function <- function(comb, parameters, nrep){
  storage <- list()
  name_vec <- c()
  
  for(i in 1:length(parameters)){ #this creates the sequences of parameters
    a <- as.numeric(parameters[[i]][[2]])
    b <- as.numeric(parameters[[i]][[3]])
    c <- as.numeric(parameters[[i]][[4]])
    output <- seq(from=a, to=b, by=c)
    storage[[i]] <-  output
    name_vec[i] <- parameters[[i]][[1]] #this just stores the names of the variables
  }
  
  
  matrix.numeration <-  paste("rep","=", 1:nrep, sep = "")
  
  if(length(parameters)==1){
    comb_ordered <-  comb %>% arrange(comb[,2])
    seq1 <- c(unlist(storage[1]))
    
    row.names <- paste(name_vec[1],"=",seq1, sep = "")
    
    dimension_array <- c(length(seq1), nrep)
    dim_names_list <- list(row.names, matrix.numeration)
  }
  
  if(length(parameters)==2){
    comb_ordered <-  comb %>% arrange(comb[,2])  %>% arrange(comb[,3])
    seq1 <- c(unlist(storage[1]))
    seq2 <- c(unlist(storage[2]))
    
    row.names <- paste(name_vec[1],"=",seq1, sep = "")
    column.names <-  paste(name_vec[2],"=",seq2, sep = "")
    
    dimension_array <- c(length(seq1), length(seq2), nrep)
    dim_names_list <- list(row.names, column.names, matrix.numeration)
  }
  
  if(length(parameters)==3){
    comb_ordered <-  comb %>% arrange(comb[,2])  %>% arrange(comb[,3]) %>% arrange(comb[,4]) 
    seq1 <- c(unlist(storage[1]))
    seq2 <- c(unlist(storage[2]))
    seq3 <- c(unlist(storage[3]))
    
    row.names <- paste(name_vec[1],"=",seq1, sep = "")
    column.names <-  paste(name_vec[2],"=",seq2, sep = "")
    matrix.names1 <-  paste(name_vec[3],"=",seq3, sep = "")
    
    dimension_array <- c(length(seq1), length(seq2), length(seq3), nrep)
    dim_names_list <- list(row.names, column.names, matrix.names1, matrix.numeration)
    
  }
  
  
  array1 <- array(comb_ordered[,ncol(comb)] #change to automatically adjust dim
                  , dim = dimension_array
                  , dim_names_list)
  return(array1)
}



# PREP TEST create_array_function() ----------------------------------------------

main_function_array_test <-  function(parameters #list of parameters
                                      , nrep #number of repetitions
                                      , simulation #data genereation
                                      , sum_fun){ #summary statistics
  
  grid <- create_grid(parameters, nrep) #Step 1: create grid
  
  raw_data <- data_generation(simulation, grid) #Step 2: simlate data
  
  summary <- summary_function(sum_fun, data_input=raw_data) #Step 3: Summary statistics
  
  comb <- cbind(grid, summary) #Step 4: Combine resuluts with parameters
  
  array_1 <- create_array_function(comb, parameters, nrep) #Step 5: Create array
  
  return(comb)
  
}






# TESTING create_array_function() ----------------------------------------------


param_list3x <- list(c("n", 10, 100, 10)
                     ,c("mu", 0, 10, 1)
                     ,c("sd", 0, 5, 1))

comb1 <- main_function_array_test(parameters=param_list3x
                                  , nrep = 5
                                  , simulation = rnorm
                                  , sum_fun="mean")

comb1 #this is how the df with all permutations and results looks like

'array function takes this data fram and turns it into a array with the right dimensions'
create_array_function(comb=comb1, parameters=param_list3x, nrep=3)








# Part 5.2: output_function ------------------------------------------------------
'Goal is to create a function, that takes the MC resuluts and all parameter input
and converts it onto output format that prints nicely into the console'

output_function


# Part 5.3: averaging over all repetitions --------------------------------

'Create grid without repetitions, then just add the simulation '


'nrep <- 5
fac <- nrow(comb1)/nrep

test_list <- list(0)
for(i in 0:fac){
  start <- 1 + i*nrep
  end <- nrep + i*nrep
  test_list[i+1] <- mean(comb1[start:end, ncol(comb1)])
}
'

average_function <- function(grid_for_avg, summary, nrep){
  grid_for_avg <- grid_for_avg[-ncol(grid_for_avg)] #remove column for reps
  n_rows <- nrow(grid_for_avg)
  n_col <- ncol(grid_for_avg)
  
  for(i in 1:n_rows){
    start <- 1 + (i-1)*nrep
    end <- i*nrep
    grid_for_avg[i, n_col+1] <- mean(summary[start:end, ])
  }
  
  grid_plus_mc <- data.frame(grid_for_avg)
  
  colnames(grid_plus_mc)[n_col+1] <- "avg"
  
  return(grid_plus_mc)
  
}
##Part 6
##############Tidy output function


####Option 1

output_function <- function(array_1,average_over_reps,parameters,cores,simulation,
                            nrep,cpt){
  
  out <- list()
  class(out) <- "Eco"
  out$results <- array_1
  out$average <- average_over_reps
  class(out$average) <- c("Eco",class(out$average))
  class(out$results) <- c("Eco",class(out$results))
  
  if(cores>1){
    parallel = "Multisession"
  } else {
    parallel = "Sequential"
  }
  text <-  cat("\n",
           "Repetition(nrep)      : ",nrep,"\n\n",
           "Parallelization Type  : ",parallel,"\n\n",
           "Number of Cores Used in  Parallelization : ",cores," out of",detectCores(),"\n\n",
           "Input Parameters : ",paste(parameters),"\n\n",
           "Simulation Length :",length(array_1),"\n",
           "Minumum :",min(array_1),"\n",
           "Maximum :",max(array_1),"\n",
           "Mean    :", mean(array_1),"\n",
           "Median  :",median(array_1),"\n\n",
          # "Quantiles :",names(quantile(out$results)),"\n",
           #"          ",round(quantile(out$results),digits=1),"\n",
           #"Simulation Function : " ,print(simulation),"\n\n",
           "Execution Time of Monte Carlo Simulation",as.numeric(cpt),"secs \n",
           "Name of The Class :",class(out))
  
  return(out)
}


# Part 7: main function part ------------------------------------------------------

main_function <-  function(parameters #list of parameters
                           , nrep #number of repetitions
                           , simulation #data genereation
                           , sum_fun #summary statistics
                           ,seed = NULL#Reproducibility
                           ,cores=NULL){
 
    
  #Number of cores
  max.cores <- detectCores()
  if(cores>max.cores){
    stop("Number of Cores cannot be bigger than total number of cores")
  }
  if(!is.null(seed)) {#Reproducibility
    set.seed(seed)}#If seed provided then set.seed takes the number
  else {
    warning("No seed provided!", call. = FALSE)
    seed <- sample.int(10000, 1)#if its not provided then we generate random seed
    set.seed(seed)
    message("Random seed = ", seed, "\n")} 
  
  
  startTime <- Sys.time()#Starting time 
  
  
  grid <- create_grid(parameters, nrep) #Step 1: create grid
  
  if(cores > 1){
    plan(multisession,workers = cores)
  } else{
    plan(sequential)
  }
  suppressMessages(raw_data <- data_generation(simulation, grid))
  
  summary <- summary_function(sum_fun, data_input=raw_data) #Step 3: Summary statistics
  
  average_over_reps <- average_function(grid_for_avg=create_grid(parameters, 1), summary, nrep)
  
  comb <- cbind(grid, summary) #Step 4: Combine resuluts with parameters
  
  array_1 <- create_array_function(comb, parameters, nrep) #Step 5: Create array
  
  endTime <- Sys.time()#Endtime
  
  cpt <- endTime - startTime#Execution time
  
  summary_1 <- output_function(array_1,average_over_reps,parameters,cores,simulation,
                           nrep,cpt)
  
return(summary_1)
}




# TESTING MAIN FUNCTION --------------------------------------------------

param_list3x <- list(c("n", 10, 100, 10)
                     ,c("mu", 0, 10, 1)
                     ,c("sd", 0, 5, 1))


test_me <- main_function(parameters=param_list3x
              , nrep = 5
              , simulation = rnorm
              , sum_fun="mean"
              ,seed=123 
              ,cores=1)


###


param_list3x <- list(c("n", 10, 30, 10)
                     ,c("mu", 0, 3, 1)
                     ,c("sd", 0, 2, 1))


mc <- main_function(parameters=param_list3x
                    , nrep = 10
                    , simulation = rnorm
                    , sum_fun="mean")

print(mc, n=20, na.print = FALSE)

#test run with runif 
param_list3x <- list(c("n", 10, 100, 10)
                     ,c("min", 0, 10, 1)
                     ,c("max", 0, 10, 1))


main_function(parameters=param_list3x
              , nrep = 5
              , simulation = runif
              , sum_fun="mean")



#test run with pois
param_list_rpois <- list(c("n", 10, 30, 10)
                         , c("lambda", 0, 5, 1))

main_function(parameters=param_list_rpois
              , nrep = 2
              , simulation = rpois
              , sum_fun="mean"
              , parallel = FALSE)




# Minimal working example for plot? ---------------------------------------

'Plot just for 1 dimensional parameter input?
x-achsis: variable
y-achsis: mc-results
+ facet maybe`?'




# selfmade function for data generation (MA(1)) ------------------------------------------------------------

selfmade_dgp <-  function(n, alpha){
  errors <- arima.sim(model = list(ma = c(alpha)), n = n) # MA(1) process
  beta0 <- rep(2, n) #beta0 = 2
  beta1 <- 3 #beta1 = 3
  x <- 0:(n-1) #x vector
  y <- beta0 + errors
  return(y)
}

param_list3x <- list(c("n", 10, 100, 10)
                     ,c("alpha", 0, 1, 0.1))

create_grid(param_list3x, nrep = 3)

main_function(parameters=param_list3x
              , nrep = 2
              , simulation = selfmade_dgp
              , sum_fun="mean")





# summary statistics test functions (OLS and ttest)  --------------------------------------


### t test
ttest<-function(data_input){ 
  
  # generate sample:
  #sample<-rnorm(n, loc, scale)
  
  # calculate test statistic:
  stat<-sqrt(length(data_input))*mean(data_input)/sd(data_input)
  
  # get test decision:
  decision<-abs(stat)>1.96
  
  # return result:
  return(decision)
  #return(list("decision"=decision))
}


### OLS Function
first_part <- test_data[[1]]
lm(data=first_part, y)
x_vec <- 1:length(first_part)

ols <- function(data_input){
  y <- data_input
  myOLS <- lm(y ~ 1)
  sumOLS <- summary(myOLS)
  output <- sumOLS$sigma^2
  return(output)
}



###########GGplot
#First we have to deal with grouping the data but we dont need to worry about that since we use the output of the average function. 
#Because parameters are already separated, we can call any input parameters by `$` sign. 


#Second, the asks about using ggplot for some spesific classes that we implemented before. 
#For that, I just included line 375 in summary_1 function. 
#The reason for that , ggplot works only with some spesific classes like data.frame etc.
#on summary_1 function I used the name "Eco" for the class of "out" which is in 
#list format and stores the result of the monte carlo simulation,average and summary parts.
#With line 375 we are able to use ggplot without making any changes.


#Lets try it with a simple example. 

#I used the parameters from the line  461.


param_list3x <- list(c("n", 10, 100, 10)
                     ,c("mu", 0, 10, 1)
                     ,c("sd", 0, 5, 1))


test_me <- main_function(parameters=param_list3x
                         , nrep = 5
                         , simulation = rnorm
                         , sum_fun="mean"
                         ,seed=123 
                         ,cores=1)

test_me$average # $avarege is the place where the averaged result is stored.
#for ggplot part we have to use it.
#Lets check if we can call the each parameters.
test_me$average %>% select(n)# So, it's nice that we even dont need to use group by
#Or
test_me$average %>% select(n,mu)# It works well! : )

##For the second issue I was talking about on the line 591.
#LEts use an example. Just think about the parameters a,b,c the parameter input and
#d is the simulation result. And kkk is the output of the average function.

a <- c(1:660)
b <- c(660:1319)
c <- c(1320:1979)
d <- c(1980:2639)

kkk <- data.frame(a,b,c,d)

#Lets use ggplot.
ggplot(kkk,aes(x=a,y=b,col=c))+geom_line()
#It works well and no problem with the class becuase it's in dataframe
class(kkk)
#[1] "data.frame"


#On the task we are asked to use ggplot on the simulation result with
#some specific class that we implemented.

#I used the name  "Eco" for class of simulation result.
#Now ,I should also implement the  same class for the average function kkk.

class(kkk) <- "Eco"
class(kkk)
#[1] "Eco"
ggplot(kkk,aes(x=a,y=b,col=c))+geom_line() #Error
#Why? Becuase ggplot works with some specfic class. like data.frame..
#That why we should change the class to data.frame or find another way.

#The line 375 created for this purpose. 
remove(kkk)

kkk <- data.frame(a,b,c,d)#Lets created the kkk again. 
class(kkk)
#[1] "data.frame"

class(kkk) <- c("Eco",class(kkk))
#Now, we have the class that  we have implemented. Now  we can us ggplot.

ggplot(kkk,aes(x=a,y=b,col=c))+geom_line()#Works well!

##Lets try it on our function.
#we already ran the function in the line 609 and stored the result in the name
#"test_me"
class(test_me)#[1] "Eco"  "list"
class(test_me$results)#[1] "Eco"   "array"
class(test_me$average)#[1] "Eco"        "data.frame"

#A simple ggplot.
ggplot(test_me$average,aes(x=n,y=sd,col=mu))+geom_line()#It works.


#Now lets try it with facet_grid 
test_me$average %>%
  ggplot(aes(x = avg, y = mu, col = sd )) + 
  facet_grid(n ~ sd) +
  geom_line()#It also works with facet!



##Paralellization#

# The paralellization implemented in data generation part. 
#Since the simulation runs here. 
#Before using paralelization the default was;

#map(var1, simulation)#for grids with 2 parameters
#map2(var1, var2, simulation)#for grids with 3 parameters
#pmap(list1,.f=simulation)##for grids with 4 parameters

#For paralellization, basically the equivalent functions 
#in the furrr package are used.
#map ---> future_map
#map2 ---> future_map2
#pmap ----> future_pmap


#.options = furrr_options(seed = TRUE) #is used for random number generation
#This function takes control of thr RNG process for paralleization. 
#It generates the same  numbers  for the given seed.

#Also in case of the missing seed , seed producer code is included as below.
if(!is.null(seed)) {#Reproducibility
  set.seed(seed)}#If seed provided then set.seed takes the number
else {
  warning("No seed provided!", call. = FALSE)
  seed <- sample.int(10000, 1)#if its not provided then we generate random seed
  set.seed(seed)
  message("Random seed = ", seed, "\n")} 

#Two methods : `Multisession` used  for paralelization and 
#`Sequential` used  to run simulation without paralellization.
#Cores  are used to indicate the number of cores that the user wanted use in paralellization.
#If the core is 1 no paralellization used in the simulation.Otherwise `Multisession`
#used for paralelization

if(cores > 1){
  plan(multisession,workers = cores)
} else{
  plan(sequential)
}

#`detectCores`checks the maximum number of the cores in the machine.
#By using if else commands we restrict the user to choose the number of cores is equal to maximum
#or less tahn maximum. Otherwise simulation would stop and the shows the Error message below.

max.cores <- detectCores()
if(cores>max.cores){
  stop("Number of Cores cannot be bigger than total number of cores")
}


#####################Lets check the execution times for paralel process###

ols_f <- function(n,mu,sd){
  e <- rnorm(n,mu,sd)
  x <- runif(n)
  y <- 0.5*x + e
  ols.hat <- t(x) %*% y / t(x)%*%x
  return("ols"=ols.hat)}

gls_f <- function(n,mu,sd){
  e <- rnorm(n,mu,sd)
  x <- runif(n)
  y <- 0.5*x + e
  v.inv <- diag(1/(1:n))
  c <- chol(v.inv)
  cy <- c %*% y
  cx <- c %*% x
  gls_hat <- t(cx) %*% cy / t(cx)%*%cx
  return("gls"=gls_hat)
}

param_list <- list(c("n",100,1000,100),c("mu",0,1,0.25),c("sd",1,2,.5))

ols <- main_function(parameters = param_list,nrep=5,simulation = ols_f,sum_fun="mean",seed=123,cores=1)
#Ols function is less demanding than gls function thats why parallelization will be checked for gls function. 

#Lets check the execution times of gls function with 1 core and 8 cores
gls <- main_function(parameters = param_list,nrep=5,simulation = gls_f,sum_fun="mean",seed=123,cores=1)
#With one core it took 47 seconds

gls <- main_function(parameters = param_list,nrep=5,simulation = gls_f,sum_fun="mean",seed=123,cores=8)
#only 24 seconds. So, it works.


#Now lets visualise it 
gls$average <-  gls$average %>% mutate(mse =(2-avg)^2 )
ols$average <-  ols$average %>% mutate(mse =(2-avg)^2 )

ols$average %>% ggplot(aes(x=avg,y=mu,col="OLS"))+
  facet_grid(n~mean(mse))+geom_line()+
 geom_line(data=gls$average,aes(x=avg,y=mu,col="GLS"))+
  scale_color_manual(name = "Estimation", values = c("OLS" = "blue", "GLS" = "red"))
###or any other combinations
