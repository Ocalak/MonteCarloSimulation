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
  storage <- list() #store sequences in this list
  name_vec <- c()
  
  for(i in 1:length(input)){ #1:3
    a <- as.numeric(input[[i]][[2]])
    b <- as.numeric(input[[i]][[3]])
    c <- as.numeric(input[[i]][[4]])
    output <- seq(from=a, to=b, by=c) #creates sequence for each parameters
    storage[[i]] <-  output
    name_vec[i] <- input[[i]][[1]] #store variable names in names_vec
  }
  
  grid <- expand_grid(unlist(storage[1]) #create parameter grid
                      , unlist(storage[2])
                      , unlist(storage[3])
                      , unlist(storage[4])
                      , unlist(storage[5])
                      , c(1:nrep)) #extra column for numbering repetitions
  
  names(grid) <- c(name_vec, "rep") #rename columns of paramteter grid
  
  return(grid)
}




# TESTING  create_grid() --------------------------------------------------

#One parameter (works)
param_list1 <- list(c("n", 10, 20, 10))
create_grid(param_list1, nrep=10)
create_grid(param_list1, nrep=1)

#two parameter (works)
param_list2 <- list(c("n", 10, 20, 10)
                    ,c("mu", 0, 1, 0.25))
create_grid(param_list2, nrep=3)

#three parameters (works)
param_list3 <- list(c("n", 10, 20, 10)
                    ,c("mu", 0, 1, 0.25)
                    ,c("sd", 0, 0.3, 0.1))
create_grid(param_list3, nrep=10)



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

data_generation <- function(simulation, grid){
  
  if(ncol(grid)==2){ #for one parameter + nrep = 2
    var1 <- c(unlist(grid[,1]))
    if(cores>1){
      data <- future_map(var1, simulation, #with parallelisation
                         .options = furrr_options(seed = TRUE))
    }else{
      data <- map(var1, simulation) #without parallelisation
    }
  }
  
  if(ncol(grid)==3){ #for two parameters + nrep = 3
    var1 <- c(unlist(grid[,1]))
    var2 <- c(unlist(grid[,2]))
    if(cores>1){
      data <- future_map2(var1, var2, simulation, #with parallelisation
                          .options = furrr_options(seed = TRUE))
    } else{
      data <- map2(var1, var2, simulation) #without parallelisation
    }
  } 
  
  if(ncol(grid)==4){ #for three parameters + nrep = 4
    var1 <- c(unlist(grid[,1]))
    var2 <- c(unlist(grid[,2]))
    var3 <- c(unlist(grid[,3]))
    list1 <- list(var1,var2,var3)
    if(cores>1){
      data <- future_pmap(list1, .f=simulation, #with parallelisation
                          .options = furrr_options(seed = TRUE))
    }else{
      data <- pmap(list1, .f=simulation) #without parallelisation
    }
  }
  
  return(data)
}



# TESTING data_generation() -------------------------------------------------


#for one parameter (works)
cores <- 1 #set number of cores to 1
grid1 <- create_grid(param_list1, nrep=3)
data_generation(simulation=rnorm, grid=grid1)

#for two parameter (works)
grid2 <- create_grid(param_list2, nrep=3)
print(grid2, n=nrow(grid2))
data_generation(simulation=rnorm, grid=grid2)


#for different data generation function (runif) WORKS WELL!
param_list_runif <- list(c("n", 10, 30, 10)
                         , c("min", 0, 0, 0)
                         ,c("max", 1, 1, 0))


grid_unif <- create_grid(param_list_runif, nrep=3)
data_generation(simulation=runif, grid=grid_unif)

#for different data generation function (rpois) WORKS WELL!
param_list_rpois <- list(c("n", 10, 30, 10)
                         , c("lambda", 0, 10, 1))
grid_pois <- create_grid(param_list_rpois, nrep=3)
print(grid_pois, n=nrow(grid_pois))
data_generation(simulation=rpois, grid=grid_pois)



# Part 3: Summary statistics ----------------------------------------------

'Next we implement the summary function, that analyses the given data point.
sum_fun = the summary function we want to use on the data
data_input = the data points we want to analyse'

#summary function for one input
summary_function <- function(sum_fun, data_input){
  
  count <- length(data_input) #iterate over each element of the list
  summary_matrix <- matrix(nrow=count, ncol=1) #prepare matrix to store the results
  
  for(i in 1:count){
    input <- list(data_input[[i]])
    output <- sapply(sum_fun, do.call, input) #sapply to apply the summary function
    summary_matrix[i] <- output
  }

  colnames(summary_matrix) <- sum_fun #properly name the column
  return(summary_matrix) #return the nrow(data_input)X1-dimensional matrix
}



# TESTING summary_function() ---------------------------------------------------

grid_test <- create_grid(param_list3, nrep=3) #create grid
test_data <- data_generation(simulation=rnorm, grid=grid_test) #create data
summary_function(sum_fun=list("mean"), data_input=test_data) #apply mean()
summary_function(sum_fun=list("max"), data_input=test_data) #apply max()
summary_function(sum_fun=list("sd"), data_input=test_data) #apply sd()

'Works for different summary functions. Next, we want to combine our results with the parameter grid and create
arrays, that structure our simulation input with the given results.'



# Part 5: Create arrays ---------------------------------------------------

'Next, we want to store the summary statistics in an array, where you can easily read
the given parameter constellation. We have to reorder the output, cause arrays gets filled
column wise.'


create_array_function <- function(comb, parameters, nrep){
  storage <- list() #prepare list
  name_vec <- c() #prepare vector for naiming
  
  for(i in 1:length(parameters)){ #this creates the sequences of parameters
    a <- as.numeric(parameters[[i]][[2]])
    b <- as.numeric(parameters[[i]][[3]])
    c <- as.numeric(parameters[[i]][[4]])
    output <- seq(from=a, to=b, by=c)
    storage[[i]] <-  output
    name_vec[i] <- parameters[[i]][[1]] #this just stores the names of the variables
  }
  
  
  matrix.numeration <-  paste("rep","=", 1:nrep, sep = "") #prepare the right naming of the elements of the matrix
  
  if(length(parameters)==1){ #for 2 dimensional array (1 variables + 1 dimension for repetition)
    comb_ordered <-  comb %>% arrange(comb[,2]) #reorder the data, in order to be columnwise filled into the array
    seq1 <- c(unlist(storage[1]))
    
    row.names <- paste(name_vec[1],"=",seq1, sep = "")
    
    dimension_array <- c(length(seq1), nrep) #saves the correct dimensions of the array
    dim_names_list <- list(row.names, matrix.numeration) #saves the names for the rows/columns of the array
  }
  
  if(length(parameters)==2){ #for 3 dimensional array (2 variables + 1 dimension for repetition)
    comb_ordered <-  comb %>% arrange(comb[,2])  %>% arrange(comb[,3])
    seq1 <- c(unlist(storage[1]))
    seq2 <- c(unlist(storage[2]))
    
    row.names <- paste(name_vec[1],"=",seq1, sep = "")
    column.names <-  paste(name_vec[2],"=",seq2, sep = "")
    
    dimension_array <- c(length(seq1), length(seq2), nrep)
    dim_names_list <- list(row.names, column.names, matrix.numeration)
  }
  
  if(length(parameters)==3){ #for 4 dimensional array (2 variables + 1 dimension for repetition)
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
  
  
  array1 <- array(comb_ordered[,ncol(comb)] #uses the preperation from before and creates the array with the right dimensions
                  , dim = dimension_array #list with dimensions
                  , dim_names_list) #list with names
  return(array1)
}



# PREP TEST create_array_function() ----------------------------------------------
'Need to set up an altered version of main_function() first to be able to test
create_array_function() at this point.'

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


param_list3x <- list(c("n", 10, 80, 10)
                     ,c("mu", 0, 7, 1)
                     ,c("sd", 0, 5, 1))

comb1 <- main_function_array_test(parameters=param_list3x
                                  , nrep = 5
                                  , simulation = rnorm
                                  , sum_fun="mean")

comb1 #this is how the df with all permutations and results looks like

'array function takes this data fram and turns it into a array with the right dimensions'
create_array_function(comb=comb1, parameters=param_list3x, nrep=3)



# Part 6: averaging over all repetitions --------------------------------


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





# TESTING average_function() ----------------------------------------------


param_list3x <- list(c("n", 10, 80, 10)
                     ,c("mu", 0, 7, 1)
                     ,c("sd", 0, 5, 1))

comb1 <- main_function_array_test(parameters=param_list3x
                                  , nrep = 5
                                  , simulation = rnorm
                                  , sum_fun="mean")


grid_test <- create_grid(param_list3x, nrep=5)
test_data <- data_generation(simulation=rnorm, grid=grid_test)
summary1 <- summary_function(sum_fun=list("mean"), data_input=test_data)


average_over_reps <- average_function(grid_for_avg=create_grid(param_list3, 1), #set nrep=1 here, since the number of repetitions wont be relevant anymore
                                      summary=summary1, nrep=5)



average_over_reps #averaged results in a data frame
create_array_function(comb=comb1, parameters=param_list3x, nrep=1) #use nrep=1, since there are no different draws for each parameter constellation anymore

# Part 7: output_function ------------------------------------------------------
'Goal is to create a function, that takes the MC resuluts and all parameter input
and converts it onto output format that prints nicely into the console'



output_function <- function(array_1,average_over_reps,parameters,cores,simulation,
                            nrep,cpt){
  # (1)
  out <- list() 
  # (2) 
  class(out) <- "Eco" 
  # (3)
  out$results <- array_1 
  out$average <- average_over_reps 
  # (4)
  class(out$average) <- c("Eco",class(out$average))
  class(out$results) <- c("Eco",class(out$results))
  # (5)
  if(cores>1){
    parallel = "Multisession"
  } else {       
    parallel = "Sequential"
  }
  text <-  cat("\n",
               "Repetition(nrep)      : ",nrep,"\n\n",
               "Parallelisation Type  : ",parallel,"\n\n",
               "Number of Cores Used in  Parallelisation : ",cores,
               " out of",detectCores(),"\n\n",
               "Input Parameters : ",paste(parameters),"\n\n",
               "Simulation Length :",length(array_1),"\n",
               "Minumum :",min(array_1),"\n",
               "Maximum :",max(array_1),"\n",
               "Mean    :", mean(array_1),"\n",
               "Median  :",median(array_1),"\n\n",
               "Execution Time of Monte Carlo Simulation",
               as.numeric(cpt),"secs \n\n",
               "Name of The Class :",class(out))
  # (6)
  return(out)
}

# Part 7: main function part ------------------------------------------------------


#(1)
main_function <-  function(parameters 
                           , nrep 
                           , simulation 
                           , sum_fun 
                           , seed = NULL
                           , cores=NULL){
  
  
  #(2)
  max.cores <- detectCores()
  if(cores>max.cores){
    stop("Number of Cores cannot be bigger than total number of cores")
  }
  #(3)
  if(!is.null(seed)) {
    set.seed(seed)}
  else {
    warning("No seed provided!", call. = FALSE)
    seed <- sample.int(10000, 1)
    set.seed(seed)
    message("Random seed = ", seed, "\n")} 
  
  #(4) and (5)
  startTime <- Sys.time()#Starting time 
  
  
  
  grid <- create_grid(parameters, nrep) 
  
  if(cores > 1){
    plan(multisession,workers = cores)
  } else{
    plan(sequential)
  }
  suppressMessages(raw_data <- data_generation(simulation, grid))
  
  summary <- summary_function(sum_fun, data_input=raw_data) 
  
  average_over_reps <- average_function(grid_for_avg=create_grid(parameters, 1),
                                        summary, nrep)
  
  comb <- cbind(grid, summary) 
  
  array_1 <- create_array_function(comb, parameters, nrep) 
  
  endTime <- Sys.time()
  
  cpt <- endTime - startTime
  
  #(6)
  summary_1 <- output_function(array_1,
                               average_over_reps,
                               parameters,cores,
                               simulation,nrep,cpt)
  
  return(summary_1)
}

```

# TESTING MAIN FUNCTION --------------------------------------------------

'
The tests below present the performance of `main_function()` with the normal distribution.
'

## Test 1 : Summary performance


param_list3x <- list(c("n", 10, 100, 10)
                     ,c("mu", 0, 10, 1)
                     ,c("sd", 0, 5, 1))

test_me <- main_function(parameters=param_list3x
                         , nrep = 5
                         , simulation = rnorm
                         , sum_fun="mean"
                         , seed=123 
                         , cores=1)


test_me



## Test 2 : Visualization performance


ggplot(test_me$average,aes(x=avg,y=n))+geom_line()




ggplot(test_me$average,aes(x=avg))+facet_grid(n~.)+
  geom_density()


# Examples

## Comparing the execution times of OLS and GLS simulations

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
  
  param_list <- list(c("n",100,1000,100),c("mu",0,1,0.25),c("sd",1,2,.5))
}


'As shown above, simple OLS and GLS functions are defined to find $\beta$ coefficients. However, the execution time of the GLS  would be much longer than OLS since the Cholesky Decomposition (`chol()`) is applied to  the GLS function. Parallel programming increases efficiency when each iteration is sufficiently expensive in terms of processor time. Since GLS function is more complicated then OLS in this example, paralle processing reduce the execution time in GLS but not in OLS.

'


#OLS simulation without parallel processing:
  

param_list <- list(c("n",100,1000,100),c("mu",0,1,0.25),c("sd",1,2,.5))
ols <- main_function(parameters = param_list,
                     nrep=5,
                     simulation = ols_f,
                     sum_fun="mean",
                     seed=123,
                     cores=1)




#OLS simulation with parallel processing:
  

ols <- main_function(parameters = param_list,
                     nrep=5,
                     simulation = ols_f,
                     sum_fun="mean",
                     seed=123,
                     cores=4)




#GLS with parallel processing:
gls <- main_function(parameters = param_list,
                     nrep=5,
                     simulation = gls_f,
                     sum_fun="mean",
                     seed=123,
                     cores=4)





#GLS without parallel processing:
  

gls <- main_function(parameters = param_list,
                     nrep=5,
                     simulation = gls_f,
                     sum_fun="mean",
                     seed=123,
                     cores=1)





## Visualisation 



#Visualizing MSE(Mean Square Error) of OLS and GLS simulations:
  
  


gls$average <-  gls$average %>% mutate(mse =(2-avg)^2 )
ols$average <-  ols$average %>% mutate(mse =(2-avg)^2 )

ols$average %>% ggplot(aes(x=avg,y=mu,col="OLS"))+
  facet_grid(n~mean(mse))+geom_line()+
  geom_line(data=gls$average,aes(x=avg,y=mu,col="GLS"))+
  scale_color_manual(name = "Estimation",
                     values = c("OLS" = "blue", "GLS" = "red"))





#Density graph of MSE of $\beta$ in OLS and GLS simulations

ggplot(ols$average,aes(x=mse,col="OLS"))+facet_grid(n~.)+
  geom_density()+
  geom_density(data=gls$average,aes(x=avg,col="GLS"))+
  scale_color_manual(name = "Estimation", 
                     values = c("OLS" = "blue", "GLS" = "red"))





  
  
  
  

#*Output of the OLS simulation
main_function(parameters = param_list,
              nrep=5,
              simulation = ols_f,
              sum_fun="mean",
              seed=123,
              cores=1)

#Output of the OLS simulation with parallelisation

main_function(parameters = param_list,
              nrep=5,
              simulation = ols_f,
              sum_fun="mean",
              seed=123,
              cores=4)




#Output of the GLS simulation without parallelisation
  

main_function(parameters = param_list,
              nrep=5,
              simulation = gls_f,
              sum_fun="mean",
              seed=123,
              cores=1)


#Output of the GLS simulation with parallelisation
main_function(parameters = param_list,
              nrep=5,
              simulation = gls_f,
              sum_fun="mean",
              seed=123,
              cores=4)

