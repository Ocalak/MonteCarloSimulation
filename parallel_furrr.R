#Parameter Grids
param_grid <- function(...){
  point <- list(...)
  if(is.null(names(point)) == TRUE){
    stop("Please provide a name for each parameters")
  }
  a <- expand.grid(...)%>% tibble()
  return(a)
}
#' Generate parameters and test the function
#' n <- c(50,100,1000)
#' loc <- c(1,2,3)
#' ttn <- c("bus","train")
#'  param_grid("Money"=n,"Location"=loc,"Transportation" = ttn)


###########################
####################################
#Test
n <- c(5,4,6)
v <- c(2,6,9)
c <- c(2,4,5)
parameter_list <- list("n"=n,"v"=v,"c"=c)
param_names <- names(parameter_list)
#Lets created user-def. test function
user_def <- function(n,v,c){
#I just wanted to keep it simple for test run.
  nx<- rnorm(n,v,c)
  res <- nx
  tes <- nx*v
  return(data.frame("res"=res,"tes"=tes))
}

test_func <-  function(user_func,param_list){
 param_names <- names(param_list)
 test <- eval(parse(text=paste("user_func(",paste(param_names,
                                           sep="",collapse=","),")", collapse="", sep="")))
 return(test)
}

save_out <- test_func(user_func = user_def,param_list = parameter_list)
#Simply I ran user-defined function with out knowing the any parameter. and get the result 
# by using parameters form parameter list, not the parameter grid.
#On the next step I will use parameter grid to run simulation.
#Before that lets get the outputs of user defined func.

names_out <- gsub(" ", "_", names(test))#Here test_names put out the name of the output of user-defined function
nu_out <- length(names_out) #number of output of user def function
#The purpose of this code above is to get the names of result variable or `s.
#and also the number of output variable.names()



#Now lets go further and replicate user defined function "nrep" times.
#For that I use map_dfc() function;
#Basically I want to get the function below;
#' mc_inner <- function(n, v, c){
#' map_dfc(1:10, ~ gfd(n = n,v=v,c=c))
#'}
 mc_out <- function(param_list,user_func,nrep){
param_names <- names(param_list)
eval(parse(text=paste("mc_inner <- function(",
                      paste(param_names,sep="",collapse=","),
                      ",nrep){map_dfc(1:nrep, ~ user_func(",paste(param_names,sep="", 
                                                         collapse=","),")",")","}",collapse="", sep="")))

eval(parse(text =paste("mc_inner(",paste(param_names,sep="",collapse =","),",nrep)")))
}

test_out <- mc_out(param_list = parameter_list,user_func = user_def,nrep=5)

#Now I have the result for nrep times repl. monte carlo simulation
#Next step is to use this function with  parameters combination (parameter grid).

#P.S: I didnt include summarise or any other methods. I think, first we need a function that do it's work.
#We can include the summarise ggplot and also parallellization after that.

#P.S : Each time I try to develop the function. 
#Now I want to implement the function for final Mc simulation that takes the parameters from parameter grids.
#' pmap_dfc(parameter_grid,mc_out)

#There is one big issue that how to order the output of the function on the line '80'. At he moment I have a bug on final MC function. I try to fix it. 
#Thats why I didnt upload it here. But simply try the function below and for your own function and see what I meant. 
#I put very simple example here;

nx <-c(1,2)
cx <- c(2,4)
parameter_grid <- expand.grid("nx"=nx,"cx"=cx)
your_func <- function(nx,cx){
  a <- rnorm(nx,cx)
  b <- rnorm(cx,nx)
  return(c("mean_a"=mean(a),"mean_b"=mean(b)))}

any_function <- function(nx=nx,cx=cx){
  map_dfc(1:10, ~ your_func(nx=nx,cx=cx))}

res <- pmap_dfc(parameter_grid,any_function)#Now check the output of the res. I couldnt how can I order these outputs? Any idea. 


#########################Not ready#############################
#Below that I work on final Mc function that works with parameter grids. But somwhow I get error I try to fix that. If we fix it we can go on other task.
#Like adding summarise ggplot nad parallelisation
final_MC <- function(param_list,user_func,nrep){
  param_names <- names(param_list)
  eval(parse(text=paste("mc_inner <- function(",
                        paste(param_names,sep="",collapse=","),
                        ",nrep){map_dfc(1:nrep, ~ user_func(",paste(param_names,sep="", 
                                                                    collapse=","),")",")","}",collapse="", sep="")))
  
  parameter_grid <- expand.grid("n"=n,"c"=c,"v"=v,"nrep"=nrep)
  
  final_mc <- eval(parse(text=paste("pmap_dfc(parameter_grid,mc_inner)")))
  return(final_mc)
}

final_MC(param_list= parameter_list,user_func=user_def,nrep=nrep)














