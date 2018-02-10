## Simulate Dataset Study ##
## Generate a dataset with 'n' number of variables with a range of 'gamma', 'poisson', and 'categorical' variables with 'n' observations

library(simstudy)
library(ggplot2)
library(GGally)
set.seed(1)


simulation_1 <- function (n_cols,n_rows,mu_range,std_range){
  
  # Parameters:
  # ---------------
  # n_cols: number of columns in dataset
  # n_rows: number of rows in dataset
  # mu_range: population mean (range)
  # std_range: population standard deviation
  #
  # Returns:
  # ---------------
  # Simulated Dataset --> Case 1 with all Numerical and Categorical Variables
  
  # List of types of distributions
  distributions <- c('gamma','normal','poisson','categorical')

  # Create First Column of DataFrame
  def <- defData(varname = "var.1", dist = "normal", formula = sample.int(mu_range,1), variance = 5)
  df <- genData(n_rows,def)

  # Append Random Columns to Dataframe
  for (i in 2:n_cols){

      # Mu and Stdev Sample Values
      mu <- sample.int(mu_range,1,replace=TRUE)
      var <- sample.int(std_range,1,replace=TRUE)
      
      # Probabilities of selecting a given distribution 
      dist<- sample(distributions,1,prob = c(0.15,0.50,0.15,0.20))
  
      # Poisson Distribution 
      if (dist == 'poisson'){
          add <- defDataAdd(varname = paste('var',i), dist = dist, formula = mu, link='log')
      } 
      # Categorical Variables
      else if (dist == 'categorical'){
        add <- defDataAdd(varname = paste('var',i), dist = dist, formula = "0.1;0.1;0.3;0.5")
      } 
      # Draw from Random Normal Distribution with Mu and Stdev 
      else {
        add <- defDataAdd(varname = paste('var',i), dist = dist, formula = mu, variance = var)
      }
      
      # Add Columns
      df <- addColumns(add,df)
  }
  
  # Add Class Labels:
  labels <- sample(c('yes','no'),size=n_rows,p=c(yes_prob,no_prob),replace=TRUE)
  df <- cbind(df,labels)
  
  # Convert table to Data.Frame
  df <- as.data.frame.matrix(df) 
  
  # Rename column --> ToDo: check column labeling 
  colnames(df)[colnames(df) == 'var 10'] <- 'var.10'
  
  return(df)
}


plot_data <- function(data,sim_case){
  
  # Parameters:
  # ---------------
  # data: input simulated dataframe
  # sim_case: title of simulation case
  #
  # Returns:
  # ---------------
  # Scatter Matrix of Simulated Dataset
  
  p <- ggpairs(data1[,2:(ncol(data1)-1)],lower=list(continuous=wrap("smooth", colour="navy")),
          diag=list(continuous=wrap("barDiag", fill="darkcyan")))

  p + labs(title= paste("Simulated Dataset Case: ",sim_case)) + theme(plot.title = element_text(size=12,face="bold"))
  return(p)
}

###################

# Input Arguments
mu_range <- c(10,20,30,50)
std_range <- c(.5,1.5,3,5,20)
n_cols <- 10
n_rows <- 1000
yes_prob <- 0.75
no_prob <- 0.25

# Run Simulation 
data1 <- simulation_1(n_cols,n_rows,mu_range,std_range)

# Visualize Dataset
plot_data(data1,'1')


