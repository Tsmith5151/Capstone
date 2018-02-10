## Simulate Dataset Study ##
## Generate a dataset with 'n' number of variables with a range of 'gamma', 'poisson', and 'categorical' variables with 'n' observations

library(simstudy)
library(ggplot2)
library(GGally)
set.seed(1)


simulation_1 <- function (n_cols,n_rows,mu_range,std_range,n_noise){
  
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
  
  # Noise Variables:
  for (i in 1:n_noise+1){
    add <- defDataAdd(varname=paste('noise',i), dist = "normal", formula = "10*var.1 + 30*var.1", variance = 3)
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
  
  p <- ggpairs(data1[,2:(ncol(data)-1)],lower=list(continuous=wrap("smooth", colour="navy")),
          diag=list(continuous=wrap("barDiag", fill="darkcyan")))

  plot <- p + labs(title= paste("Simulated Dataset Case:",sim_case)) + theme(plot.title = element_text(size=14,face="bold")) + 
    theme(plot.title = element_text(hjust = 0.5))

  return(plot)
}

###################

# Input Arguments
mu_range <- c(0.50,20)
std_range <- c(2,10)
n_cols <- 3
n_rows <- 1000
n_noise <- 2 
yes_prob <- 0.75
no_prob <- 0.25

# Run Simulation 
data1 <- simulation_1(n_cols,n_rows,mu_range,std_range,n_noise)

# Visualize Dataset
plot_data(data1,'1')


