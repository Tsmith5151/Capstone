library(simstudy)
library(ggplot2)
library(GGally)
set.seed(1)


sim_data <- function(n_obs,n_noise,n_ev){
  
  data <- c()
  # Noise Variables
  for (i in 1:n_noise){
    data<- defData(data,varname=paste0('n',i), dist="normal", formula = "0", variance = .1, link = "identity")
  }
  
  # Explanatory Variables
  for (i in 1:n_ev){
    data<- defData(data,varname=paste0('EV',i), dist="normal", formula = "0", variance = .1, link = "identity")
  }

  # EV Names
  ev <- c()
  weights <- round(runif(n_noise+n_ev,0,1),1)
  for (i in (1:n_ev)){
          ev[[i]] <- (paste0(weights[[i]],'*EV',i,sample(c('-','+'),1)))
          }
 
  # Build Response Variable
  x <- (paste(unlist(ev), collapse=''))
  y_int = 0.10
  formula <- paste0(y_int,'+',x)
  
  #print(formula)
  
  data <- defDataAdd(data,varname="y", dist="binary", formula=formula, link = "logit")
  
  # Build Simulated Data 
  data <- genData(n_obs,data)
  
  return(as.data.frame.matrix(data))
}


plot_data <- function(data){
  
  # Parameters:
  # ---------------
  # data: input simulated dataframe

  # Returns:
  # ---------------
  # Scatter Matrix of Simulated Dataset
  
  # Only plot continous variables
  data <-data[,2:(ncol(data)-1)]
  cols <- sapply(data,is.integer)
  data <- data[,!cols]
  
  p <- ggpairs(data,lower=list(continuous=wrap("smooth", colour="navy")),
               diag=list(continuous=wrap("barDiag", fill="darkcyan")))
  
  plot <- p + labs(title= paste("Simulated Dataset")) + theme(plot.title = element_text(size=14,face="bold")) + 
    theme(plot.title = element_text(hjust = 0.5))
  
  return(plot)
}

x <- sim_data(100,2,4)
plot_data(x)



#gfit = glm(y~EV1 + EV2 + EV3 + EV4 + n1 + n2, family = "binomial", data = x)
#step(gfit)

