library(simstudy)
library(stringr)

# Extra stuff to add
# categorical variables
# Vectors for variances/ mean
# Random forest
# UI

simulateData <- function (n_noise,n_explanatory, n_datapoints, n_categorical){
  simformula <- ""
  if(n_noise > 0){
    def <- defData(varname="n1", dist="normal", formula = "0", variance = .1, link = "identity")
    if(n_noise > 1) {
      for (i in 2:n_noise) {
        simvarname <- paste(c("n",i), collapse = " ")
        simvarname <- str_replace(simvarname, fixed(" "), "")
        def <- defDataAdd(def,varname = simvarname, dist="normal", formula = "0", variance = .1, link = "identity")
      }
    }
  }
  else {
    print("No noise variables")
  }
  if(n_explanatory > 0) {
    simformula <- ".1"
    for (i in 1:n_explanatory) {
      simvarname <- paste(c("EV",i), collapse = " ")
      simvarname <- str_replace(simvarname, fixed(" "), "")
      def <- defDataAdd(def,varname = simvarname, dist="normal", formula = "0", variance = .1, link = "identity")
      weight <- 0.1 * i
      simformula <- paste(c(simformula," + "), collapse = " ")
      simformula <- paste(c(simformula,weight), collapse = " ")
      simformula <- paste(c(simformula,"*"), collapse = " ")
      simformula <- paste(c(simformula,simvarname), collapse = " ")
    }
    def <- defData(def, varname = "response", dist = "binary", formula = simformula, link = "logit")
    print(simformula)
    print(def)
  }
  else {
    print("No explanatory variables")
  }
  
  simdata <- genData(n_datapoints, def)
  #summary(simdata)
  return(simdata)
  
}
runLogistic <- function(data) {
  model = glm(response ~., family = "binomial", data = data)
  step(model)
  #summary(model)
  #anova(model)
  predict <- predict(model, type = 'response')
  table(data$response, predict >= 0.5)
}


simdata <- simulateData(10,5,10000)
runLogistic(simdata)

