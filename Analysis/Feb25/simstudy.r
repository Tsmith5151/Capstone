library(simstudy)
library(stringr)


simulateData <- function (n_noise,n_explanatory){
  simformula <- ""
  if(n_noise > 0){
    def <- defData(varname = "rn1", dist = "normal", formula = "-1;1", variance= 0.5)
    if(n_noise > 1) {
      for (i in 2:n_noise) {
        simvarname <- paste(c("rn",i), collapse = " ")
        simvarname <- str_replace(simvarname, fixed(" "), "")
        def <- defDataAdd(def,varname = simvarname, dist = "normal", formula = "-1;1", variance= 0.5)
      }
    }
  }
  else {
    print("No noise variables")
  }
  if(n_explanatory > 0) {
    for (i in 1:n_explanatory) {
      simvarname <- paste(c("EX",i), collapse = " ")
      simvarname <- str_replace(simvarname, fixed(" "), "")
      def <- defDataAdd(def,varname = simvarname, dist = "normal", formula = "-1;1", variance= 0.5)
      weight <- 0.1 * i
      if(i > 1) {
        simformula <- paste(c(simformula," + "), collapse = " ")
      }
      simformula <- paste(c(simformula,weight), collapse = " ")
      simformula <- paste(c(simformula," * "), collapse = " ")
      simformula <- paste(c(simformula,simvarname), collapse = " ")
    }
    def <- defData(def, varname = "response", dist = "binary", formula = simformula, link = "logit")
  }
  else {
    print("No explanatory variables")
  }
  
  data <- genData(1000, def)
  summary(data)
  
}
runModel <- function(data) {
  model <- lm(response ~., data = data)
  summary(model)
  #anova(model)
  predict <- predict(model, type = 'response')
  table(data$response, predict >= 0.5)
}


simulateData(10,5)
runModel(data)

