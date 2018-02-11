# Create 5 explanatory, 10 noise, and 1 response
library(simstudy)


def <- defData(varname = "x1", dist = "nonrandom", formula = 7, id = "idnum")
def <- defData(def, varname = "x2", dist = "uniform", formula = "10;20")
def <- defData(def, varname = "x3", formula = "x1 + x2 * 2", variance = 8)
def <- defData(def, varname = "x4", dist = "poisson", formula = "x1 - 0.2 * x2", link = "log")
def <- defData(def, varname = "x5", formula = "0.3;0.2;0.5", dist = "categorical")
def <- defData(def, varname = "x6", dist = "gamma", formula = "5+x5", variance = 1, link = "log")
def <- defDataAdd(def, varname = "x7", dist = "normal", formula = 0, variance = sqrt(3))
def <- defDataAdd(def,varname = "x8", dist = "normal", formula = "5 + 2.5*x1 + 1.5*T + 3.5*x1*T + x7", 
                    variance = 1)
def <- defDataAdd(def, varname="x9", formula = "10", variance = "3")
def <- defDataAdd(def, varname="x10", dist="uniform", formula="-5;5")
def <- defDataAdd(def, varname="x11", dist="poisson", formula="abs(x4)")
def <- defDataAdd(def, varname = "x12", dist = "categorical", formula = ".33;.33")
def <- defData(def, varname = "res", dist = "binary", formula = "-400 + x8 + (x4 * x4) > 0 & x12 == 2", link = "logit")

def

data <- genData(100, def)
summary(data)
model <- lm(res ~., data = data)
summary(model)
#anova(model)

# Trace's plot function
library(ggplot2)
library(GGally)
plot_data <- function(data,sim_case){
  
  # Parameters:
  # ---------------
  # data: input simulated dataframe
  # sim_case: title of simulation case
  #
  # Returns:
  # ---------------
  # Scatter Matrix of Simulated Dataset
  
  p <- ggpairs(data[,2:(ncol(data)-1)],lower=list(continuous=wrap("smooth", colour="navy")),
               diag=list(continuous=wrap("barDiag", fill="darkcyan")))
  
  p + labs(title= paste("Simulated Dataset Case: ",sim_case)) + theme(plot.title = element_text(size=12,face="bold"))
  return(p)
}

plot_data(data,'K data')



# # Define conditions
# defC <- defCondition(condition = "x == 1", formula = "5 + 2*y-.5*y^2",
#                      variance = 1,dist = "normal")
# defC <- defCondition(defC, condition = "x == 2",
#                      formula = "3 - 3*y + y^2", variance = 2, dist="normal")
# defC <- defCondition(defC, condition = "x == 3",
#                      formula = "abs(y)", dist="poisson")
# # Add column
# dt <- addCondition(defC, dt, "NewVar")
# # Plot data
# ggplot(data = dt, aes(x=y, y=NewVar, group = x)) +
#   geom_point(aes(color = factor(x)))