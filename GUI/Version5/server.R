library(simstudy)
library(ggplot2)
library(GGally)
library(randomForest)
library(ROCR)
library(caTools)
library(corrplot)
library(gplots)
set.seed(1)

linear_eq <- function(n_ev,weights,y_int){
  
  # Explanatory Variables
  ev <- c()
  if(weights == 1) {
    for (i in (1:n_ev)){
      ev[[i]] <- (paste0('0.5*EV',i,sample(c('-','+'),1)))
    }
  } else if(weights == 2) {
    for (i in (1:n_ev)){
      if(i <= n_ev / 2) {
        ev[[i]] <- (paste0('0.3*EV',i,sample(c('-','+'),1)))
      }
      else {
        ev[[i]] <- (paste0('0.7*EV',i,sample(c('-','+'),1)))
      }
    }
  }else {
    for (i in (1:n_ev)){
      if(i <= n_ev / 3) {
        ev[[i]] <- (paste0('0.2*EV',i,sample(c('-','+'),1)))
      }
      else if(i <= 2 * n_ev / 3){
        ev[[i]] <- (paste0('0.5*EV',i,sample(c('-','+'),1)))
      }
      else {
        ev[[i]] <- (paste0('0.8*EV',i,sample(c('-','+'),1)))
      }
    }
  }
  
  # Build Equation
  x <- (paste(unlist(ev), collapse=' '))
  formula <- paste0(y_int,' + ',x)
  return(substring(formula,1,nchar(formula)-1))
  
}

sim_data <- function(n_obs,n_noise,cat,ndist,nvar,n_ev,weights,y_int){
  
  data <- c()
  
  # Noise Variables
  for (i in 1:n_noise){
    data<- defData(data,varname=paste0('N',i), dist=ndist, formula = "0", variance = nvar, link = "identity")
  }
  
  # Explanatory Variables
  for (i in 1:n_ev){
    data<- defData(data,varname=paste0('EV',i), dist="normal", formula = "0", variance = nvar, link = "identity")
  }
  
  #Categorical Variables
  if(cat != 0){
    for (i in 1:cat){
    data<- defData(data,varname=paste0('Cat',i), dist="categorical", formula = "0.3;0.2;0.5")
    }
  }
  
  # Response Variable
  data <- defDataAdd(data,varname="y", dist="binary", formula=linear_eq(n_ev,weights,y_int), link = "logit")
  
  # Build Simulated Data 
  data <- as.data.frame.matrix(genData(n_obs,data))
  
  # Convert Categorical Variables to Factors
  cols =grep('Cat', names(data), value=TRUE) 
  data[cols] <- lapply(data[cols], factor)
  
  return(data[,2:(ncol(data))])
}


# Split Training/Testing Sets
split_data <- function(data, r_split) {
  set.seed(123)
  sample = sample.split(data$y,SplitRatio = r_split)
  TRAIN <<- subset(data,sample ==TRUE)
  TEST <<- subset(data, sample==FALSE)
}


# Random Forest
do_randomforest <- function(train,test,n_tree){
  
  # Fit Model
  model.train <- randomForest(as.factor(y)~.,train,ntree=n_tree, cutoff=c(0.5,1-0.5))
  
  # Prediction
  pred<-predict(model.train, test[, !names(test) %in% c("y")], type = 'response')
  pred <- prediction(as.numeric(pred), as.numeric(test$y))
  return(pred)
}

# Logistic Regression
do_logisticregression <- function(train,test,varselect){
  
  # Fit Model
  model.train = step(lm(y ~., family = "binomial", data = train), direction = varselect)
  
  # Prediction
  pred <- predict(model.train, test[, !names(test) %in% c("y")], type = 'response')
  pred <- prediction(pred, test$y)
  return(pred)
}

# Random forest performance
getRFPerformance <- function (rf_pred) {
  rf_temp <- performance(rf_pred, "tpr", "fpr")
  rf_perf <- data.frame(fpr=rf_temp@x.values[[1]][2], tpr=rf_temp@y.values[[1]][2])
  
  rf_temp <- performance(rf_pred, "rec", "prec")
  rf_perf2 <- data.frame(prec=rf_temp@x.values[[1]][2], rec=rf_temp@y.values[[1]][2])
  rf_perf$prec <- rf_perf2$prec
  rf_perf$rec <- rf_perf2$rec
  
  rf_temp <- performance(rf_pred, "f", "acc")
  rf_perf3 <- data.frame(acc=rf_temp@x.values[[1]][2], f=rf_temp@y.values[[1]][2])
  rf_perf$acc <- rf_perf3$acc
  rf_perf$f <- rf_perf3$f
  
  rf_temp <- performance(rf_pred, "auc")
  rf_perf4 <- data.frame(auc=rf_temp@y.values[[1]])
  rf_perf$auc <- rf_perf4$auc
  
  return(rf_perf)
}



# CASE 1: Run Logistic Regression and Random Forest Simulations sweeping variance of variables
simulation_nvar <- function(n_sim,split,ncat,nrows,noise,ndist,nvar,ev,weights,yint,varselect,ntrees){
  
  # Iterate over variance 
  for (nvar in seq(from=0.50,to=5.0,by=0.50)){
    
    # Iterate of number of simulations
    for (n in 1:n_sim){
      
      # Regenerate Data
      data<-sim_data(nrows,noise,ncat,ndist,nvar,ev,weights,yint)
      
      # Split Train/Testing
      split_data(data,split)
      
      # Do RandomForest
      rf_pred <- do_randomforest(TRAIN,TEST,ntrees)
      
      # Do Logistic Regression
      lr_pred <- do_logisticregression(TRAIN,TEST,varselect)
      
      # Logistic regression performance
      
      
      # Random forest performance
      rf_perf <- getRFPerformance(rf_pred)
      rf_perf$nvar <- nvar
      
      if (exists("RF_PERF")) {
        RF_PERF <<- rbind(RF_PERF, rf_perf)
      }
      else {
        RF_PERF <<- rf_perf
      }
      
    }
  }
  result=list(LR_PERF, RF_PERF)
  return(result)
}


# CASE 2: Run Logistic Regression and Random Forest Simulations sweeping number of noise variables
simulation_num_nvar <- function(n_sim,split,ncat,nrows,noise,ndist,nvar,ev,weights,yint,varselect,ntrees){
 
  # Iterate over number of noise variables 
  for (noise in c(1,5,10,20,50)){
   
    # Number of Simulations
    for (n in 1:n_sim){
      
      # Regenerate Data
      data<-sim_data(nrows,noise,ncat,ndist,nvar,ev,weights,yint)
      
      # Split Train/Testing
      split_data(data,split)
      
      # Do RandomForest
      rf_pred <- do_randomforest(TRAIN,TEST,ntrees)
      
      # Do Logistic Regression
      lr_pred <- do_logisticregression(TRAIN,TEST,varselect)
      
      # Logistic Regreesion performance

      # Random Forest performance
      
    }
    
    #end sims
  }

  #end end
}


# CASE 3: Run Logistic Regression and Random Forest Simulations sweeping num ex. variables 
simulation_num_evar <- function(n_sim,split,ncat,nrows,noise,ndist,nvar,ev,weights,yint,varselect,ntrees){
  
  # Iterate over number of noise variables 
  for (num_ev in c(1,5,10,20,50)){
    
    # Number of Simulations
    for (n in 1:n_sim){
      
      # Regenerate Data
      data<-sim_data(nrows,noise,ncat,ndist,nvar,num_ev,weights,yint)
  
      # Split Train/Testing
      split_data(data,split)
      
      # Do RandomForest
      rf_pred <- do_randomforest(TRAIN,TEST,ntrees)
      
      # Do Logistic Regression
      lr_pred <- do_logisticregression(TRAIN,TEST,varselect)
      
      # Logistic Regreesion performance
      
      # Random Forest performance
      
    }
    
    # End sims
  }
  # end end
}



###################### PLOTS ##############################

get_line_plots <- function(lr,rf,xvar,yvar){
  
  # Logistic Regression
  plot(lr[,xvar],lr[,yvar],xlab=xvar,ylab=yvar,main=paste0('Simulation Results: ',xvar,' vs ',yvar),
       col='sienna1 ',type='b',pch=19,lwd=3,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,ylim=c(0.50,1))
  
  # Random Forest
  lines(rf[,xvar],rf[,yvar],col='lightseagreen ',type='b',pch=18,lwd=3)
  
  #grid
  grid(nx = NULL, ny = NULL, col = "white", lty = "solid")
  
  # Legend
  legend("bottomright", legend=c("RF", "LR"),
         col=c("lightseagreen" , "sienna1"), lty=1:2, cex=0.8,text.font=4,box.lty=0)
}


get_boxplots <- function(df,x_axis) {
  ggplot(df, aes(x=as.factor(df[,x_axis]), y=score, fill=df$alg)) + xlab(x_axis) + ylab("AUC Score") + geom_boxplot()  + 
    ggtitle('Logistic Regression vs Random Forest') + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(size=20)) +
    scale_fill_discrete(name = "Algorithms")
}


####################### Varying Variance ####################### 
server <- function(input, output) {
  
  # Return the requested dataset ----
  simdata <- reactive({
    SIM_DATA <<- sim_data(input$nrows,input$noise,input$cat,input$ndist,input$nvar,input$ev,input$weights,input$yint)
  })
  
  equation <- reactive({
    rm(list = ls())
    gc()
    par(mfrow=c(1,1))
    
    linear_eq(input$ev,input$weights,input$yint)
  })
  
  # Show Table
  output$table <- renderTable({
    head(simdata(), n = 10)
  })
  
  output$cplot <- renderPlot({
    corrplot(cor(simdata()[,1:length(names(simdata()))-1]), method="shade",type='lower',tl.col = "black", tl.srt = 45)
  })
  
  # Print Equation
  output$equation <- renderText({
    paste0("y = ",equation())
  })
  
  # Logistic and Random forest simulation
  lr_rf_nvar <- reactive({
    withProgress(message = 'Training Logistic Regression and Random Forest Models', value = 0,
                 run_model <- simulation_nvar(input$n_sim,
                                               input$split,
                                               input$cat,
                                               input$nrows,
                                               input$noise,
                                               input$ndist,
                                               nvar,input$ev,
                                               input$weights,
                                               input$yint,
                                               input$varselect, # Logistic Regression
                                               input$ntree # Random Forest
                 )
    )
  })
  
  # Titles
  output$lr_title_nvar <- renderText({
    paste0( "The table below is the average of running ", input$n_sim, " simulations of logistic regression at 10 levels of variance (variance = 0.5 to 5.0 in increments of 0.5) in the generated data.")
  })
  
  output$rf_title_nvar <- renderText({
    paste0( "The table below is the average of running ", input$n_sim, " simulations of random forest at 10 levels of variance (variance = 0.5 to 5.0 in increments of 0.5) in the generated data.")
  })
  
  # Print LR Matrix
  output$lr_sim_nvar <- renderTable({
    LR_RF <<- lr_rf_nvar()
    LR_RF[1]
  })
  
  # Print RF Matrix
  output$rf_sim_nvar <- renderTable({
    LR_RF[2]
  })
  
  # CASE1 Plots
  output$case1_chart1 <- renderPlot({
    LR1 <<- as.data.frame(LR_RF[1])
    RF1 <<- as.data.frame(LR_RF[2])
    get_line_plots(LR1,RF1,'nVar','TPR')
  })
  
  # CASE1 Plots
  output$case1_chart2 <- renderPlot({
    get_line_plots(LR1,RF1,'nVar','Accuracy')
  })
  
  # CASE1 Plots
  output$case1_chart3 <- renderPlot({
    get_line_plots(LR1,RF1,'nVar','F1')
  })
  
  # CASE1 BOX PLOT
  output$case1_chart4 <- renderPlot({
    df <- AUC
    get_boxplots(df,'nvar')
  })
  

  ####################### Number of noise variables ####################### 
  output$lr_title_num_nvar <- renderText({
    paste0( "The table below displays the results of running logistic regression 10 times with ", input$n_sim, " explanatory variables and 1, 5, 10, 20, and 50 noise variables in the dataset.")
  })
  
  output$rf_title_num_nvar <- renderText({
    paste0( "The table below displays the results of running random forest 10 times with ", input$n_sim, " explanatory variables and 1, 5, 10, 20, and 50 noise variables in the dataset.")
  })
  
  lr_rf_num_nvar <- reactive({
    withProgress(message = 'Training Logistic Regression and Random Forest Models', value = 0,
                 run_model <- simulation_num_nvar(input$n_sim,
                                              input$split,
                                              input$cat,
                                              input$nrows,
                                              inpur$noise,
                                              input$ndist,
                                              input$nvar,
                                              input$ev,
                                              input$weights,
                                              input$yint,
                                              input$varselect, # Logistic Regression
                                              input$ntree # Random Forest
                 )
    )
  })
  
  
  # Print LR Matrix
  output$lr_sim_num_nvar <- renderTable({
    LR_RF2 <<- lr_rf_num_nvar()
    LR_RF2[1]
  })
  
  # Print RF Matrix
  output$rf_sim_num_nvar <- renderTable({
    LR_RF2[2]
  })
  
  # CASE2 Plots
  output$case2_chart1 <- renderPlot({
    LR2 <<- as.data.frame(LR_RF2[1])
    RF2 <<- as.data.frame(LR_RF2[2])
    get_line_plots(LR2,RF2,'Num.Noise','TPR')
  })
  
  # CASE2 Plots
  output$case2_chart2 <- renderPlot({
    get_line_plots(LR2,RF2,'Num.Noise','Accuracy')
  })
  
  # CASE2 Plots
  output$case2_chart3 <- renderPlot({
    get_line_plots(LR2,RF2,'Num.Noise','F1')
  })
  
  # CASE2 BOX PLOT
  output$case2_chart4 <- renderPlot({
    df <- AUC2
    get_boxplots(df,'noise')
  })
  
  
  ####################### Number of explanatory variables ####################### 
  output$lr_title_num_nevar <- renderText({
    paste0( "The table below displays the results of running logistic regression 10 times with ", input$n_sim, " explanatory variables and 1, 5, 10, 20, and 50 noise variables in the dataset.")
  })
  
  output$rf_title_num_nevar <- renderText({
    paste0( "The table below displays the results of running random forest 10 times with ", input$n_sim, " explanatory variables and 1, 5, 10, 20, and 50 noise variables in the dataset.")
  })
  
  lr_rf_num_ev <- reactive({
    withProgress(message = 'Training Logistic Regression and Random Forest Models', value = 0,
                 run_model <- simulation_num_evar(input$n_sim,
                                                  input$split,
                                                  input$cat,
                                                  input$nrows,
                                                  input$noise,
                                                  input$ndist,
                                                  input$nvar,
                                                  input$ev,
                                                  input$weights,
                                                  input$yint,
                                                  input$varselect, # Logistic Regression
                                                  input$ntree # Random Forest
                 )
    )
  })
  
  
  # Print LR Matrix
  output$lr_sim_num_evar <- renderTable({
    LR_RF3 <<- lr_rf_num_ev()
    LR_RF3[1]
  })
  
  # Print RF Matrix
  output$rf_sim_num_evar <- renderTable({
    LR_RF3[2]
  })
  
  # CASE3 Plots
  output$case3_chart1 <- renderPlot({
    LR3 <<- as.data.frame(LR_RF3[1])
    RF3 <<- as.data.frame(LR_RF3[2])
    get_line_plots(LR3,RF3,'num_ev','TPR')
  })
  
  # CASE3 Plots
  output$case3_chart2 <- renderPlot({
    get_line_plots(LR3,RF3,'num_ev','Accuracy')
  })
  
  # CASE3 Plots
  output$case3_chart3 <- renderPlot({
    get_line_plots(LR3,RF3,'num_ev','F1')
  })
  
  # CASE3 BOX PLOT
  output$case3_chart4 <- renderPlot({
    df <- AUC3
    get_boxplots(df,'noise')
  })
}