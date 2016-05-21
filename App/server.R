# Server
library(shiny)
library(reshape2)
library(ggplot2)
library(plotly)
library(survival)
library(dplyr)
library(nnet)

# This is where the server script starts (where we put together things to be shown in UI)
shinyServer(
  
  function(input, output){
    
    #### Predicted length of time ####
    predictedLength <- reactive({
      paste0("Predicted Decision Time: ", round(predict(survReg, data.frame(Type = input$type, OralArgument = input$oral, 
                                                                            Calabria = ifelse("Calabria" %in% input$judges, 1, 0),
                                                                            Stroud = ifelse("Stroud" %in% input$judges, 1, 0),
                                                                            Geer = ifelse("Geer" %in% input$judges, 1, 0)), type = "response")), " Days")
    })
    
    #For this text to be displayed in UI it needs to be made into a "renderText" object
    output$PredictedLength <- renderText( predictedLength())
    
    
    ### Length plot ###
    plotData <- reactive({
      pct <- 1:98/100   # The 100th percentile of predicted survival is at +infinity ?
      #Generate predictions for each percentile for a given input
      ptime <- predict(survReg, newdata=list(Type = input$type, OralArgument = input$oral,
                                             Calabria = ifelse("Calabria" %in% input$judges, 1, 0),
                                             Stroud = ifelse("Stroud" %in% input$judges, 1, 0),
                                             Geer = ifelse("Geer" %in% input$judges, 1, 0)), type='quantile', p=pct, se=TRUE)
      
      # Visualization of survival time
      plotData <- data.frame(cbind(surv = 1-pct, fit = ptime$fit, sdplus = ptime$fit + 2*ptime$se.fit,
                                   sdminus = ptime$fit - 2*ptime$se.fit))
      plotData <- melt(plotData, id = "surv")
      
      return(plotData)
    })
    
    output$LengthPlot <- renderPlot(
      ggplot(plotData(), aes(x=value, y = surv, colour = variable, linetype = variable)) + geom_line(lwd=1.1) +
        labs(x = "Days Elapsed", y = "Probability Not Decided") +
        scale_colour_manual(values=c("black", "red", "red"), labels = c("Average", "Upper 95% CI", "Lower 95% CI")) + 
        scale_linetype_manual(values = c("solid","dashed","dashed"), labels = c("Average", "Upper 95% CI", "Lower 95% CI")) +
        geom_vline(xintercept = input$greaterThan,lwd=1, colour = "blue") +
        theme(legend.title=element_blank())
    )
    
    # Probability of a case taking longer than the inputted time #
    output$LengthProb <- renderText(
      paste0("There is approximately a ", 
             ifelse( !is.na(round((plotData() %>% mutate(diff = value - input$greaterThan) %>% filter(variable == "fit" & diff > 0) %>% arrange(diff))$surv[1]*100, 2)),
                     round((plotData() %>% mutate(diff = value - input$greaterThan) %>% filter(variable == "fit" & diff > 0) %>% arrange(diff))$surv[1]*100, 2), 0),
             "% chance that the case will be decided after ", input$greaterThan, " days.")
    )
    
    
    ### Predicted Disposition ####
    
    #Disposition prediction
    dispPrediction <- reactive({
      ## Parse entered info into a dataframe
      obs <- data.frame(McGee = 0, Calabria = 0, Type = factor(input$type2, levels = levels(appeals$Type)),
                        OralArgument = factor(input$oral2, levels = levels(appeals$OralArgument)))
      obs$countyGroup <- factor(ifelse(input$county2 %in% countyMeans$county[1:8], "Low", 
                         ifelse(input$county2 %in% countyMeans$county[(nrow(countyMeans)-4):nrow(countyMeans)], "High",
                         "Medium")), levels = levels(appeals$countyGroup))
      obs$McGee <- ifelse("McGee" %in% input$judges2, 1, 0)
      obs$Calabria <- ifelse("Calabria" %in% input$judges2, 1, 0)

      # Make prediction and form the probabilities into a nice table
      prediction <- data.frame(predict(logistic, obs, type = "probs"))
      names(prediction) <- "Probability"
      prediction$Disposition <- row.names(prediction)
      prediction = prediction[order(prediction$Probability), ]
      prediction$ymax = cumsum(prediction$Probability)
      prediction$ymin = c(0, head(prediction$ymax, n=-1))
      prediction$Probability <- paste0(round(prediction$Probability*100,2), " %")
      names(prediction)[1:2] <- c("Likelihood (%)", "Disposition")
      prediction <- prediction[, c(2,1,3,4)]
      
      return(prediction)
    })
    
    output$dispPredTable <- renderTable({dispPrediction()[,1:2]}, include.rownames = F)
    
    # Disposition plot
    output$DispositionPlot <- renderPlot(ggplot(dispPrediction(), aes(fill=Disposition, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
      geom_rect(colour="grey30") +
      coord_polar(theta="y") +
      xlim(c(0, 4)) +
      theme(panel.grid=element_blank()) +
      theme(axis.text=element_blank()) +
      theme(axis.ticks=element_blank()) +
      labs(title="Probability of Dispositions", fill = "Disposition")
    )
    
    output$dissentPrediction <- renderText({
      paste0("There is approximately a ", 
             round(predict(dissentModel, data.frame(OralArgument = input$oral3,
                                                    Dillon = ifelse("Dillon" %in% input$judges3, 1, 0),
                                                    Hunter = ifelse("HunterJr." %in% input$judges3, 1, 0),
                                                    Tyson = ifelse("Tyson" %in% input$judges3, 1, 0)), type = "response")*100, 2),
             "% chance that the case will have a dissenting vote.")
    })
    
    # Dissent plot
    output$DissentPlot <- renderPlot(ggplot(data.frame(Dissent = c("Yes", "No"), 
                                                           Probability = c(predict(dissentModel, data.frame(OralArgument = input$oral3,
                                                                                                 Dillon = ifelse("Dillon" %in% input$judges3, 1, 0),
                                                                                                 Hunter = ifelse("HunterJr." %in% input$judges3, 1, 0),
                                                                                                 Tyson = ifelse("Tyson" %in% input$judges3, 1, 0)), type = "response"),
                                                                           1-predict(dissentModel, data.frame(OralArgument = input$oral3,
                                                                                                   Dillon = ifelse("Dillon" %in% input$judges3, 1, 0),
                                                                                                   Hunter = ifelse("HunterJr." %in% input$judges3, 1, 0),
                                                                                                   Tyson = ifelse("Tyson" %in% input$judges3, 1, 0)), type = "response"))),
                                                aes(fill=Dissent, ymin = c(0, Probability[1]), ymax=cumsum(Probability), xmax=4, xmin = 3)) +
                                       geom_rect(colour="grey30") +
                                       coord_polar(theta="y") +
                                       xlim(c(0, 4)) +
                                       theme(panel.grid=element_blank()) +
                                       theme(axis.text=element_blank()) +
                                       theme(axis.ticks=element_blank()) +
                                       labs(title="Probability of Dissent", fill = "Dissent")
    )
    
  }
)