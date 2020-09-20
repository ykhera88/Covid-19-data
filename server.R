#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


shinyServer(function(input, output, session) {
  
  output$Assignment2rawdatatable <- DT::renderDataTable({
    DT::datatable(data = as.data.frame(assignment2rawdata))
  })
  output$summaryassignment2rawdata <- renderUI({
    view(dfSummary(assignment2rawdata), omit.headings = FALSE, method = 'render',footnote = NA)
  })
  output$Assignment2datatable <- DT::renderDataTable({
    DT::datatable(data = as.data.frame(assignment2data))
  })
  output$summaryassignment2data <- renderUI({
    view(dfSummary(assignment2data), omit.headings = FALSE, method = 'render',footnote = NA)
  })
  output$Missing_overall <- renderPlot({
    vis_miss(assignment2data,cluster = input$cluster) +
      labs(title = "Missingness of data")
  })
  output$Missing <- renderPlot({
    visdat::vis_dat(assignment2data) +
      labs(title = "Missingness of data by variable type")
  })
  output$Missingpattern <- renderPlot({
    naniar::gg_miss_upset(assignment2ggupset,nsets=14) 
      
  })
   output$missingcorrgram <-renderPlot({
     m <- is.na(assignment2ggupset) + 0
     cm <- colMeans(m)
     m <- m[, cm > 0 & cm < 1, drop = FALSE]
     corrgram::corrgram(cor(m), order = "OLO", abs = TRUE,cor.method = input$CorrMeth)
     title(main = "Variable missing value correlation",
           sub = "Notice whether variables are missing in sets")
   })
   
  output$variablesremove <- renderText({
    pMiss <- function(x){ sum(is.na(x))/length(x)*100 }
    threshold <- input$VarThresh
    d <- assignment2ggupset
    cRatio <- apply(X = d, MARGIN = 2, FUN = pMiss) # run pMiss for each column of the data frame
    paste("Variables to remove:", paste(colnames(d)[cRatio >= threshold], collapse = ","))
  })
 
  output$rf<- renderPlot({
    assignment2ggupset.rf <- randomForest(DEATHRATE ~.-COUNTRY, data=assignment2ggupset, ntree=1000,
                                          keep.forest=FALSE, importance=TRUE,na.action = na.exclude)
    varImpPlot(assignment2ggupset.rf)
  })
  output$variableimportance <- renderPrint({
    assignment2ggupset.rf <- randomForest(DEATHRATE ~.-COUNTRY, data=assignment2ggupset, ntree=1000,
                                          keep.forest=FALSE, importance=TRUE,na.action = na.exclude)
    importance(assignment2ggupset.rf)
  })
  output$variablesremovefinal <- renderText({
    pMiss <- function(x){ sum(is.na(x))/length(x)*100 }
    threshold <- input$VarThreshfinal
    f <- assignment2
    cRatio <- apply(X = f, MARGIN = 2, FUN = pMiss) # run pMiss for each column of the data frame
    paste("Variables to remove:", paste(colnames(f)[cRatio >= threshold], collapse = ","))
  })
  output$observationremove <- renderText({
    pMiss <- function(x){ sum(is.na(x))/length(x)*100 }
    threshold <- input$obsThresh
    e <- assignment2
    cRatio <- apply(X = e, MARGIN = 1, FUN = pMiss) # run pMiss for each column of the data frame
    paste("obs to remove:", paste(rownames(e)[cRatio >= threshold], collapse = ","))
  })
  
  output$observationremovefinal <- renderText({
    pMiss <- function(x){ sum(is.na(x))/length(x)*100 }
    threshold <- input$obsThreshfinal
    g <- assignment2a
    cRatio <- apply(X = g, MARGIN = 1, FUN = pMiss) # run pMiss for each column of the data frame
    paste("obs to remove:", paste(rownames(g)[cRatio >= threshold], collapse = ","))
    
  })
  output$summary  <- renderUI({
    view(dfSummary(assignment2a), omit.headings = FALSE, method = 'render',footnote = NA)
  })
  output$Missing_overall2 <- renderPlot({
    vis_miss(assignment2a,cluster = input$cluster) +
      labs(title = "Missingness of data")
  })
  output$treeplot <- renderPlot({
    assignment2a$MISSINGNESS <- apply(X = is.na(assignment2a), MARGIN = 1, FUN = sum)
    tree <- train(MISSINGNESS ~ . -COUNTRY, data = assignment2a, method = "rpart", na.action = na.rpart)
    rpart.plot(tree$finalModel, main = "TUNED: Predicting the number of missing variables in an observation", roundint = TRUE, clip.facs = TRUE)
  })
  output$Pairs <- renderPlot({
    GGally::ggpairs(data = assignment2a [input$select],mapping = aes(colour = GOVERNMENT), title = "Pairs of numeric and categorical data")
  })
  output$Boxplot <- renderPlot({
    car::Boxplot(assignment2anumerical[,input$select2], range = 1.5, notch = FALSE, col = "blue",main = "Box Plot showing numeric variables",na.rm=TRUE)
    
  })
  
  output$Assignment2TRAIN <- DT::renderDataTable({
    set.seed(10)
    subIndex <- caret::createDataPartition(y = assignment2a$DEATHRATE, p = 0.7, list = FALSE)
    assignment2.train <<- assignment2a[subIndex,]
    assignment2.test <<- assignment2a[-subIndex,]
    DT::datatable(data = as.data.frame(assignment2.train))
  })
  
  output$Assignment2TEST <- DT::renderDataTable({
    DT::datatable(data = as.data.frame(assignment2.test))
  })
  
  output$Assignment2RECIPE <- renderPrint({
    rec <- recipes::recipe(DEATHRATE ~., data = assignment2a) %>%
      update_role("COUNTRY", new_role = "id") %>% #id is not a predictor
      step_knnimpute(all_predictors(), neighbours = 5) %>%
      step_center(all_numeric(), -has_role("outcome")) %>%
      step_scale(all_numeric(), -has_role("outcome")) %>%
      step_dummy(all_nominal())
    model1 <- caret::train(rec, data = assignment2.train, method = "glmnet")
    yhat <<- predict(model1, newdata = assignment2.test)
    print(yhat)
    })
     output$RMSE <- renderPrint({
       caret::RMSE(yhat, assignment2.test$DEATHRATE, na.rm = TRUE)
     })
  
      output$plot1 <- renderPlot({
        rang <- range(c(assignment2.test$DEATHRATE, yhat))
          ggplot(data = assignment2.test) +
            geom_point(mapping = aes(x = assignment2.test$DEATHRATE, y = yhat)) +
            geom_abline(slope = 1, col = "blue") +
            labs(title = "Deathrate Prediction", y = "Predicted", x = "Actual") +
            coord_fixed(ratio = 1, xlim = rang, ylim = rang, expand = TRUE)
           })
      output$residualplot <- renderPlot({
        coef <- input$range_residual
        assignment2.test$Residual <- assignment2.test$DEATHRATE-yhat
        limits <- boxplot.stats(x = assignment2.test$Residual, coef = coef)$stats
        assignment2.test$label <- ifelse(assignment2.test$Residual < limits[1] | assignment2.test$Residual > limits[5], rownames(assignment2.test), NA)
        ggplot(assignment2.test, mapping = aes(x = Residual, y = 0, label = label),) +
          geom_boxplot(coef = coef) +
          ggrepel::geom_text_repel() +
          labs(title = paste("Residuals Boxplot using", coef, "as IQR Multplier")) +
          theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
      })
      })
  
  




