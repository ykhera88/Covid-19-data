library(shiny)
library(shinyjs)
library(vcd)
library(MASS)
library(RColorBrewer)
library(datasets)
library(corrgram)
library(visdat)
library(forecast)
library(tidytext)
library(tidyverse)
library(janeaustenr)
library(stringr)
library(wordcloud2)
library(reshape2)
library(pls)
library(ggplot2)
library(naniar)
library(lubridate)
library(shinycssloaders)
library(rpart)
library(rpart.plot)
library(carData)
library(missForest)
library(caret)
library(recipes)
library(Hmisc)
library(summarytools)

#Loading the raw data
assignment2rawdata <- read.csv("Ass2Data.csv", header = TRUE)
#processing the data
assignment2data <- read.csv("Ass2Data.csv", header = TRUE)
#replacing -99,--, blanks with NA
assignment2data[assignment2data == -99] <- NA
assignment2data[assignment2data =="--" ] <- NA
assignment2data[assignment2data ==" " ] <- NA
#assigning 0 to healthcare cost if healthcare basis free
assignment2data[!is.na(assignment2data$HEALTHCARE_BASIS) & assignment2data$HEALTHCARE_BASIS=="FREE", "HEALTHCARE_COST"] <- 0 
assignment2ggupset <- assignment2data[c(1:14)]
#removing excessive missing variable age median
assignment2 <- assignment2ggupset[c(1:4,6:14)]
#removing excessive missing rows
assignment2a <- slice(assignment2,1:27,29:75,77:117,119:167,169:190)
assignment2colnames <- colnames(assignment2a)
assignment2anumerical <- assignment2a[c(3:10,12,13)]
assignment2numericalcolnames <- colnames(assignment2anumerical)


#train and test split
set.seed(10)
subIndex <- caret::createDataPartition(y = assignment2a$DEATHRATE, p = 0.7, list = FALSE)
assignment2.train <<- assignment2a[subIndex,]
assignment2.test <<- assignment2a[-subIndex,] 
#recipe based processing pipeline
rec <- recipes::recipe(DEATHRATE ~., data = assignment2a) %>%
  update_role("COUNTRY", new_role = "id") %>% #id is not a predictor
  step_knnimpute(all_predictors(), neighbours = 5) %>%
  step_center(all_numeric(), -has_role("outcome")) %>%
  step_scale(all_numeric(), -has_role("outcome")) %>%
  step_dummy(all_nominal())
#model
 model1 <- caret::train(rec, data = assignment2.train, method = "glmnet")
 #prediction
 yhat <- predict(model1, newdata = assignment2.test)


