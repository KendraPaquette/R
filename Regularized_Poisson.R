library(tidyverse)
library(caret)
library(glmnet)
library(lmSupport)
library(doParallel)
library(beepr)
library(ggplot2)


###########################################################
####################Pre-processing#########################
###########################################################
#Data paths------------------------------------------------
DataPath = ''
FeaturePath = ''
ModelPath = ''

#Setup Y--------------------------------------------------
dY = read_rds(file.path(DataPath, ''))

#Setup X--------------------------------------------------
dX = read_rds(file.path(FeaturePath, ''))

#setup parallel processing--------------------------------
nCore =detectCores()
if(nCore>2) {
  cl = makeCluster(nCore-1)
  registerDoParallel(cl)
  getDoParWorkers()
}

#Global pre-processing of X------------------------------
#Stats on X
dim(dX)
names(dX)

#drop features with too many missing values
dX = dX[,tNA/nrow(dX)<.15]
dim(dX)

#Find and remove near zero variance
iNZV = nearZeroVar(dX, foreach = TRUE, allowParallel = TRUE)
length(iNZV)
length(iNZV)/ncol(dX)
names(dX)[iNZV]
if(length(iNZV)>0)  dX = dX[,-iNZV]

#Final setup of Y and X-----------------------------------
X = dX
Y = as.numeric(dY$Score)

###########################################################
###################### Poisson Model ######################
###########################################################
#model-----------------------------------------------------
#control structure
trainCTRL = trainControl(
  method = "repeatedcv", number = 10, repeats=5, 
  savePredictions = TRUE,   allowParallel = TRUE, verbose = TRUE
)

#train model
set.seed(47)
m <- train(x = X, y = Y,
               method = "glmnet", family="poisson",
               preProcess = c("center","scale", 'medianImpute'),
               trControl = trainCTRL,
               tuneLength = 15,
               metric="RMSE"
)
beep()

#Close parallel processing
if(nCore>2) {
  stopCluster(cl)
  remove(cl)
}

#save model
write_rds(m, file.path(ModelPath,''))

#Performance metrics---------------------------------------
#Basic info
dR = as_tibble(m$results)
(MinRMSE = min(dR$RMSE))
(MinLambda = min(dR$lambda))
(MaxLambda = max(dR$lambda))
m$bestTune

#Make data.frame (obs, pred, and response for CV Selection samples only)
SI <- (m$pred$lambda == m$finalModel$tuneValue$lambda) & (m$pred$alpha == m$finalModel$tuneValue$alpha)
obs <- m$pred$obs[SI]
pred <- m$pred$pred[SI]
dCV <- data.frame(obs=obs,link=pred, response=exp(pred))

###########################################################
################### Data Visualization ####################
###########################################################
quartz() 
#scatterplots---------------------------------------------
#observed vs predicted 
ggplot(dCV, aes(x = obs, y = response)) + geom_point(position = "jitter") +  
  labs(title = "Observed and Predicted Symptom Counts", x = "Observed Symptom Count",
       y = "Predicted Symptom Count") + geom_smooth(method=lm) + theme_classic()

#separate by severity
dCV$severity <- NA
dCV$severity[dCV$obs<2] <- "No AUD"
dCV$severity[dCV$obs>1 & dCV$obs<4] <- "Mild"
dCV$severity[dCV$obs>3 & dCV$obs<6] <- "Moderate"
dCV$severity[dCV$obs>5] <- "Severe"

#model performance by severity
ggplot(dCV, aes(x = obs, y = response, color = severity)) + geom_point(position = "jitter") + 
  labs(title = "Observed and Predicted Symptom Counts", x = "Observed Symptom Count",
       y = "Predicted Symptom Count") + geom_smooth(method=lm, se=FALSE) + theme_classic()
