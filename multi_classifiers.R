
library(tidyverse)
library(caret)
library(ranger)
library(rpart)
library(rpart.plot)
library(e1071)
library(kernlab)
library(nnet)
library(BT)
library(ROCR)
library(partykit)
library(MLeval)
library(pROC)
library(ModelMetrics)


df <- read.csv("selected_data.csv")

df$PotentialFraud <- as.factor(df$PotentialFraud)

######################################################
###### Feature selection
# Using RFE method & Random Forest algorithm
fselect_control <- rfeControl(functions=rfFuncs, method="cv", number=5)
set.seed(123)
fselect_results <- rfe(df[,1:32], 
                       df$PotentialFraud, 
                       sizes=c(5:10,12,14,16), 
                       rfeControl=fselect_control)

# Summarise & plot the results & list the chosen features
print(fselect_results)
predictors(fselect_results)
plot(fselect_results, type=c("g", "o"))

# Extract features
# fselect_cols <- predictors(fselect_results)
df_fselect <- df %>% select(all_of(predictors(fselect_results)), PotentialFraud)

# Rename the factor levels to valid R variable names
levels(df_fselect$PotentialFraud) <- make.names(levels(df_fselect$PotentialFraud))
# df_fselect <- df


##### Standardise data
df_scaled <- df_fselect[ , 1:7] %>% scale()
df_scaled <- cbind(as.data.frame(df_scaled), PotentialFraud = df_fselect$PotentialFraud)

# df_test <- scale(df$Count_OP_CLaims)



##### Partitioning dataset
set.seed(998)
index_train <- createDataPartition(df_scaled$PotentialFraud, p = .8, list = FALSE)
df_train <- df_scaled[index_train, ]
df_test  <- df_scaled[-index_train, ]


######################################################
###### Parameter tuning & model building

# Random forest
ctr_rf <- trainControl(
  method = "repeatedcv",
  number = 5,  repeats = 10,
  # adaptive = list(min =3,                 # minimum number of resamples per hyperparameter
  #                 alpha =0.05,            # Confidence level for removing hyperparameters
  #                 method = "BT",          # Bradly-Terry Resampling method (here you can instead also use "gls")
  #                 complete = FALSE),      # If TRUE a full resampling set will be generated 
  search = "random",
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  sampling = "up")

set.seed(825)
model_rf <- train(PotentialFraud ~ ., 
                  data = df_train, 
                  metric = "ROC",
                  method = "ranger", 
                  importance = "impurity", # Add this value
                  trControl = ctr_rf, 
                  tuneLength = 30)
ggplot(model_rf) + theme(legend.position = "top")

varimp_rf <- importance(model_rf$finalModel)


# Decision tree
set.seed(825)
model_ctree <- train(PotentialFraud ~ ., 
                  data = df_train, 
                  metric = "ROC",
                  method = "rpart",
                  trControl = ctr_rf, 
                  tuneLength = 30)
ggplot(model_ctree) + theme(legend.position = "top")

# SVM Support Vector Machine
# set.seed(825)
# model_svm <- train(PotentialFraud ~ ., 
#                      data = df_train, 
#                      metric = "ROC",
#                      method = "svmRadial",
#                      trControl = ctr_rf, 
#                      tuneLength = 10)
# ggplot(model_svm) + theme(legend.position = "top")

set.seed(825)
model_svm <- train(PotentialFraud ~ ., 
                   data = df_train, 
                   metric = "ROC",
                   method = "svmPoly",
                   trControl = ctr_rf, 
                   tuneLength = 10)
ggplot(model_svm) + theme(legend.position = "top")

# set.seed(825)
# model_svm_linear <- tune(svm, PotentialFraud ~ ., data=df_train, 
#                              tunecontrol = tune.control(random=10, sampling="cross", cross=5))
  

# Neural network
set.seed(825)
model_ann <- train(PotentialFraud ~ ., 
                   data = df_train, 
                   metric = "ROC",
                   method = "nnet",
                   trControl = ctr_rf, 
                   tuneLength = 10)
ggplot(model_ann) + theme(legend.position = "top")


######################################################

###### Prediction deploying
# Predictions & confusion matrix
predict_ctree <- predict.train(object=model_ctree, df_test[, -which(names(df_test) == "PotentialFraud")], type="raw")
cm_ctree <- confusionMatrix(predict_ctree,df_test$PotentialFraud)

predict_svm <- predict.train(object=model_svm, df_test[, -which(names(df_test) == "PotentialFraud")], type="raw")
cm_svm <- confusionMatrix(predict_svm,df_test$PotentialFraud)

predict_ann <- predict.train(object=model_ann, df_test[, -which(names(df_test) == "PotentialFraud")], type="raw")
cm_ann <- confusionMatrix(predict_ann,df_test$PotentialFraud)

predict_rf <- predict.train(object=model_rf, df_test[, -which(names(df_test) == "PotentialFraud")], type="raw")
cm_rf <- confusionMatrix(predict_rf,df_test$PotentialFraud)

# Extract metrics from confusion matrix
cm_ctree[["overall"]]
cm_svm[["overall"]]
cm_ann[["overall"]]
cm_rf[["overall"]]

cm_ctree[["byClass"]]
cm_svm[["byClass"]]
cm_ann[["byClass"]]
cm_rf[["byClass"]]


# Calculating and plotting ROC
predict_prob_ctree <- predict(model_ctree, newdata = df_test, type = "prob")[,2] 
roc_ctree <- roc(df_test$PotentialFraud, predict_prob_ctree)
plot.roc(roc_ctree, print.auc = TRUE, main = "ROC Curve for Decision Tree model")

predict_prob_svm <- predict(model_svm, newdata = df_test, type = "prob")[,2] 
roc_svm <- roc(df_test$PotentialFraud, predict_prob_svm)
plot.roc(roc_svm, print.auc = TRUE, main = "ROC Curve for SVM model")

predict_prob_ann <- predict(model_ann, newdata = df_test, type = "prob")[,2] 
roc_ann <- roc(df_test$PotentialFraud, predict_prob_ann)
plot.roc(roc_ann, print.auc = TRUE, main = "ROC Curve for Neural Network model")

predict_prob_rf <- predict(model_rf, newdata = df_test, type = "prob")[,2] 
roc_rf <- roc(df_test$PotentialFraud, predict_prob_rf)
plot.roc(roc_rf, print.auc = TRUE, main = "ROC Curve for Random Forest model")

