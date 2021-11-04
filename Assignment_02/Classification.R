# Import Libraries

library(finalfit)
library(caTools)
library(rpart)        # direct engine for decision tree application
library(rpart.plot)   # for plotting decision trees
library(caret)        # meta engine for decision tree application
library(ggplot2)      # for awesome plotting
library(corrplot)     # plot correlation matrix using Heatmap
library(tune)         # For hyperparemeter tuning
library(workflows)    # streamline process
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(ROSE)
############################################################################
# Read Dataset
churn_df <-read.csv("/media/shehata/Data/R_Assignment_02/Datasets/Churn_Dataset.csv", stringsAsFactors = TRUE)
View(churn_df)

###################################### 1 #####################################
# Ensure data is in the correct format for downstream processes
# and address missing data 

# Check severity of class imbalance
round(prop.table(table(churn_df$Churn)), 2)

# Check missing values
anyNA(churn_df)
sum(is.na(churn_df))

# find columns with NA
list_na <- colnames(churn_df)[apply(churn_df, 2, anyNA) ]
list_na

# Remove Missing values
clean_churn <- churn_df %>%
  na.omit()

anyNA(clean_churn)
sum(is.na(clean_churn))

# Drop customerID columns from dataset
clean_churn <- subset(clean_churn, select = -c(customerID))
View(clean_churn)

# Convert categorical variables
clean_churn <- sapply(clean_churn, unclass)         
View(clean_churn)

churn_data_df <- subset(clean_churn, select = -Churn)

###################################### 2 #####################################
# Generate a scatterplot matrix to show the relationships between the variables
# and a correlation matrix to determine correlated attributes

### Scatter Matrix
pairs(churn_data_df[,1:6], pch = 19, lower.panel = NULL)
pairs(churn_data_df[,7:11], pch = 19, lower.panel = NULL)
pairs(churn_data_df[,12:19], pch = 19, lower.panel = NULL)

### Correlation matrix
#calculate correlation matrix
correlationMatrix <- cor(churn_data_df)

# summarize the correlation matrix
print(correlationMatrix)

# find attributes that are highly corrected (ideally > 0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)

# print indexes of highly correlated attributes
print(highlyCorrelated) # 19

corrplot(cor(churn_data_df[,2:19]),        # Correlation matrix
         method = "circle",                # Correlation plot method (method = number, circle, pie, or color)
         type = "full",                   # Correlation plot style (also "upper" and "lower")
         diag = TRUE,                     # If TRUE (default), adds the diagonal
         tl.col = "black",                # Labels color
         bg = "white",                    # Background color
         title = "",                      # Main title
         col = NULL,                      # Color palette
         tl.cex =0.7,
         cl.ratio =0.2)   

###################################### 3 #####################################
# Split the dataset into 80 training /20 test set
# and fit a decision tree to the training data.
# Plot the tree,
# and interpret the results.

# 3.1 80% of the sample size
smp_size <- floor(0.80 * nrow(clean_churn))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(clean_churn)), size = smp_size)

train1 <- clean_churn[train_ind, ] # 5625 row
test1  <- clean_churn[-train_ind, ] # 1407 row

# convert matrix to dataframe for model
train_df1 <- as.data.frame(train1)
test_df1 <- as.data.frame(test1)

# 3.2 rpart(traget , data, method)
#specify method as class since we are dealing with classification
DT_model1 <- rpart(Churn ~ .,
                   data = train_df1,
                   method = "class") 
DT_model1

# 3.3 plot the DT model
rpart.plot(DT_model1)
plotcp(DT_model1)

# Select features by checking feature importance
# use the varImp() function to determine how much predictive power lies in each feature
importances1 <- varImp(DT_model1, scale=FALSE) 
importances1 %>% arrange(desc(Overall))
# plot importance
plot(importances1)

#Make predictions
y_pred_dt1 <- predict(DT_model1,
                     newdata = test_df1,
                     type = "class") #use the predict() function and pass in the testing subset
y_pred_dt1
# Accuracy : 0.8088  

#Print the confusion Matrix
cm1 <- confusionMatrix(as.factor(test_df1$Churn), factor(y_pred_dt1), mode = "prec_recall", dnn = c("Actual", "Prediction"))
cm1

plot_confusionMatrix <- function(cm) {
  plt <- as.data.frame(cm$table)
  plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))
  
  ggplot(plt, aes(Actual , Prediction, fill= Freq)) +
    geom_tile() + geom_text(aes(label=Freq)) +
    scale_fill_gradient(low="white", high="#009194") +
    labs(x = "Prediction",y = "Actual") +
    scale_x_discrete(labels=c("Class_1","Class_2")) +
    scale_y_discrete(labels=c("Class_2","Class_1"))
}

plot_confusionMatrix(cm1)

# ROC and AUC
ROSE::roc.curve(test_df1$Churn, y_pred_dt1)
# (AUC): 0.690

###################################### 4 #####################################
# Describe the first few splits in the decision tree. Extract some rules.

###################################### 5 #####################################
# Try different ways to improve your decision tree algorithm 
# (e.g., use different splitting strategies,prune tree after splitting).
# Does pruning the tree improves the accuracy? 

## 5.1 Try different ways to improve your decision tree algorithm 

######## Way 01 to improve decision tree accuracy
important_var_df <- clean_churn[ , c('tenure',
                                     'Contract',
                                     'TechSupport',
                                     'OnlineSecurity',
                                     'MonthlyCharges',
                                     'OnlineBackup',
                                     'InternetService',
                                     'TotalCharges',
                                     'Churn')]
View(important_var_df)

set.seed(101) 
# Now Selecting 80% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(important_var_df),
                     size = floor(.80*nrow(important_var_df)),
                     replace = F)
train2 <- important_var_df[sample, ]
test2  <- important_var_df[-sample, ]

train_df2 <- as.data.frame(train2)
test_df2 <- as.data.frame(test2)

# specify method as class since we are dealing with classification
DT_model2 <- rpart(Churn ~ .,
                   data = train_df2,
                   method = "class",
                   parms = list(split = 'gini'),
                   control = rpart.control(cp = 0.001,
                                           maxdepth = 6,
                                           minsplit = 2))
DT_model2 

# plot the DT model
rpart.plot(DT_model2)
plotcp(DT_model2)

# Select features by checking feature importance
#use the varImp() function to determine how much predictive power lies in each feature
importances2 <- varImp(DT_model2, scale=FALSE) 
importances2 %>% arrange(desc(Overall))

# plot importance
plot(importances2)

#Make predictions
#use the predict() function and pass in the testing subset
y_pred_dt2 <- predict(DT_model2,
                      newdata = test_df2,
                      type = "class")
y_pred_dt2
# Accuracy : 0.796 

# Print the confusion Matrix
cm2 <- confusionMatrix(as.factor(test_df2$Churn), factor(y_pred_dt2),mode = "prec_recall",  dnn = c("Actual", "Prediction"))
cm2

plot_confusionMatrix(cm2)

# ROC and AUC
ROSE::roc.curve(test_df2$Churn, y_pred_dt2) 
# (AUC): 0.678

######## Way 02 to improve decision tree accuracy  (split by information)
ctrl <- trainControl(method = "cv", number = 10, repeats = 3)

DT_model3 <- rpart(Churn ~ .,
                   data = train_df2,
                   method = "class",
                   minsplit = 3, 
                   maxdepth = 6,
                   minbucket = 1,
                   #specify method as class since we are dealing
                   # with classification
                   parms = list(split = 'information'),
                   trControl = ctrl,
                   tuneLength = 10) 
DT_model3

rpart.plot(DT_model3)

y_pred_dt3 <- predict(DT_model3,
                      newdata = test_df2,
                      type = "class")
y_pred_dt3

# Print the confusion Matrix
cm3 <- confusionMatrix(as.factor(test_df2$Churn), factor(y_pred_dt3), mode = "prec_recall",  dnn = c("Actual", "Prediction"))
cm3 # Accuracy : 0.7964
plot_confusionMatrix(cm3)

# ROC and AUC
ROSE::roc.curve(test_df2$Churn, y_pred_dt3)
# (AUC): 0.697

###################################### 6 #####################################
# Train an XGboost model using 10-fold cross-validation repeated 3 times
# and a hyperparameter grid search to train the optimal model. Evaluate the performance

library(xgboost)      # For XGBoost Model 
# install.packages("themis")
library(recipes)
library(themis)

# 6.1 Convert the training and testing sets into Matrix
X_train = as.matrix(train_df1 %>% select(-Churn))
# y_train = as.matrix(train_df1$Churn)
y_train <- factor(train_df1$Churn, labels = c("2", "1"))

X_test = as.matrix(test_df1 %>% select(-Churn))
y_test = factor(test_df1$Churn, labels = c("2", "1"))

# 6.2 Specify cross-validation method and number of folds
xgb_trcontrol = trainControl( method = "cv",
                              number = 10,
                              repeats = 3,
                              allowParallel = TRUE,
                              verboseIter = FALSE,
                              returnData = FALSE)

# 6.3 grid space to search for the best hyperparameters
xgb_grid <- expand.grid(gamma = c(1, 0),
                        nrounds = c(100,200),  # n_estimators 
                        max_depth = c(5, 10, 15, 20, 25),
                        colsample_bytree = seq(0.5, 0.9, length.out = 5),
                        eta = c(0.1, 0.01, 0.001, 0.0001),
                        min_child_weight = 1,
                        subsample = 1)
 
# 6.4 train your model                      
set.seed(0) 
xgb_model = train(x= X_train,
                  y= y_train,
                  trControl = xgb_trcontrol,
                  tuneGrid = xgb_grid,
                  method = "xgbTree")
                 
# 6.5 Best values for hyperparameters
xgb_model$bestTune

y_pred_xgboost <- predict(xgb_model, newdata= X_test)
y_pred_xgboost

# Print the confusion Matrix
cm_xgboost <- confusionMatrix(as.factor(y_test), factor(y_pred_xgboost),  mode = "prec_recall", dnn = c("Actual", "Prediction"))
cm_xgboost # Accuracy : 0.823   
plot_confusionMatrix(cm_xgboost)

y_pred_xgboost_nm = as.numeric(y_pred_xgboost)
y_test_nm = as.numeric(y_test)
class(y_test_nm)

# ROC and AUC
ROSE::roc.curve(y_test_nm, y_pred_xgboost_nm)
# (AUC): 0.711

###################################### 7 #####################################
# 7.1 load library
# install.packages("kerasR")
library(neuralnet)
library(keras)
library(mlbench)
library(dplyr)
library(magrittr)
library(kerasR)
library(tensorflow)

########### Way 1 ###########
# 7.2 Split data to train and test
set.seed(123)
ind1 <- sample(2, nrow(important_var_df), replace = T, prob = c(.8, .2))
X_train1 <- important_var_df[ind1 == 1,1:8]
X_test1 <- important_var_df[ind1  == 2, 1:8]
y_train1 <- important_var_df[ind1 == 1, 9]
y_test1 <- important_var_df[ind1  == 2, 9]
View(X_train1)

# 7.3 Scaling
m1 <- colMeans(X_train1)
s1 <- apply(X_train1, 2, sd)
X_train1 <- scale(X_train1, center = m1, scale = s1)
X_test1 <- scale(X_test1, center = m1, scale = s1)

# 7.4 Model Creation
model1 <- keras_model_sequential()
model1 %>%
  layer_dense(units = 5, activation = 'relu', input_shape = c(8)) %>%
  layer_dropout(rate=0.2)  %>%
  layer_dense(units = 1)

# 7.5 Model Compilation
model1 %>% compile(loss = 'mse',
                  optimizer = 'rmsprop', 
                  metrics = 'accuracy')

# 7.6 Model Training
nn_model1 <- model1 %>%          
  fit(X_train1,
      y_train1,
      epochs = 30,
      batch_size = 32,
      validation_split = 0.2)

# 7.7 Prediction
model1 %>% evaluate(X_test1, y_test1)
nn_pred1 <- model1 %>% predict(X_test1)
nn_pred1
table(y_test1, round(nn_pred1))

# 7.8 Scatter Plot Original vs Predicted
plot(y_test1, nn_pred1) 

# Confusion Matrix
cm_nn1 <- confusionMatrix(as.factor(y_test1), factor(round(nn_pred1)),  mode = "prec_recall", dnn = c("Actual", "Prediction"))
cm_nn1 # Accuracy : 0.7699    
plot_confusionMatrix(cm_nn1)

# ROC and AUC
ROSE::roc.curve(y_test1, nn_pred1)
# (AUC): 0.810

# 7.9 Save model
keras_save(model1, "nn_model1.h5")
keras_save_weights(model1, "nn_model1_model.h5")
keras_model_to_json(model1, "nn_model1_architecture.json")

########### Way 2 ###########

set.seed(2021)
ind2 <- sample(2, nrow(clean_churn), replace = T, prob = c(.8, .2))
X_train2 <- clean_churn[ind2==1,1:19]
X_test2 <- clean_churn[ind2==2, 1:19]
y_train2 <- clean_churn[ind2==1, 20]
y_test2 <- clean_churn[ind2==2, 20]

normalize<-function(x) {
  y<-(x - mean(x)) / sd(x)
  return(y)
}
# 7.3 Normalization
X_train_norm <-apply(X_train2, 2, normalize)
X_test_norm <-apply(X_test2, 2, normalize)

# 7.4 Model Creation
model2 <- keras_model_sequential()
model2 %>%
  layer_dense(units = 5,
              activation = 'tanh',
              input_shape = dim(X_train_norm)[2]) %>%
  layer_dropout(rate=0.2)  %>%
  layer_dense(units = 1)

# 7.5 Model Compilation
model2 %>% compile(loss = 'mean_squared_error',
                   optimizer = 'adam', 
                   metrics = c('mean_absolute_error','mean_squared_error'))

# 7.6 Model Training
nn_model2 <- model2 %>%          
  fit(X_train_norm,
      y_train2,
      epochs = 15,
      batch_size = 32,
      verbose = 1,
      validation_split = 0.3)

# 7.7 Prediction
model2 %>% evaluate(X_test_norm, y_test2)
nn_pred2 <- model2 %>% predict(X_test_norm)
nn_pred2
table(y_test2, round(nn_pred2))

# 7.8 Scatter Plot Original vs Predicted
plot(y_test2, nn_pred2) 

# Confusion Matrix
cm_nn2 <- confusionMatrix(as.factor(y_test2),
                          factor(round(nn_pred2)),
                          mode = "prec_recall",
                          dnn = c("Actual", "Prediction"))
cm_nn2 # Accuracy : 0.7934      
plot_confusionMatrix(cm_nn2)

# ROC and AUC
ROSE::roc.curve(y_test3, nn_pred2)
# (AUC): 0.830


# 7.9 Save model
keras_save(model2, "nn_model2.h5")
keras_save_weights(model2, "nn_model2.h5")
keras_model_to_json(model2, "nn_model2_architecture.json")
