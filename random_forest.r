# performs random forrest algorithm on ML_data_train, generates predictions on ML_data_test

print("Random forrest initializing")

#if(!require(randomForest)) {install.packages("randomForest")}
#library(randomForest)

if(!require(xgboost)) {install.packages("xgboost")}
library(xgboost)

#if(!require(randomForestSRC)) {install.packages("randomForestSRC")}
#library(randomForestSRC)

#possibly need caTools but probably not
# y will be "death_in_6h"

#rf <- randomForest(num ~ ., data=ML_data_train)

dtrain <- xgb.DMatrix(data = as.matrix(subset(ML_data_train, select = - c(death_in_6h))), label = as.matrix(ML_data_train[,"death_in_6h"]))
# this has separated x and y

bst <- xgboost(data = dtrain, max.depth = 2, eta = 1, nthread = 2, nrounds = 7, objective = "binary:logistic", verbose = 2)
# this has performed the gradient-boosted random forest algorithm

pred = predict(bst, as.matrix(subset(ML_data_test, select = - c(death_in_6h))))
# generated predictions





print("Assessing rf model against test set; calculating AUC")

outcomes <- ML_data_test[,"death_in_6h"]  # outcomes taken as the true outcoms from the validation set
ML_data_test_temp <- subset(ML_data_test, select = - c(death_in_6h))

predictions <- pred

#death_in_6h <- outcomes
#ML_data_test <- ML_data <- cbind(ML_data_test, death_in_6h)
#plot.roc(outcomes, predictions, print.auc=TRUE)

rocobj <- plot.roc(outcomes, predictions,
                   main = paste(nrow(ML_data_train), " training examples,",nrow(ML_data_test), "tests"), 
                   ci = TRUE,                  # compute AUC (of AUC by default)
                   print.auc = TRUE)           # print the AUC (will contain the CI)
ciobj <- ci.se(rocobj,                         # CI of sensitivity
               specificities = seq(0, 1, 0.05)) # over a select set of specificities
plot(ciobj, type = "shape", col = "#1c61b6AA")     # plot as a blue shape
plot(ci(rocobj, of = "thresholds", thresholds = "best")) # add one threshold