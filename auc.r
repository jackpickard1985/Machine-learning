### calculates the auc of the using the coefficients held in 'result' applied to ML_data_test and prints the output
### NOTE: also required pROC package to be installed into r ; this will be done automatically

if(!require(pROC)) {install.packages("pROC")}
library(pROC)

print("Assessing model against test set; calculating AUC")

outcomes <- ML_data_test[,"outcome_column"]  # outcomes taken as the true outcoms from the test set
ML_data_test_temp <- subset(ML_data_test, select = - c(outcome_column))

predictions <- vector(length = nrow(ML_data_test_temp))  # calculate results as predicted by the logistic regression model coefficients
for (y in 1:nrow(ML_data_test_temp)) {
predictions[1] <- coef(result)[1]
for (z in 2:ncol(ML_data_test_temp)) {
predictions[y] <- predictions[y] + (coef(result)[z] * as.numeric(ML_data_test_temp[y, z]))
}
}

#outcome_column <- outcomes
#ML_data_test <- ML_data <- cbind(ML_data_test, outcome_column)
#plot.roc(outcomes, predictions, print.auc=TRUE)

rocobj <- plot.roc(outcomes, predictions,
                   main = paste(nrow(ML_data_train), " training examples,",nrow(ML_data_test), "tests"), 
                   ci = TRUE,                  # compute AUC (of AUC by default)
                   print.auc = TRUE)           # print the AUC (will contain the CI)
ciobj <- ci.se(rocobj,                         # CI of sensitivity
               specificities = seq(0, 1, 0.05)) # over a select set of specificities
plot(ciobj, type = "shape", col = "#1c61b6AA")     # plot as a blue shape
plot(ci(rocobj, of = "thresholds", thresholds = "best")) # add one threshold


# alternate method using ROCR
# df <- data.frame(ROCR.simple)
# pred <- prediction(predictions, outcomes)
# perf <- performance(pred,"tpr","fpr")
# plot(perf,colorize=TRUE)


