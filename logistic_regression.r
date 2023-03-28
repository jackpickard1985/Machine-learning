if (run_log == TRUE){

print("Performing logistic regression on the training set")

data_frame <- as.data.frame(ML_data_train)



if(!require(glmnet)) {install.packages("glmnet")}
library(glmnet)

data_frame[] <- lapply(data_frame, function(x) {

if(is.factor(x)) as.numeric(as.character(x)) else x})

X_data_2 <- data.matrix(subset(data_frame, select = -  c(outcome_column)))    # changed from as.matrix to avoid error

Y_data_2 <- data.matrix(subset(data_frame, select = c(outcome_column)))



# Find the best lambda using cross-validation 

print("Optimising lambda constant")

set.seed(12345) # random initalisation

cvfit <- cv.glmnet(X_data_2, Y_data_2, alpha = 1, family = "binomial")     # alpha: o for ridge regression, 1 for LASSO regression

# Fit the final model on the training data 

result <- glmnet(X_data_2, Y_data_2, alpha = 1, family = "binomial", lambda = cvfit$lambda.min)  # alpha: o for ridge regression, 1 for LASSO regression

print(paste("Lambda =", cvfit$lambda.min))

print(summary(result))

print(coef(result)) # Display regression coefficients




print("calculating AUC when model is applied to test set.")

source("auc.r")

}
