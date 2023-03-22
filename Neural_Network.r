# This will take the inputs of X_data_2, Y_data_2 and run them through a neural network algorithm to generate a predictive model
# (Note that inputs have already been normalised)
# The model will then need to be tested for accuracy against ML_data_test to generate the neural network AUC

print("Running neural network")


data_frame <- as.data.frame(ML_data_train)

if(!require(glmnet)) {install.packages("glmnet")}
library(glmnet)

data_frame[] <- lapply(data_frame, function(x) {     # unregularised logistic regression; requires data frame; but glmnet is faster

if(is.factor(x)) as.numeric(as.character(x)) else x})

X_data_2 <- data.matrix(subset(data_frame, select = -  c(death_in_6h)))    # this was changed from as.matrix as was getting an error

Y_data_2 <- data.matrix(subset(data_frame, select = c(death_in_6h)))



#options(expressions = 5e5)
#memory.limit(size=5000000)   #prevents stack overflow by limiting memory use

if(!require(neuralnet)) {install.packages("neuralnet")}
library(neuralnet)



n <- cbind(X_data_2, Y_data_2)   #from datascienceplus.com/fitting-neural-network-in-r/

#f <- as.formula(paste("death_in_6h ~", paste(n[!n %in% "death_in_6h"], collapse = " + ")))    #pastes all factors separated by a +
#f <- as.formula(paste("death_in_6h ~", "HR_above + HR_below + RR_above + RR_below + FiO2 + inverse_SATs")) #small scale test

nnames <- colnames(X_data_2)

f <- as.formula(paste("death_in_6h ~", paste(nnames[!nnames %in% "death_in_6h"], collapse = " + ")))

hidden_nn <- c(100, 100)   # HIDDEN LAYERS

nn_model <- neuralnet(f, data=n, hidden=hidden_nn , linear.output=FALSE)

# options(max.print = .Machine$integer.max)

print(nn_model)

print("Plotting network")

#plot(nn_model)

print("Generating predictions in test set")
# see datacamp.com/community/tutorials/neural-network-models-r

nn_test_X <- data.frame(subset(ML_data_test, select = - c(death_in_6h)))
nn_test_Y <- data.frame(ML_data_test[,"death_in_6h"])
nn_test <- cbind(nn_test_X, nn_test_Y)

nn_test[] <- lapply(data_frame, function(x) {   #temporarily replacing nn_test with 
if(is.factor(x)) as.numeric(as.character(x)) else x})

Predict=compute(nn_model, nn_test) #generates predictions as probabilities

prob <- Predict$net.result #extracts predictions only from the results

pred <- ifelse(prob > 0.5, 1, 0) #converts predictions to 1/0 boolean indicators (note that pROC prefers the numeric outcomes from prob)

pred

print("Calculating AUC of neural network")

source("auc_nn.r")


