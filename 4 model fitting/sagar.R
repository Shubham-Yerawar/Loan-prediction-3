setwd("H://projects//Analytix Vidhya//loan prediction 3//4 model fitting/")
loantrain <- read.csv(file="train_set.csv", header=TRUE,as.is=T)
loantest <- read.csv(file="test_set.csv", header=TRUE,as.is=T)

#summary(loantrain)
#summary(loantest)
 

col_numeric <- c(2:6)
for(i in col_numeric){
  loantrain[,i] <- as.numeric(loantrain[,i])
  loantest[,i] <- as.numeric(loantest[,i])
}

col_factor <- c(7,8)
for(i in col_factor){
  loantrain[,i] <- as.factor(loantrain[,i])
  loantest[,i] <- as.factor(loantest[,i])
}


##########-------------svm-------------------#########################
library(e1071)

tuned_parameters <- tune.svm(Loan_Status~.-Loan_ID, data = loantrain, gamma = 10^(-5:-1), cost = 10^(-3:1))
summary(tuned_parameters)
model <- tuned_parameters$best.model

my_prediction <- predict(model, loantest)
my_prediction <- ifelse(my_prediction > 0.9,1,0)
my_prediction
misClasificError <- mean(my_prediction != loantest$Loan_Status)
print(paste('Accuracy',1-misClasificError))

#confusion matrix
table(my_prediction, loantest$Loan_Status)

#####################################################################

loantrain$Loan_Status <- factor(loantrain$Loan_Status, levels = c("N","Y"), labels = c("0","1"))
loantrain$Loan_Status <- as.factor(loantrain$Loan_Status)
loantrain$Loan_Status <- as.numeric(loantrain$Loan_Status)
loantrain$Loan_Status <- loantrain$Loan_Status -1
loantrain$Loan_Status
#class(loantest$Loan_Status)

loantest$Loan_Status <- factor(loantest$Loan_Status , levels = c("N","Y"), labels = c("0","1"))
loantest$Loan_Status <- as.factor(loantest$Loan_Status)
loantest$Loan_Status <- as.numeric(loantest$Loan_Status)
loantest$Loan_Status <- loantest$Loan_Status -1
loantest$Loan_Status

###################

library(wSVM)
X <- as.matrix(loantrain[c(2:6)])             # 7 is omitted for now but shud b considered
Y <- as.matrix(loantrain$Loan_Status)
new.X <-as.matrix(loantest[c(2:6)]) 
new.Y <- as.matrix(loantest$Loan_Status)
tmp_new.Y <- t(new.Y)


model <- wsvm(X, Y, c.n = rep(1/ length(Y),length(Y)))


# predict the model and compute an error rate. 
pred <- wsvm.predict(X,Y, new.X, new.Y, model)
#pred$error.rate
Error.rate(pred$predicted.Y, tmp_new.Y)

misClasificError <- mean(pred$predicted.Y != loantest$Loan_Status)
print(paste('Accuracy',1-misClasificError))



boo <- wsvm.boost(X, Y, new.X, new.Y, c.n = rep(1 / length(Y),length(Y)), 
                  B = 100, kernel.type = list(type="rbf", par= 0.5), C = 4, 
                  eps = 1e-10, plotting = T)
boo
kadya_op <- as.matrix(boo$predicted.model$predicted.Y)
####after boosting
#pred <- wsvm.predict(X,Y, new.X, new.Y, boo)
#pred$error.rate
#Error.rate(pred$predicted.Y, Y)
misClasificError <- mean(kadya_op != loantest$Loan_Status)
print(paste('Accuracy',1-misClasificError))

table(kadya_op, loantest$Loan_Status)
