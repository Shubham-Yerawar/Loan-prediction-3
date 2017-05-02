# program for model fitting <- linear regression model

setwd("H://projects//loan prediction 3//4 model fitting/")
loandata<- read.csv(file="train_set.csv", header=TRUE,as.is=T)
summary(loandata)

loantest<- read.csv(file="test_set.csv", header=TRUE,as.is=T)
summary(loantest)

col_numeric <- c(2:6)
for(i in col_numeric){
  loandata[,i] <- as.numeric(loandata[,i])
}

col_factor <- c(7,8)
for(i in col_factor){
  loandata[,i] <- as.factor(loandata[,i])
}

loandata$Loan_Status <- factor(loandata$Loan_Status, levels=c("Y","N"), labels=c("0", "1"))
loandata$Loan_Status <- as.factor(loandata$Loan_Status)
loandata$Loan_Status <- as.numeric(loandata$Loan_Status)
loandata$Loan_Status<- loandata$Loan_Status-1
loandata$Loan_Status

loantest$Loan_Status <- factor(loantest$Loan_Status, levels=c("Y","N"), labels=c("0", "1"))
loantest$Loan_Status <- as.factor(loantest$Loan_Status)
loantest$Loan_Status <- as.numeric(loantest$Loan_Status)

loantest$Loan_Status <- loantest$Loan_Status-1
loantest$Loan_Status
##################for test data
col_numeric <- c(2:6)
for(i in col_numeric){
  loantest[,i] <- as.numeric(loantest[,i])
}

col_factor <- c(7,8)
for(i in col_factor){
  loantest[,i] <- as.factor(loantest[,i])
}
newcol <- c(2:7)
newTest <- loantest[newcol]


### parameter scaling
#applicantincome.c = scale(loandata$ApplicantIncome, center=TRUE, scale=TRUE)
#coapplicantincome.c = scale(loandata$CoapplicantIncome, center=TRUE, scale=TRUE)
#loanamt.c = scale(loandata$LoanAmount, center=TRUE, scale=TRUE)
#loanamtterm.c = scale(loandata$Loan_Amount_Term, center=TRUE, scale=TRUE)

# fit a linear model and run a summary of its results.
mod1 <- lm(Loan_Status ~ ApplicantIncome + CoapplicantIncome + loandata$LoanAmount + loandata$Loan_Amount_Term + loandata$Credit_History + loandata$Property_Area, data=loandata)
summary(mod1)

predict.lm(mod1,newTest, interval = "predict")


loandata$Loan_Status <- loandata$Loan_Status -1
loandata$Loan_Status
###################################################
#logistic regression
newTrain <- loandata[c(2:8)]
model <- glm(Loan_Status ~ ., family = binomial(link = 'logit'), data = newTrain)
summary(model)
anova(model, test = 'Chisq')

fitted.results <- predict(model,newdata=newTest,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
fitted.results
misClasificError <- mean(fitted.results != loantest$Loan_Status)
print(paste('Accuracy',1-misClasificError))



#########################################
# svm

library(e1071)
tuned_parameters <- tune.svm(Loan_Status~., data = newTrain, gamma = 10^(-5:-1), cost = 10^(-3:1))
summary(tuned_parameters )

model_svm <- svm(Loan_Status ~., data = newTrain,  kernel = "radial", gamma = 0.1, cost = 0.5)
model_svm
my_prediction <- predict(model_svm, newTest)
my_prediction <- ifelse(my_prediction > 0.5,1,0)
my_prediction
misClasificError <- mean(my_prediction != loantest$Loan_Status)
print(paste('Accuracy',1-misClasificError))


####################################################################
