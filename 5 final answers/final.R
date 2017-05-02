# final submission
setwd("H://projects//Analytix Vidhya//loan prediction 3//5 final answers/")
loantrain <- read.csv(file="train_file.csv", header=TRUE,as.is=T)
loantest<- read.csv(file="test_file.csv", header=TRUE,as.is=T)
summary(loantrain)
summary(loantest)

######################################################
#formatting train file

loantrain[loantrain=="No Info"] <- NA
loantrain[loantrain==""] <- NA

#replacing 3+ with (3,4,5) randomly
for(i in 1:614 ){
  if(!is.na(loantrain$Dependents[i])){
    if(loantrain$Dependents[i]=="3+"){
      loantrain$Dependents[i] <- sample(c("3","4","5"),replace = FALSE)
    }
  }
}

col_numeric <- c(4,7:11)
for(i in col_numeric){
  loantrain[,i] <- as.numeric(loantrain[,i])
}

col_factor <- c(2,3,5,6,12,13)
for(i in col_factor){
  loantrain[,i] <- as.factor(loantrain[,i])
}

summary(loantrain)
######################################################################
#formatting test file

loantest[loantest=="No Info"] <- NA
loantest[loantest==""] <- NA

#replacing 3+ with (3,4,5) randomly
for(i in 1:367 ){
  if(!is.na(loantest$Dependents[i])){
    if(loantest$Dependents[i]=="3+"){
      loantest$Dependents[i] <- sample(c("3","4","5"),replace = FALSE)
    }
  }
}

col_numeric <- c(4,7:11)
for(i in col_numeric){
  loantest[,i] <- as.numeric(loantest[,i])
}

col_factor <- c(2,3,5,6,12)
for(i in col_factor){
  loantest[,i] <- as.factor(loantest[,i])
}

summary(loantest)
##################################################################################
#cleaning train data using mice package

library(mice)

my_col1 <- c(2,3,4,6,9:11)
new_loantrain <- loantrain[my_col1]
colnames(new_loantrain)
summary(new_loantrain)

imputed_Data1 <- mice(new_loantrain, m=5, defaultMethod = c("pmm","logreg", "polyreg", "polr"), maxit = 50, seed = 777)

completeData1 <- complete(imputed_Data1,5)
#write.csv(completeData1,"filtered_data-test-final.csv",row.names=F)
summary(completeData1)

loantrain$Gender <- completeData1$Gender
loantrain$Married <- completeData1$Married
loantrain$Self_Employed <- completeData1$Self_Employed
loantrain$Dependents <- completeData1$Dependents
loantrain$LoanAmount <- completeData1$LoanAmount
loantrain$Loan_Amount_Term <- completeData1$Loan_Amount_Term
loantrain$Credit_History <- completeData1$Credit_History

summary(loantrain)
write.csv(loantrain,"exp1.csv",row.names=F)
##################################################################################
#cleaning test data using mice package

my_col2 <- c(2,4,6,9:11)
new_loantest <- loantrain[my_col2]
colnames(new_loantest)
summary(new_loantest)

imputed_Data2 <- mice(new_loantest, m=5, defaultMethod = c("pmm","logreg", "polyreg", "polr"), maxit = 50, seed = 777)

completeData2 <- complete(imputed_Data2,5)
#write.csv(completeData1,"filtered_data-test-final.csv",row.names=F)
summary(completeData2)

loantest$Gender <- completeData2$Gender
#loantrain$Married <- completeData1$Married
loantest$Self_Employed <- completeData2$Self_Employed
loantest$Dependents <- completeData2$Dependents
loantest$LoanAmount <- completeData2$LoanAmount
loantest$Loan_Amount_Term <- completeData2$Loan_Amount_Term
loantest$Credit_History <- completeData2$Credit_History

summary(loantest)
#write.csv(floantest,"filtered_data-test-final.csv",row.names=F)

##############################################################################
#dimension reduction using boruta pValue=0.01

library(Boruta)
set.seed(777)

boruta.train <- Boruta(Loan_Status ~ . - Loan_ID, data= loantrain ,pValue =0.01, doTrace=2)
print(boruta.train)

final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

selected_attrib <- data.frame(getSelectedAttributes(final.boruta, withTentative = F))
#automate this process
final_loan_train <- loantrain[c(1,5,7:11,13)]
final_loan_test <- loantest[c(1,5,7:11)]

################################################################################
#model fitting : svm


library(e1071)
tuned_parameters <- tune.svm(Loan_Status~.-Loan_ID, data = loantrain, gamma = 10^(-5:-1), cost = 10^(-3:1))
summary(tuned_parameters )

model_svm <- svm(Loan_Status ~.-Loan_ID, data = loantrain,  kernel = "radial", gamma = 0.01, cost = 1)
model_svm
my_prediction <- predict(model_svm, loantest)
my_prediction

submit <- data.frame(my_prediction)
submit

#plot(model_svm, data = )

write.csv(submit,"submission-file-temp3.csv",row.names=F)

###########################################
# kaadya
new_data <- read.csv(file="exp1.csv", header=TRUE,as.is=T)
new_data<- new_data[c(2:13)]

model_svm <- svm(Loan_Status ~., data = new_data,  kernel = "linear", gamma = 0.01, cost = 1)
model_svm
my_prediction <- predict(model_svm, loantest)
my_prediction

submit <- data.frame(my_prediction)
submit

#plot(model_svm, data = )

write.csv(submit,"submission-file-temp-exp1.csv",row.names=F)

##########################################################
# logistic regression
newTest <- final_loan_test[c(1:7)]
model <- glm(Loan_Status ~ ., family = binomial(link = 'logit'), data = final_loan_train)
summary(model)
anova(model, test = 'Chisq')

fitted.results <- predict(model,newdata=newTest,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
fitted.results
misClasificError <- mean(fitted.results != loantest$Loan_Status)
print(paste('Accuracy',1-misClasificError))

