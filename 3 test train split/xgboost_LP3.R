## xgboost : loan prediction 3
library(xgboost)
loandata <- read.csv(file = "reduced_data.csv", header = T, as.is = T)
loandata <- loandata[,-1]

testdata <- read.csv(file = "filtered_data-test-final.csv", header = T, as.is = T)
testdata <- testdata[,c(1,7:12)]

col_numeric <- c(1:3)+1
for(i in col_numeric){
  #loandata[,i] <- as.numeric(loandata[,i])
  testdata[,i] <- as.numeric(testdata[,i])
}

col_factor <- c(4:6)+1 # 6 -> 7 for loandata
for(i in col_factor){
  #loandata[,i] <- as.factor(loandata[,i])
  testdata[,i]<- as.factor(testdata[,i])
}

summary(testdata)

########### splitting ###########################

## i% of the sample size
#smp_size <- floor(0.7 * nrow(loandata))

## set the seed to make your partition reproductible
#set.seed(777)
#train_ind <- sample(seq_len(nrow(loandata)), size = smp_size)

#train <- loandata[train_ind, ]
#test <- loandata[-train_ind, ]


#################### xgboost ##############################

dtrain <- xgb.DMatrix(as.matrix(sapply(loandata[,2:ncol(loandata)], as.numeric)), label=loandata$Loan_Status)
dtest <- xgb.DMatrix(as.matrix(sapply(testdata[,1:ncol(testdata)], as.numeric)))
xgb <- xgboost(data = dtrain, 
               label = loandata$Loan_Status, 
               eta = 0.1, # learning rate
               max_depth = 15, 
               nround=25
)

predictions <- predict(xgb, dtest)
predictions <- ifelse(predictions > 1,"Y","N")

########## for output #############
submission <- data.frame()
submission[1:367,1] <- 1
submission[1:367,2] <- 1
submission[,1] = testdata[,1]  
submission[,2] = predictions
colnames(submission) <- c("Loan_ID","Loan_Status")
####### write to output file
write.csv(submission, file = "LP3_output_xgboost.csv")


#misClasificError2 <- mean(predictions != test2$Loan_Status)
#print(paste('Accuracy',1-misClasificError2))
