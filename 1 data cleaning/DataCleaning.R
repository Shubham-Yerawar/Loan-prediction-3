
setwd("H://projects//loan prediction 3//data cleaning")
loandata <- read.csv(file="train_file.csv", header=TRUE,as.is=T)
summary(loandata)

col_numeric <- c(4,7:11)
for(i in col_numeric){
  loandata[,i] <- as.numeric(loandata[,i])
}

col_factor <- c(2,3,5,6,12,13)
for(i in col_factor){
  loandata[,i] <- as.factor(loandata[,i])
}

library(mice)


my_col1 <- c(4,9:11)
new_loandata <- loandata[my_col1]
colnames(new_loandata)
summary(new_loandata)


imputed_Data1 <- mice(new_loandata, m=5, method="pmm", maxit = 10, seed = 777)


completeData1 <- complete(imputed_Data1,5)
write.csv(completeData1,"filtered_data.csv",row.names=F)

summary(completeData1)



