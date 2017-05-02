
setwd("H:/projects//loan prediction 3//dimension reduction")
loandata<- read.csv(file="cleaned_data.csv", header=TRUE,as.is=T)
summary(loandata)

col_numeric <- c(4,7:11)
for(i in col_numeric){
  loandata[,i] <- as.numeric(loandata[,i])
}

col_factor <- c(2,3,5,6,12,13)
for(i in col_factor){
  loandata[,i] <- as.factor(loandata[,i])
}

summary(loandata)

library(Boruta)
set.seed(777)

boruta.train <- Boruta(Loan_Status ~ . - Loan_ID, data= loandata ,pValue =0.05, doTrace=2)
print(boruta.train)

final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

getSelectedAttributes(final.boruta, withTentative = F)

boruta.df <- attStats(final.boruta)
class(boruta.df)
print(boruta.df)

write.csv(boruta.df,"select_boruta.csv",row.names=F)
 