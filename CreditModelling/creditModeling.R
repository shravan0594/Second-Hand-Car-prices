getwd()
setwd("G:\\Learn\\DATASCIENCE\\CreditModelling")
Data<-read.csv("LoansTrainingSetV2.csv",stringsAsFactors = FALSE)
#Actual_data<-Data
str(Data)
length(unique(Data$Loan.ID))
loan_unique = Data[row.names(as.data.frame(unique(Data$Loan.ID))),]
#loan_unique1=loan_unique
class(loan_unique$Loan.ID)
class(loan_unique$Customer.ID)
length(unique(loan_unique$Customer.ID))

class(loan_unique$Loan.Status)

loan_unique$Loan.Status<-as.factor(loan_unique$Loan.Status)
str(loan_unique)
levels(loan_unique$Loan.Status)
table(loan_unique$Loan.Status)
View(loan_unique)
class(loan_unique$Current.Loan.Amount)
summary(loan_unique$Current.Loan.Amount)
hist(loan_unique$Current.Loan.Amount)
install.packages("ggplot2")
library(ggplot2)
qplot(loan_unique$Current.Loan.Amount,geom = "histogram")
boxplot.stats(loan_unique$Current.Loan.Amount)
#loan_unique2<-loan_unique1
extreme_values<-boxplot.stats(loan_unique$Current.Loan.Amount)
loan_unique$Current.Loan.Amount[loan_unique$Current.Loan.Amount>=min(extreme_values$out)]<-NA
summary(loan_unique$Current.Loan.Amount)
qplot(loan_unique$Current.Loan.Amount,geom = "histogram")

loan_unique$Term<-as.factor(loan_unique$Term)
table(loan_unique$Term)

summary(loan_unique$Credit.Score)
loan_unique$Credit.Score<-ifelse(loan_unique$Credit.Score>800,loan_unique$Credit.Score/10,loan_unique$Credit.Score)
#loan_unique$Credit.Score[is.na(loan_unique$Credit.Score)==TRUE]<-median(loan_unique$Credit.Score,na.rm = TRUE)

summary(loan_unique$Years.in.current.job)
install.packages("stringr")
library(stringr)
loan_unique$Years.in.current.job<-str_replace_all(loan_unique$Years.in.current.job,fixed("n/a"),NA)

table(loan_unique$Years.in.current.job)


loan_unique$Years.in.current.job<-as.factor(loan_unique$Years.in.current.job)
table(loan_unique$Home.Ownership)

loan_unique$Home.Ownership<-str_replace_all(loan_unique$Home.Ownership,fixed("HaveMortgage"),"Home Mortgage")
loan_unique$Home.Ownership<-as.factor(loan_unique$Home.Ownership)
summary(loan_unique$Home.Ownership)

summary(loan_unique$Annual.Income)

loan_unique$Annual.Income1<-loan_unique$Annual.Income

boxplot.stats(loan_unique$Annual.Income1)

quantile(loan_unique$Annual.Income1,probs = seq(0,1,0.05),na.rm = TRUE)
outlier<-quantile(loan_unique$Annual.Income1,probs = seq(0.95,1,0.01),na.rm = TRUE)
outlier[5]
loan_unique$Annual.Income1[loan_unique$Annual.Income1>outlier[5]]<-outlier[5]
summary(loan_unique$Annual.Income1)

hist(loan_unique$Annual.Income1)
loan_unique$Annual.Income1<-log(loan_unique$Annual.Income1)
loan_unique$Annual.Income<-loan_unique$Annual.Income1
table(loan_unique$Purpose)
loan_unique$Purpose<-str_replace_all(loan_unique$Purpose,"Other","other")

loan_unique$Purpose<-as.factor(loan_unique$Purpose)

loan_unique$Monthly.Debt2<-str_replace_all(loan_unique$Monthly.Debt,fixed("$"),"")
loan_unique$Monthly.Debt2<-str_replace_all(loan_unique$Monthly.Debt2,fixed(","),"")
loan_unique$Monthly.Debt2<-as.numeric(loan_unique$Monthly.Debt2)
summary(loan_unique$Monthly.Debt2)
loan_unique$Monthly.Debt<-loan_unique$Monthly.Debt2

quantile(loan_unique$Monthly.Debt,probs = seq(0,1,0.05),na.rm = TRUE)
outlier<-quantile(loan_unique$Monthly.Debt,probs = seq(0.95,1,0.01),na.rm = TRUE)

loan_unique$Monthly.Debt[loan_unique$Monthly.Debt>outlier[5]]<-outlier[5]

summary(loan_unique$Monthly.Debt)

qplot(loan_unique$Monthly.Debt)

class(loan_unique$Years.of.Credit.History)

summary(loan_unique$Years.of.Credit.History)

quantile(loan_unique$Years.of.Credit.History,probs = seq(0,1,0.05),na.rm = TRUE)
outlier<-quantile(loan_unique$Years.of.Credit.History,probs = seq(0.95,1,0.01),na.rm = TRUE)
outlier
qplot(loan_unique$Years.of.Credit.History)


summary(loan_unique$Number.of.Open.Accounts)
quantile(loan_unique$Number.of.Open.Accounts,probs = seq(0,1,0.05),na.rm = TRUE)
outlier<-quantile(loan_unique$Number.of.Open.Accounts,probs = seq(0.95,1,0.01),na.rm = TRUE)
loan_unique$Number.of.Open.Accounts[loan_unique$Number.of.Open.Accounts>outlier[5]]<-outlier[5]

quantile(loan_unique$Number.of.Credit.Problems,probs = seq(0,1,0.05),na.rm = TRUE)
summary(loan_unique$Number.of.Credit.Problems)



quantile(loan_unique$Current.Credit.Balance,probs = seq(0,1,0.05),na.rm = TRUE)
outlier<-quantile(loan_unique$Current.Credit.Balance,probs = seq(0.95,1,0.01),na.rm = TRUE)
outlier
loan_unique$Current.Credit.Balance[loan_unique$Current.Credit.Balance>outlier[5]]<-outlier[5]

qplot(log(loan_unique$Current.Credit.Balance))

loan_unique$Current.Credit.Balance<-sqrt(loan_unique$Current.Credit.Balance)


loan_unique$Maximum.Open.Credit<-str_replace_all(loan_unique$Maximum.Open.Credit,fixed("#VALUE!"),NA)

loan_unique$Maximum.Open.Credit<-as.numeric(loan_unique$Maximum.Open.Credit)

summary(loan_unique$Maximum.Open.Credit)

quantile(loan_unique$Maximum.Open.Credit,probs = seq(0,1,0.05),na.rm = TRUE)

quantile(loan_unique$Maximum.Open.Credit,probs = seq(0.95,1,0.01),na.rm = TRUE)

loan_unique$Maximum.Open.Credit[loan_unique$Maximum.Open.Credit>150000]<-150000

summary(loan_unique$Maximum.Open.Credit)

loan_unique$Maximum.Open.Credit<-ifelse(is.na(loan_unique$Maximum.Open.Credit),21780,loan_unique$Maximum.Open.Credit)
loan_unique$Maximum.Open.Credit<-sqrt(loan_unique$Maximum.Open.Credit)
summary(loan_unique$Maximum.Open.Credit)

loan_unique$Bankruptcies<-ifelse(is.na(loan_unique$Bankruptcies),0,loan_unique$Bankruptcies)

summary(loan_unique$Tax.Liens)

loan_unique$Tax.Liens<-ifelse(is.na(loan_unique$Tax.Liens),0,loan_unique$Tax.Liens)
summary(loan_unique$Bankruptcies)


str(loan_unique)

sapply(loan_unique,function(x){sum(is.na(x))})

loan_unique$Annual.Income1<-NULL
loan_unique$Monthly.Debt1<-NULL
loan_unique$Monthly.Debt2<-NULL

str(loan_unique)

table(loan_unique$Bankruptcies)


sapply(loan_unique,function(x){sum(is.na(x))})

loan_unique$Months.since.last.delinquent<-NULL

sapply(loan_unique, summary)

View(loan_unique)

install.packages("mice")

library(mice)

simple<-loan_unique[,4:18]

impute<-mice(simple,m=1)
sapply(loan_compute,function(x){sum(is.na(x))})

loan_complete<-complete(impute)
#loan_complete_dummy<-loan_complete

str(loan_complete)

loan_complete<-cbind(loan_complete,Loan.Status=loan_unique$Loan.Status)

loan_complete$Loan.Status<-ifelse(loan_complete$Loan.Status=='Charged Off',1,0)

install.packages("caTools")
library(caTools)
set.seed(1)
spl<-sample.split(loan_complete$Loan.Status,SplitRatio = 0.7)

train<-subset(loan_complete,spl==TRUE)

test<-subset(loan_complete,spl==FALSE)

install.packages("xgboost")
install.packages("Matrix")
library(xgboost)
library(Matrix)

sparse.train<- sparse.model.matrix(Loan.Status ~ .-1,data = train)

dtrain<-xgb.DMatrix(data = sparse.train,label=train$Loan.Status)

sparse.test<- sparse.model.matrix(Loan.Status ~ .-1,data = test)

dtest<-xgb.DMatrix(data = sparse.test,label=test$Loan.Status)


watchlist<-list(train=dtrain,test=dtest)
class(watchlist)

params<-list(eta=0.01,max_depth=4,objective="binary:logistic")

model_xgb<-xgb.train(params = params,nrounds = 5000,data = dtrain,verbose = 1,watchlist = watchlist)

importance_final<-xgb.importance(feature_names = colnames(sparse.train),model = model_xgb)
summary(importance_final)















 
 

















