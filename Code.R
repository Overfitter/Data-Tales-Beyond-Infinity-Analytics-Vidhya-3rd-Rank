# Data-Tales-Beyond-Infinity-Analytics-Vidhya-3rd-Rank
Analytics Vidhya competition organized by Great Lake Institute Of Management, Chennai.
setwd("C:/Users/DELL/Downloads/Data Tales - beyond infinity")
train <- read.csv("C:/Users/DELL/Downloads/Data Tales - beyond infinity/train_uC9nQgh.csv", stringsAsFactors=FALSE)
test <- read.csv("C:/Users/DELL/Downloads/Data Tales - beyond infinity/test_gLI9AN9.csv", stringsAsFactors=FALSE)

### lowering the header values ###
colnames(train) <- tolower(colnames(train))
colnames(test) <- tolower(colnames(test))

#### no missing values ###
library(VIM)
aggr_plot <- aggr(total, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(total), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

## storing target variable and combining both train and test data ###
leads = train$obtained.leads
train$obtained.leads = NULL
total = rbind(train, test)
str(total)
names(total)
summary(total)

### feature engineering ###

total$month = substring(total$day.index, 4,5)
total$day = substring(total$day.index, 1,2)
total$year = substring(total$day.index, 7,11)
total$day.index = NULL

total$avg.cpc = (total$min.cpc + total$max.cpc)/2
total$diff.cpc = (total$max.cpc - total$min.cpc)

total$lacked.clicks = (total$target.clicks - total$obt.click)
total$gained.clicks = -(total$lacked.clicks)
total$add.shown.times = total$obt.click/total$ctr
total$add.shown.target = total$target.clicks/total$ctr
total$add.shown.times = round(total$add.shown.times, digits = 0)
total$add.shown.target = round(total$add.shown.target, digits = 0)
total$defect.add.number = (total$add.shown.target- total$add.shown.times)
total$gained.add.number = -(total$defect.add.number)

total$lacked.clicks[total$lacked.clicks<0] = 0
total$defect.add.number[total$defect.add.number<0] = 0
total$gained.clicks[total$gained.clicks<0] = 0
total$gained.add.number[total$gained.add.number<0] = 0

total$profit.p = ((total$total.revenue - total$amount.spend)/total$amount.spend)*100
total$lacked.clicks[total$lacked.clicks<0] = 0
total$loss.p = -(total$profit.p)
total$profit.p[total$profit.p<0] = 0
total$loss.p[total$loss.p<0] = 0

total$avg.bounce = total$avg..bounce.rate*total$avg.time.page

library(dummies)
total_with_dummy <- dummy.data.frame(total, names=c("campaign","specialday","avg.position"), sep="_")

names(total_with_dummy)
summary(total_with_dummy)
str(total_with_dummy)

total_with_dummy <- data.frame(total_with_dummy)

### spliting the data in original format ### 
training_with_dummy = total_with_dummy[1:nrow(train),]
testing_with_dummy = total_with_dummy[(nrow(train)+1):nrow(total),]
## rm(total_with_dummy)

training_with_dummy$month = as.numeric(training_with_dummy$month)
training_with_dummy$year = as.numeric(training_with_dummy$year)
training_with_dummy$day = as.numeric(training_with_dummy$day)

testing_with_dummy$month = as.numeric(testing_with_dummy$month)
testing_with_dummy$year = as.numeric(testing_with_dummy$year)
testing_with_dummy$day = as.numeric(testing_with_dummy$day)

## GBM Model ##
library(gbm)
zeros <- rep(0, 427)

#--------------------------------------------
# I will fit 20 models.
# The predictions are averaged out.
# So this is simply an ensemble of boosters.
#--------------------------------------------

control <- 20
gbm.perf(gbm)
for (i in 1:control)
{
gbm = gbm(leads ~., distribution="gaussian", data=training_with_dummy,cv.fold=10, n.trees=5000, interaction.depth =6, shrinkage=0.05, n.minobsinnode = 10)
#outputfile 
yhat <- predict.gbm(gbm,testing_with_dummy, n.trees = 5000)
zeros <- zeros + yhat
}
zeros <- zeros/control
mysolution_new = data.frame( Day.Index = test$day.index, Obtained.Leads = zeros, stringsAsFactors = FALSE)
submission_gbm = mysolution_new
submission_gbm$Obtained.Leads = round(mysolution_new1$Obtained.Leads, digits = 0)
write.csv(submission_gbm, file = "submission_gbm.csv", row.names = FALSE)
write.csv(mysolution_new, file = "mysolution_new_gbm.csv", row.names = FALSE)
