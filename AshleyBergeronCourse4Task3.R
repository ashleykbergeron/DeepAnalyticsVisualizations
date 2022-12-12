library(caret)
ExactLoc <- read.csv("~/Desktop/UpdatedTrainingData.csv")
View(ExactLoc)
attributes(ExactLoc)
summary(ExactLoc)
summary(ExactLoc$SPACEID)
str(ExactLoc)
names(ExactLoc)
is.na(ExactLoc)
#try histograms
hist(ExactLoc$FLOOR)
hist(ExactLoc$BUILDINGID)
#building #s 0, 1, 2
#over 7000 occurrences in building 2, approx 5500 in buildings 0 & 1
hist(ExactLoc$SPACEID)
#space ID is basically room number
hist(ExactLoc$RELATIVEPOSITION)
#relative position - 1 is inside the door and 2 is outside the door
hist(ExactLoc$USERID)
#people given a number by height
hist(ExactLoc$PHONEID)
summary(ExactLoc$PHONEID)
#over 7000 occurrences happened with phone ID #13
#need to try to combine building, floor, space ID, and relative position into 1 attribute
#try to combine columns using str_c() function
library(stringr)
ExactLoc$SpecificLoc <- str_c(ExactLoc$BUILDINGID, ExactLoc$FLOOR, ExactLoc$SPACEID, ExactLoc$RELATIVEPOSITION)
#trying to create a random sample of approx 10%
#sample without replacement
SampleLoc <- ExactLoc[sample(1:nrow(ExactLoc), 2000, replace=FALSE),]
#try using near zero variance to remove some unneeded features
nzv <- nearZeroVar(SampleLoc, saveMetrics = TRUE)
#list first ten
nzv[nzv$nzv,][1:10,]
#get dimensions
dim(SampleLoc)
#create a list of indexes which have zeroVar = TRUE
zeroVarData1 <- which(nzv$zeroVar == TRUE)
print(zeroVarData1)
summary(zeroVarData1)
str(zeroVarData1)
#use list name to filter the attributes that have zero variance
library(dplyr)
SampleLoc = select(SampleLoc, select = -c((zeroVarData1)))
#try creating an even smaller sample size
SmallSampleLoc <- ExactLoc[sample(1:nrow(ExactLoc), 1000, replace = FALSE),]
#near zero variance to remove unneeded features
nzv2 <- nearZeroVar(SmallSampleLoc, saveMetrics = TRUE)
#create a list of indexes with have zeroVar = TRUE
zeroVarData2 <- which(nzv2$zeroVar == TRUE)
#use list to filter attributes with zero variance
SmallSampleLoc = select(SmallSampleLoc, select = -c((zeroVarData2)))
#now has 398 variables
#feature selection, remove USERID and PHONEID
SmallSampleLoc = subset(SmallSampleLoc, select = -c(USERID,PHONEID))
#now 396 variables
#up to here is the clean part

#now try some modeling
#train/test/split, then modeling
#plan to try KNN, Stochastic Gradient Boosting Model, C5.0, Random Forest
set.seed(123)
# define 75%/25% train/test split of the dataset
inTraining <- createDataPartition(SmallSampleLoc$SpecificLoc, p = .75, list = FALSE)
training <- SampleLoc[inTraining,]
testing <- SampleLoc[-inTraining,]
#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
#Try Gradient Boosting first
gbFit1 <- train(SpecificLoc~., data = training, method = "gbm", 
                trControl=fitControl, tuneLength = 1)
#task failed
##check for NAs and remove
SmallSampleLoc <- SmallSampleLoc %>%
  na.omit() %>% 
  select_if(is.numeric)
#removed one feature
#try the above model again
gbFit2 <- train(SpecificLoc~., data = training, method = "gbm", 
                trControl=fitControl, tuneLength = 1)
#failed also
#now try C5.0
system.time(c5Fit1 <- train(SpecificLoc~., data = training, method = "C5.0", 
                            trControl=fitControl, tuneLength = 1))
#check training results
c5Fit1
#check variable importance
varImp(c5Fit1)

#must be a factor
SmallSampleLoc$SpecificLoc<-as.factor(SmallSampleLoc$SpecificLoc)

