
############################### Extracting Data from the file ############################### 
# Set Working Directory
setwd("E:/Purdue University/Spring 2018/IE 590 - Predictive Modeling/Final Takehome Exam")
# Reading the csv file and storing the content in the dataframe 
dataframe <- read.csv("2012_public_use_data_aug2016.csv", sep =",")
# Reading the dataframe
dataframe

# Sorting the dataset based on CENDIV =="1" that is, New England Region
dataframe.sort <- dataframe[order(dataframe$CENDIV),]
# Reading the dataframe
dataframe.sort
# Checking the number of the data points marked as New England in the dataset - 319 in our case
summary(dataframe.sort$CENDIV == "1")

# New England region.
final.dataframe <- subset(dataframe.sort, CENDIV == "1")
# Response Variable for prediction - Electricity (Summation of three variables) - (Electricity consumed)
final.dataframe$ELECTRICITY <- final.dataframe$ELWTBTU + final.dataframe$ELHTBTU + final.dataframe$ELCLBTU
# Reading the final dataframe of New England Region.
final.dataframe

# Storing the New England dataset as a CSV file
write.csv(final.dataframe,"NewEngland.csv",row.names=FALSE)

# Selecting variables of Heating, Cooling and Heating Water. 
df <- final.dataframe[,c(1120,4,6,1056,8,9,10,11,12,13,16,27,29,30,31,32,36,38,41,42,50,85,88,89,90,102,116,126,138,139,140,141:144,152,200,209:214,216,225,255:260,269,279,398,1049,1050)]
# Reading the Dataframe 
df
# Check for Missing Values in the Dataset if any. 
summary(df)

############################## Missing Values in percentage for Visualization ############################## 
# Finding the percentage of Missing Values in the dataset for every predictor and visualizing this dataset.
# Look for missing > 5% variables
pMiss <- function(x){sum(is.na(x))/length(x)*100}

# Check each column
apply(df,2,pMiss)

# Plotting the Histogram of the Missing Values along with values in decreasing order.
library(VIM)
aggr_plot <- aggr(df, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

############################## Removing Missing Values from the Dataset ############################## 
# Making a new dataframe with no missing values in it.
final.df <- df[complete.cases(df),]
summary(final.df)
# Checking the structure of the dataset
str(final.df)



############################## Creating Dummies using Encoding ############################## 
# Dummy variables
# dummy.df <- new.df[,-c(1,2,3,21,55,56)]
# Structure of the Dummy Dataframe
# str(dummy.df)

# dummy <- dummy.df
# Encoding
# library(ade4)
# library(data.table)
# ohe_feats = c('SQFTC', 'WLCNS', 'RFCNS', 'RFCOOL', 'RFTILT','BLDSHP','GLSSPC','NFLOOR','RENOV','RENRDC', 
#              'RENCOS', 'RENINT', 'RENRFF','RENLGT','RENELC','RENSTR','RENOTH','MONUSE','OPEN24','OPNMF','OPNWE','GENR','ELHT1','ELHT2','FURNAC','PKGHT','BOILER','STHW','HTPMPH','SLFCON','OTHTEQ','MAINHT','ELCOOL','RCAC','PKGCL','CHILLR','CHWT','HTPMPC','ACWNWL','OTCLEQ','MAINCL','CLVCAV','CLVVAV','CLVFLR','CLVOAS','CLVDEM','CLVNON','ELWATR','WTHTEQ','DATACNTR')
# for (f in ohe_feats){
#   df_all_dummy = acm.disjonctif(dummy[f])
#   dummy[f] = NULL
#   dummy = cbind(dummy, df_all_dummy)
# }
# Encoded Dataframe
# str(dummy)


# New Dataframe with continuous variables in it.
# cont.df <- new.df[,c(1,2,3,21,55,56)]
# Final Dataframe for evaluation of New England Region.
# final.df <- cbind(dummy,cont.df)
# Structure of Final Dataframe
# str(final.df)

############################### Explolatory Data Analysis ############################### 
# Distribution of each variable - Histograms
par(mfrow=c(1,2))
for (i in 4:48) {
  hist(final.df[,i], xlab=names(final.df)[i], main=names(final.df)[i], col="blue", prob=T)
  lines(density(final.df[,i]), col="red", lwd=2) #adds density line
}

# Visualizing Outliers in form of Boxplots
outlier_values <- boxplot.stats(final.df$ELECTRICITY)$out  # outlier values.
boxplot(final.df$ELECTRICITY, main="Electricity Consumed", boxwex=0.1)
final.df <- final.df[which(final.df$ELECTRICITY<=24000000),]

# Visualizing Outliers after transition.
outlier_values <- boxplot.stats(final.df$ELECTRICITY)$out  # outlier values.
boxplot(final.df$ELECTRICITY, main="Electricity Consumed", boxwex=0.1)


final.df$ELECTRICITY = ((final.df$ELECTRICITY)/1000)


################################ Parameter Selection ################################
################################ Model 3 - Random Forest ################################ 
set.seed(9)

folds <- cut(seq(1,nrow(final.df)),breaks=10,labels=FALSE)
rmse.test.RF <- vector("numeric", 10)
rmse.train.RF <- vector("numeric", 10)
for(i in 1:10)
{
  testIndexes <- which(folds==i,arr.ind=TRUE)
  test <- final.df[testIndexes, ]
  train <- final.df[-testIndexes, ]
  

random.forest.model<-randomForest(ELECTRICITY ~ .,data=train,ntree=500,mtry = 4,importance=T)

# Making Predictions
random.forest.pred<-predict(random.forest.model,newdata=train)
random.forest.pred.OS<-predict(random.forest.model,newdata=test)

rmse.train.RF[i]<-rmse(actual = train$ELECTRICITY,predicted = random.forest.pred)
rmse.test.RF[i]<-rmse(actual = test$ELECTRICITY,predicted = random.forest.pred.OS)

plot(random.forest.pred.OS, test$ELECTRICITY, xlab = "predicted rf", ylab = "actual.test", main=c(i, "test"))
abline(a=0,b=1)

}
# Finding the important predictors.
plot(random.forest.model)
varImpPlot(random.forest.model)
varImpPlot(random.forest.model, sort = T, main="Variable Importance")

var.imp <- data.frame(importance(random.forest.model,type=2))
var.imp$Variables <- row.names(var.imp)
var.imp[order(var.imp$IncNodePurity,decreasing = T),]
random.forest.model$predicted



################################ Model 4 - SVM ################################ 
set.seed(9)

folds <- cut(seq(1,nrow(final.df)),breaks=10,labels=FALSE)
rmse.test.svm <- vector("numeric", 10)
rmse.train.svm <- vector("numeric", 10)

rmse.kernel.train <- vector("numeric", 3)
rmse.kernel.test <- vector("numeric", 3)

inTrain <- createDataPartition(y = final.df$ELECTRICITY,
                               p = .85,            
                               list = FALSE)
train <- final.df[inTrain,]
test <- final.df[-inTrain,]

#choosing kernel 
kernel <- c("linear","radial","polynomial")

#radial best choice
for(j in 1:3){
  
  for(i in 1:10){
    
    testIndexes <- which(folds==i,arr.ind=TRUE)
    test <- final.df[testIndexes, ]
    train <- final.df[-testIndexes, ]
    
    model.svm <- svm(ELECTRICITY ~., data = train, kernel = kernel[j])
    
    model.pred.svm <- predict(model.svm, train)
    model.pred.OS.svm <- predict(model.svm, test)
    
    rmse.train.svm[i] <- rmse(actual = train$ELECTRICITY, predicted = model.pred.svm)
    rmse.test.svm[i] <- rmse(actual = test$ELECTRICITY, predicted = model.pred.OS.svm)
    
    
  }
  
  rmse.kernel.train[j] <- mean(rmse.train.svm)
  rmse.kernel.test[j] <- mean(rmse.test.svm)
  
}

#model with best kernel
model.svm <- svm(ELECTRICITY ~., data = train, kernel = kernel[2])
summary(model.svm)

#C and gamma parameters
#does CV implicitly -> again, we do it explicitly once our parameters are chosen
svm.tune <- tune(svm, ELECTRICITY~., data=train, kernel="radial", ranges=list(cost=10^(-1:1), gamma=2^(-1:1)), folds.num = 5)






################################ Model 5 - BART ################################ 
inTrain <- createDataPartition(y = final.df$ELECTRICITY,
                               p = .85,            
                               list = FALSE)
train <- final.df[inTrain,]
test <- final.df[-inTrain,]

bart_machine_cv <- bartMachineCV(train[,-1], train$ELECTRICITY)

investigate_var_importance(bart_machine_cv, type = "splits", 
                           plot = TRUE, num_replicates_for_avg = 5, num_trees_bottleneck = 20, 
                           num_var_plot = Inf, bottom_margin = 10)


################################ Model 6 - Neural Nets ################################ 

#choosing the parameters for nnet - its own cross validation - not doing it explicitly 
#because here we're choosing the tuning - explicity CV when we pick the parameters

set.seed(9)
inTrain <- createDataPartition(y = final.df$ELECTRICITY,p = .70,list = FALSE)

train <- final.df[inTrain,]
test <- final.df[-inTrain,]


#Regression type control
ctrl <- trainControl(method="cv",     # cross-validation set approach to use
                     number=10,       # k number of times to do k-fold
                     classProbs = F,  
                     summaryFunction = defaultSummary,
                     allowParallel=T
)

# Each model below is training using different tuning parameters
myModel1 <- train(ELECTRICITY ~ .,       
                  data = train,     
                  method = "nnet",     
                  trControl = ctrl,    
                  tuneLength = 1,
                  maxit = 100,
                  linout = 1
)

myModel2 <- train(ELECTRICITY ~ .,       
                  data = train,     
                  method = "nnet",     
                  trControl = ctrl,    
                  tuneLength = c(1:3),
                  maxit = 100,
                  linout = 1
)  


myGrid <-  expand.grid(size = c(3, 5, 10, 20)    
                       , decay = c(0.5, 0.1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7)) 
myModel3 <- train(ELECTRICITY ~ .,       
                  data = train,     
                  method = "nnet",     
                  trControl = ctrl,    
                  tuneGrid = myGrid,
                  linout = 1
)


myGrid <-  expand.grid(size = seq(from = 1, to = 10, by = 1)     
                       , decay = c(0.5, 0.1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7))  
myModel4 <- train(ELECTRICITY ~ .,       
                  data = train,     
                  method = "nnet",     
                  trControl = ctrl,    
                  tuneGrid = myGrid,
                  maxit = 500,
                  linout = 1
)

model.pred.1 <- predict(myModel1, train)
model.pred.OS.1 <- predict(myModel1, test)

rmse.train.1 <- rmse(actual = train$ELECTRICITY, predicted = model.pred.1)
rmse.test.1 <- rmse(actual = test$ELECTRICITY, predicted = model.pred.OS.1)

model.pred.2 <- predict(myModel2, train)
model.pred.OS.2 <- predict(myModel2, test)

rmse.train.2 <- rmse(actual = train$ELECTRICITY, predicted = model.pred.2)
rmse.test.2 <- rmse(actual = test$ELECTRICITY, predicted = model.pred.OS.2)


model.pred.3 <- predict(myModel3, train)
model.pred.OS.3 <- predict(myModel3, test)


rmse.train.3 <- rmse(actual = train$ELECTRICITY, predicted = model.pred.3)
rmse.test.3 <- rmse(actual = test$ELECTRICITY, predicted = model.pred.OS.3)


model.pred.4 <- predict(myModel4, train)
model.pred.OS.4 <- predict(myModel4, test)

rmse.train.4 <- rmse(actual = train$ELECTRICITY, predicted = model.pred.4)
rmse.test.4 <- rmse(actual = test$ELECTRICITY, predicted = model.pred.OS.4)

#we choose model4 as the best one

data.frame(rmse.train.1,rmse.train.2,rmse.train.3,rmse.train.4)
data.frame(rmse.test.1, rmse.test.2, rmse.test.3, rmse.test.4)


set.seed(9)

rmse.test.UPMARS <- vector("numeric", 10)
rmse.train.UPMARS <- vector("numeric", 10)

rmse.test.PMARS <- vector("numeric", 10)
rmse.train.PMARS <- vector("numeric", 10)


folds <- cut(seq(1,nrow(final.df)),breaks=10,labels=FALSE)
rmse.test.RF <- vector("numeric", 10)
rmse.train.RF <- vector("numeric", 10)
for(i in 1:10)
{
  testIndexes <- which(folds==i,arr.ind=TRUE)
  test <- final.df[testIndexes, ]
  train <- final.df[-testIndexes, ]
  ################################ Model 7-MARS(Unpruned) ################################
  mars.unpruned.model<- earth(ELECTRICITY~., data=train,pmethod = 'none')

  # Making Predictions
  mars.unpruned.model.pred <- predict(mars.unpruned.model, train)
  mars.unpruned.model.pred.OS <- predict(mars.unpruned.model, test)  

  rmse.train.UPMARS[i] <- rmse(actual = train$ELECTRICITY, predicted = mars.unpruned.model.pred)
  rmse.test.UPMARS[i] <- rmse(actual = test$ELECTRICITY, predicted = mars.unpruned.model.pred.OS)

  par(mfrow=c(1,1))
  plot(evimp(mars.unpruned.model), main = "UnPruned Model")  
  
  ################################ Model 8-MARS(Pruned) ################################
  mars.pruned.model<-earth(ELECTRICITY~., data=train)

  # Making Predictions
  mars.pruned.model.pred <- predict(mars.pruned.model, train)
  mars.pruned.model.pred.OS <- predict(mars.pruned.model, test)  

  rmse.train.PMARS[i] <- rmse(actual = train$ELECTRICITY, predicted = mars.pruned.model.pred)
  rmse.test.PMARS[i] <- rmse(actual = test$ELECTRICITY, predicted = mars.pruned.model.pred.OS)
  
  par(mfrow=c(1,1))
  plot(evimp(mars.unpruned.model), main = "Pruned Model")  

}





################################ Fitting the Models ################################
library(ModelMetrics)
library(glmnet)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ModelMetrics)
library(caret)
library(e1071)
library(LncFinder)
library(bartMachine)
library(gam)
library(mda)
library(earth)
library(rminer)
library(psych)
library(caTools)


rmse.test.LM <- vector("numeric", 10)
rmse.train.LM <- vector("numeric", 10)

rmse.test.DT <- vector("numeric", 10)
rmse.train.DT <- vector("numeric", 10)

rmse.test.RF <- vector("numeric", 10)
rmse.train.RF <- vector("numeric", 10)

rmse.test.RF.fl <- vector("numeric", 10)
rmse.train.RF.fl <- vector("numeric", 10)

rmse.test.SVM <- vector("numeric", 10)
rmse.train.SVM <- vector("numeric", 10)

rmse.test.BART <- vector("numeric", 10)
rmse.train.BART <- vector("numeric", 10)

rmse.test.NNET <- vector("numeric", 10)
rmse.train.NNET <- vector("numeric", 10)

rmse.test.UPMARS <- vector("numeric", 10)
rmse.train.UPMARS <- vector("numeric", 10)

rmse.test.PMARS <- vector("numeric", 10)
rmse.train.PMARS <- vector("numeric", 10)

options(java.parameters = "-Xmx3000m")
set_bart_machine_num_cores(2)

folds <- cut(seq(1,nrow(final.df)),breaks=10,labels=FALSE)
#Perform 10 fold cross validation
  for(i in 1:10)
{
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- final.df[testIndexes, ]
  trainData <- final.df[-testIndexes, ]
    

  
  
  ################################ Model 1-Linear Model ################################
  # Fitting the Model
  linear.model<-lm(ELECTRICITY ~ ., data = trainData)
  # summary(linear.model)
  # alias(linear.model)

  # Predict
  linear.model.pred<-predict(linear.model,newdata=trainData)
  linear.model.pred.OS<-predict(linear.model,newdata=testData)
  
  rmse.train.LM[i]<-rmse(actual = trainData$ELECTRICITY,predicted = linear.model.pred)
  rmse.test.LM[i]<-rmse(actual = testData$ELECTRICITY,predicted = linear.model.pred.OS)

  plot(linear.model.pred.OS, testData$ELECTRICITY, xlab = "predicted lm", ylab = "actual.test", main=c(i, "test"))
  abline(a=0,b=1)
  
  
  

  ################################ Model 2-Decision Trees ################################
  # Fitting the Model
  rpart.model<-rpart(ELECTRICITY~.,data=trainData)
  
  # Making Predictions
  rpart.pred<-predict(rpart.model,newdata=trainData)
  rpart.pred.OS<-predict(rpart.model,newdata=testData)

  rmse.train.DT[i]<-rmse(actual = trainData$ELECTRICITY,predicted = rpart.pred)
  rmse.test.DT[i]<-rmse(actual = testData$ELECTRICITY,predicted = rpart.pred.OS)

  plot(rpart.pred.OS, testData$ELECTRICITY, xlab = "predicted Decision Trees", ylab = "actual.test", main=c(i, "test"))
  abline(a=0,b=1)
  
  
  
  
  ################################ Model 3-Random Forest ################################
  # Fitting the Model(After finding the Predictors)
  random.forest.model.fl<-randomForest(ELECTRICITY ~ ELEXP + SQFT + DATACNTR + NFLOOR + WLCNS + CHILLR + HDD65 + CDD65 + PBAPLUS + PBA + OPEN24,data=trainData,ntree=500,mtry = 4,importance=T)
  
  # Making Predictions
  random.forest.pred.fl<-predict(random.forest.model.fl,newdata=trainData)
  random.forest.pred.fl.OS<-predict(random.forest.model.fl,newdata=testData)
  
  rmse.train.RF.fl[i]<-rmse(actual = trainData$ELECTRICITY,predicted = random.forest.pred.fl)
  rmse.test.RF.fl[i]<-rmse(actual = testData$ELECTRICITY,predicted = random.forest.pred.fl.OS)
  
  plot(random.forest.pred.fl.OS, testData$ELECTRICITY, xlab = "predicted rf", ylab = "actual.test", main=c(i, "test"))
  abline(a=0,b=1)
  
  plot(random.forest.model.fl)
  partialPlot(random.forest.model.fl, testData,ELEXP,main=paste("Partial Dependence Plot"), ylab = "Elec")
  partialPlot(random.forest.model.fl, testData,NFLOOR,main=paste("Partial Dependence Plot"), ylab = "Elec")
  partialPlot(random.forest.model.fl, testData,DATACNTR,main=paste("Partial Dependence Plot"), ylab = "Elec")
  partialPlot(random.forest.model.fl, testData,CHILLR,main=paste("Partial Dependence Plot"), ylab = "Elec")
  partialPlot(random.forest.model.fl, testData,OPEN24,main=paste("Partial Dependence Plot"), ylab = "Elec")
  
  
  ################################ Model 4-SVM ################################
  # Fitting the Model
  svm.model <- svm(ELECTRICITY ~., data = trainData, kernel = "radial", cost=10, gamma=0.5)
  
  qqnorm(resid(svm.model))
  qqline(resid(svm.model), main = c("qq svm",i))
  
  # Making Predictions
  svm.model.pred <- predict(svm.model, trainData)
  svm.model.pred.OS <- predict(svm.model, testData)
  
  rmse.train.SVM[i] <- rmse(actual = trainData$ELECTRICITY, predicted = svm.model.pred)
  rmse.test.SVM[i] <- rmse(actual = testData$ELECTRICITY, predicted = svm.model.pred.OS)
  
  plot(svm.model.pred.OS, testData$ELECTRICITY, xlab = "predicted svm", ylab = "actual.test", main=c(i, "test"))
  abline(a=0,b=1)

  
 
   
  ################################ Model 5-NNET ################################
   myGrid <-  expand.grid(size = seq(from = 1, to = 10, by = 1)
                          , decay = c(0.5, 0.1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7))

    # Fitting the Model
   neural.nets.model <- train(ELECTRICITY ~ .,data = trainData,method = "nnet",tuneGrid = myGrid,maxit = 500,linout = 1)

   qqnorm(resid(neural.nets.model))
   qqline(resid(neural.nets.model))

   # Making Predictions
   neural.nets.model.pred <- predict(neural.nets.model, trainData)
   neural.nets.model.pred.OS <- predict(neural.nets.model, testData)

   rmse.train.NNET[i] <- rmse(actual = trainData$ELECTRICITY, predicted = neural.nets.model.pred)
   rmse.test.NNET[i] <- rmse(actual = testData$ELECTRICITY, predicted = neural.nets.model.pred.OS)

   plot(neural.nets.model.pred.OS, testData$ELECTRICITY, xlab = "predicted nnet", ylab = "actual.test", main=c(i, "test"))
   abline(a=0,b=1)



  ################################ Model 6-MARS(Unpruned) ################################
  mars.unpruned.model<- earth(ELECTRICITY~ ELEXP + DATACNTR + HDD65 + NFLOOR + CHWT + BLDSHP + SQFT + GLSSPC  + MAINHT + RFCOOL, data=trainData,pmethod = 'none')
  
  qqnorm(resid(mars.unpruned.model))
  qqline(resid(mars.unpruned.model))
  
  # Making Predictions
  mars.unpruned.model.pred <- predict(mars.unpruned.model, trainData)
  mars.unpruned.model.pred.OS <- predict(mars.unpruned.model, testData)  
  
  rmse.train.UPMARS[i] <- rmse(actual = trainData$ELECTRICITY, predicted = mars.unpruned.model.pred)
  rmse.test.UPMARS[i] <- rmse(actual = testData$ELECTRICITY, predicted = mars.unpruned.model.pred.OS)
  
  
  
  
  
  ################################ Model 7-MARS(Pruned) ################################
  mars.pruned.model<-earth(ELECTRICITY~ ELEXP + DATACNTR + HDD65 + NFLOOR + CHWT + BLDSHP + SQFT + GLSSPC + MAINHT + RFCOOL, data=trainData)

  qqnorm(resid(mars.pruned.model))
  qqline(resid(mars.pruned.model))

  # Making Predictions
  mars.pruned.model.pred <- predict(mars.pruned.model, trainData)
  mars.pruned.model.pred.OS <- predict(mars.pruned.model, testData)  
  
  rmse.train.PMARS[i] <- rmse(actual = trainData$ELECTRICITY, predicted = mars.pruned.model.pred)
  rmse.test.PMARS[i] <- rmse(actual = testData$ELECTRICITY, predicted = mars.pruned.model.pred.OS)
  
  }



################################ Model 8-BART ################################

options(java.parameters = "-Xmx3000m")
set_bart_machine_num_cores(2)

folds <- cut(seq(1,nrow(final.df)),breaks=10,labels=FALSE)
rmse.test.BART <- vector("numeric", 10)
rmse.train.BART <- vector("numeric", 10)

for(i in 1:10)
{
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- final.df[testIndexes, ]
  trainData <- final.df[-testIndexes, ]
  
  train.covariates <- trainData[,-1]
  train.response <- trainData$ELECTRICITY
  
  bart.model <- bartMachine(X=train.covariates, y=train.response, k=2, q=0.99,num_trees=200, serialize = T )
  
  bart.model.predict <- predict(bart.model, trainData[,-1])
  bart.model.predict.OS <- predict(bart.model, testData[,-1])
  
  rmse.train.BART[i] <- rmse(actual = trainData$ELECTRICITY, predicted = bart.model.predict)
  rmse.test.BART[i] <- rmse(actual = testData$ELECTRICITY, predicted = bart.model.predict.OS)
  
  plot(bart.model.predict.OS, testData$ELECTRICITY, xlab = "predicted bart", ylab = "actual.test", main=c(i, "test"))
  abline(a=0,b=1)
  
}  

all.models.folds.OS<-data.frame((rmse.test.LM),(rmse.test.DT),(rmse.test.RF.fl),(rmse.test.SVM),(rmse.test.NNET),(rmse.test.UPMARS),(rmse.test.PMARS),(rmse.test.BART))
colnames(all.models.folds.OS)<-c("LM","DT","RF","SVM","NN","MARS(Unpruned)","MARS(Pruned)", "BART")
rownames(all.models.folds.oS)<-"RMSE(10 Folds)"
all.models.folds.OS



all.models.folds.IS<-data.frame((rmse.train.LM),(rmse.train.DT),(rmse.train.RF.fl),(rmse.train.SVM),(rmse.train.NNET),(rmse.train.UPMARS),(rmse.train.PMARS),(rmse.train.BART))
colnames(all.models.folds.IS)<-c("LM","DT","RF","SVM","NN","MARS(Unpruned)","MARS(Pruned)", "BART")
rownames(all.models.folds.IS)<-"RMSE(10 Folds)"
all.models.folds.IS

folds<-rbind(all.models.folds.IS,all.models.folds.OS)
write.csv(folds,"ErrorValues.csv",row.names = FALSE)


all.models<-data.frame(mean(rmse.train.LM),mean(rmse.train.DT),mean(rmse.train.RF.fl),mean(rmse.train.SVM),mean(rmse.train.NNET),mean(rmse.train.UPMARS),mean(rmse.train.PMARS), mean(rmse.train.BART))
colnames(all.models)<-c("LM","DT","RF","SVM","NN","MARS(Unpruned)","MARS(Pruned)","BART")
rownames(all.models)<-"RMSE(In Sample)"
all.models


all.models.OS<-data.frame(mean(rmse.test.LM),mean(rmse.test.DT),mean(rmse.test.RF.fl),mean(rmse.test.SVM),mean(rmse.test.NNET),mean(rmse.test.UPMARS),mean(rmse.test.PMARS), mean(rmse.test.BART))
colnames(all.models.OS)<-c("LM","DT","RF","SVM","NN","MARS(Unpruned)","MARS(Pruned)", "BART")
rownames(all.models.OS)<-"RMSE(Out of Sample)"
all.models.OS




################################ Final Model ################################ 
rmse.test.final.fl <- vector("numeric", 10)
rmse.train.final.fl <- vector("numeric", 10)

folds <- cut(seq(1,nrow(final.df)),breaks=10,labels=FALSE)
#Perform 10 fold cross validation
for(i in 1:10)
{
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- final.df[testIndexes, ]
  trainData <- final.df[-testIndexes, ]
  
  final.model<-randomForest(ELECTRICITY ~ ELEXP + SQFT + DATACNTR + NFLOOR + WLCNS + CHILLR + HDD65 + CDD65 + PBAPLUS + PBA + OPEN24,data=trainData,ntree=500,mtry = 4,importance=T)

  # Making Predictions
  final.model.pred.fl<-predict(final.model,newdata=trainData)
  final.model.pred.fl.OS<-predict(final.model,newdata=testData)
  
  rmse.train.final.fl[i]<-rmse(actual = trainData$ELECTRICITY,predicted = final.model.pred.fl)
  rmse.test.final.fl[i]<-rmse(actual = testData$ELECTRICITY,predicted = final.model.pred.fl.OS)
  
  plot(final.model.pred.fl.OS, testData$ELECTRICITY, xlab = "predicted rf", ylab = "actual.test", main=c(i, "test"))
  abline(a=0,b=1)
  
  plot(final.model)
  partialPlot(final.model, testData,ELEXP,main=paste("Partial Dependence Plot"), ylab = "Elec")
  partialPlot(final.model, testData,NFLOOR,main=paste("Partial Dependence Plot"), ylab = "Elec")
  partialPlot(final.model, testData,DATACNTR,main=paste("Partial Dependence Plot"), ylab = "Elec")
  partialPlot(final.model, testData,CHILLR,main=paste("Partial Dependence Plot"), ylab = "Elec")
  partialPlot(final.model, testData,OPEN24,main=paste("Partial Dependence Plot"), ylab = "Elec")
  
}  

saveRDS(final.model, file = "0029971620.RData")









