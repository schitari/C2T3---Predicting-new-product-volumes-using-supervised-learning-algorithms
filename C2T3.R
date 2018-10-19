install.packages("xlsx")
library(readr)
library(ggplot2)
library(lattice)
library(caret)
install.packages("corrplot")
install.packages("stats")
library(corrplot)
library(dplyr)
library(ggplot2)
library(xlsx)
library(stats)

set.seed(39)
existingproductattributes2017 <- read_csv("existingproductattributes2017.csv")
summary(existingproductattributes2017)
plot (existingproductattributes2017$BestSellersRank)
ggplot(existingproductattributes2017,aes(x=existingproductattributes2017$BestSellersRank)) + geom_histogram(na.rm = TRUE, show.legend = TRUE)
#######################################################################################################################################################
#get a subset of the data frame with only the observations with BestSellersRank populated
#This is done to help impute the bservations with NA as best sellers rank to complete the dataset if possible.
exist_subset = subset(existingproductattributes2017, !is.na(existingproductattributes2017$BestSellersRank))

#convert the exist_subset categorical attributes to factors
exist_subset$ProductType = as.factor(exist_subset$ProductType)
exist_subset$ProductNum = as.factor(exist_subset$ProductNum)

#dummyfy the non-numeric attributes
newdatframe = dummyVars(" ~ .", data = exist_subset)
readyData <- data.frame(predict(newdatframe, newdata = exist_subset))
readyData

#remove the product number column from the dataset
exist_subset_no_prodNum = subset(exist_subset,select =-ProductNum)
#remove the highest values od best sellers rank
boxplot(exist_subset_no_prodNum$BestSellersRank)
summary(exist_subset_no_prodNum)

outlier_values <- boxplot.stats(exist_subset_no_prodNum$BestSellersRank)$out  # outlier values.
boxplot(exist_subset_no_prodNum$BestSellersRank, main="Best Sellers Rank", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)


exist_subset_no_prodNum = subset(exist_subset_no_prodNum, !exist_subset_no_prodNum$BestSellersRank %in% c(17502, 14086, 12076, 6295, 5742))
exist_subset_no_prodNum
#dummyfy the non-numeric attributes
newdatframe = dummyVars(" ~ .", data = exist_subset_no_prodNum)
readyData <- data.frame(predict(newdatframe, newdata = exist_subset_no_prodNum))
readyData


#find correlation between all populated attributes.
#Find correlation in the columns
CorrData = cor(readyData)
corrplot(CorrData)

#train on data set to predict best sellers rank
##Split data for training and testing
respIndices = createDataPartition(readyData$BestSellersRank, p=0.75,list = FALSE)
resp_75_train = readyData[respIndices,]
resp_25_test =  readyData[-respIndices,]

#apply 10 fold cross validation
fitcontrol = trainControl(method ="repeatedcv", number = 10, repeats = 1)

rfTrain10 = train(BestSellersRank~.,data = resp_75_train, method ='rf', trControl= fitcontrol)
rfTrain10
###############################################################################################################################
#After trying to find if there is a good confidence level to predict best sellers rank missing values we conclude that
#the confidence level to predict that column is under 50% hence we will exclude that column from our analysis.

# we will exclude product Number and Best sellers Rank from analysis for now.
exist_subset_no_prodNum = subset (existingproductattributes2017, select = -BestSellersRank)
exist_subset_no_prodNum$ProductNum = NULL
exist_subset_no_prodNum


#find outliers in data
outlier_values <- boxplot.stats(existingproductattributes2017$Volume~existingproductattributes2017$ProductType)$out  # outlier values.
boxplot(existingproductattributes2017$Volume ~ existingproductattributes2017$ProductType, main="Volume Vs Product Type", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)
#The below code will show the outliers on the plot. Just running the tapply portion will show the min/max/quartile numbers. 
#Once you have a list of outliers , calculate to see if they are acually outside range 
boxplot(tapply(existingproductattributes2017$Volume,existingproductattributes2017$ProductType, summary))
# we found 4 outliers - Accessories with Volume 11204, Printer with Volume = 824, Smartphone with Volume = 1472 and ExtendedWarranty with volume = 0
#remove those from the data
exist_subset_no_prodNum= subset(exist_subset_no_prodNum, !exist_subset_no_prodNum$Volume %in% c(11204,824,1472,7036))
boxplot(tapply(exist_subset_no_prodNum$Volume,exist_subset_no_prodNum$ProductType, summary))

#Check the distribution of the other attributes 
boxplot(tapply(exist_subset_no_prodNum$Price,exist_subset_no_prodNum$ProductType, summary))
write.csv(exist_subset_no_prodNum,"C:/Users/Chitari/Documents/Data analytics Course/Deliverables/Course 2/T3/exist_subset_no_prodNum.csv",row.names=FALSE)

####################################################################################################################################
#Normalization worsened the predictions on the test set from the training portion. So we will not normalize the data
#dummyfy the non-numeric attributes of the normalized dataset
newdatframe = dummyVars(" ~ .", data = normdataset)
readyData <- data.frame(predict(newdatframe, newdata = normdataset))
readyData
####################################################################################################################################
#dummyfy the non-numeric attributes
newdatframe = dummyVars(" ~ .", data = exist_subset_no_prodNum)
readyData <- data.frame(predict(newdatframe, newdata = exist_subset_no_prodNum))
readyData

#find correlation between all populated attributes.
#Find correlation in the columns
CorrData = cor(readyData)
corrplot(CorrData)
CorrData
hc = findCorrelation(CorrData, cutoff = 0.9)
hc
exist_remove_corr = readyData[,-c(hc)]
print (exist_remove_corr)
corrplot(cor(exist_remove_corr),method = c("number"),type ="lower" , mar = c(0,0,0,0), title = "Correlation Heat map - No Collinearity")

#normalize data set except the Volume column
#exist_remove_corr
#mean(exist_remove_corr$Price)
#sd(exist_remove_corr$Price)
#exist_remove_corr$Price = ((exist_remove_corr$Price - mean(exist_remove_corr$Price))/(sd(exist_remove_corr$Price)))
#summary(exist_remove_corr)

##Split data for training and testing
respIndices = createDataPartition(exist_remove_corr$Volume, p=0.75,list = FALSE)
resp_75_train = exist_remove_corr[respIndices,]
resp_25_test =  exist_remove_corr[-respIndices,]


#apply 10 fold cross validation
fitcontrol = trainControl(method ="repeatedcv", number = 10, repeats = 1)

#train SVM. SVM performed poorly with RMSE = 464 and R2 = 0.80
system.time(svmTrain1 <- train(Volume~., data = resp_75_train, method ="svmLinear", trControl = fitcontrol, tuneLength = 10))
svmTrain1
plot(svmTrain1)
plot(varImp(svmTrain1))

#train rf. after some trys mtry= 25 seemed to be the most optimum RMSE under 200 and R-squared between 0.92 and 0.95
tunegrid = expand.grid(mtry = 25)
system.time(rftrain <- train(Volume~., data = resp_75_train, method ="rf", trControl = fitcontrol, tuneGrid = tunegrid))
rftrain
plot(rftrain)
plot(varImp(rftrain))

# train gbm. after some trys interaction.depth = 7, n.trees = 500 was the better with RMSE under 250 and R2 0.90
tunegrid = expand.grid(n.trees = 50, interaction.depth = 2, shrinkage = 0.1, n.minobsinnode = 10)
system.time(gbmtrain <- train(Volume~., data = resp_75_train, method ="gbm", trControl = fitcontrol, tuneGrid = tunegrid, tuneLength = 30))
gbmtrain
plot(gbmtrain)
plot(varImp(gbmtrain))

#predict using random forest gave a RMSE of over 200 and R2 = 0.81. Prediction with gbm gave an RMSE = 186 and R2 = 0.91. 
volumePred = predict(rftrain,resp_25_test, type = "raw")
volumePred

qplot(volumePred,resp_25_test$Volume, geom = "jitter")
postResample(volumePred,resp_25_test$Volume)

# read newproducts csv file
newproductattributes2017 <- read_csv("newproductattributes2017.csv")
# Apply the same preprocessing steps to it as we did during training.
# we will exclude product Number and Best sellers Rank from analysis for now.
new_subset_no_prodNum = subset (newproductattributes2017, select = -BestSellersRank)
new_subset_no_prodNum$ProductNum = NULL
new_subset_no_prodNum

# we will not normalize the dataset since during training normalization made the RMSE and R2 worse.

#dummyfy the non-numeric attributes
newNewdatframe = dummyVars(" ~ .", data = new_subset_no_prodNum)
newreadyData <- data.frame(predict(newNewdatframe, newdata = new_subset_no_prodNum))
newreadyData

# exclude the same attribute we excluded from the training due to correlation and collinearity
newreadyData$x5StarReviews = NULL
newreadyData$x4StarReviews = NULL
newreadyData$x2StarReviews = NULL
newreadyData$NegativeServiceReview = NULL

#do the predictions
finalPred = predict(rftrain,newreadyData, type = "raw")
finalPred

#qplot(finalPred,newreadyData$Volume, geom = "jitter")
#postResample(VolumePredictionsGBM,newreadyData$Volume)

#Move the predicted results to the csv file
output = newproductattributes2017
output$Predictions = finalPred
write.csv(output, file="C2T3outputNewPred.csv", row.names = TRUE)

boxplot(tapply(output$Predictions,output$ProductType, summary))

