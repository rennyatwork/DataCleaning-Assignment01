## 0 - initializing environment
initializeEnvironment <-function()
{
  require(dplyr)
  
}
## 1 - merging datasets
mergeTrainAndTest <- function()
{
  #test_x.csv is a modified file
  testDf<- read.table("./test/test_X.csv", header=FALSE, sep=",")
  
  trainDf <- read.table("./train/train_X.csv", header=FALSE, sep=",")
  
  trainAndTestDf <- merge(testDf, trainDf, all=T)
  
  return(trainAndTestDf)
}

## 2 - returns dataframe containing only measurements on mean and std
getOnlyMeanAndStdDf <-function(pTrainAndTestDf)
{
  #getting headers
  headersDf <- read.table("features.txt", header=F, sep="\n")  
  
  #cutting off the numbers prefixing the columns' names
  newColNames <- sub("\\d+ ", "", headersDf[,1])
  
  colnames(pTrainAndTestDf) <- newColNames
  #renaming columns in trainAndTestDf  
  # indexes of columns matching either mean or std
  meanAndStdCols <- grep("mean|std", newColNames ,ignore.case=T, value=F)
  
  reducedDf <- pTrainAndTestDf[,meanAndStdCols]
  
  return (reducedDf)
}

#initializeEnvironment()
#trainAndTestDf <- mergeTrainAndTest()
#reducedDf <- getOnlyMeanAndStdDf(trainAndTestDf)
##################

#require(dplyr)
# setwd("C:\\Users\\renato\\Documents\\MOOC\\2014\\course03 - DataCleaning\\Assignment-01\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\")

#loading test data
#testDf <- read.table("test/test_X.csv", header=FALSE, sep=",")
#ncol(testDf)

#loading training data
# trainDf <- read.table("train/train_X.csv", header=FALSE, sep=",")
# ncol(trainDf)

#trainAndTestDf <- merge(testDf, trainDf, all=T)

#setwd("C:\\Users\\renato\\Documents\\MOOC\\2014\\course03 - DataCleaning\\Assignment-01\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset")
#headersDf <- read.table("features.txt", header=F, sep="\n")

#cutting off numbers before colum names
#sub("\\d+ ", "", headersDf[,1])
#newColNames <- sub("\\d+ ", "", headersDf[,1])

#renaming columns in trainAndTestDf

# indexes of columns matching either mean or std
#meanAndStdCols <- grep("mean|std", newColNames ,ignore.case=T, value=F)
#reducedDf <- trainAndTestDf[meanAndStdCols]

## listing all .txt
#list.files(path="./train/Inertial Signals", ignore.case=T, pattern=".txt")