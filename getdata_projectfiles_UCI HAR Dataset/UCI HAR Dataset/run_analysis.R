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
#colnames(trainAndTestDf) <- newColNames