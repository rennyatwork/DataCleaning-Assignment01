## 0 - initializing environment
initializeEnvironment <-function()
{
  require(dplyr)
  require(sqldf)
  
}

#### Aux methods
loadFile <- function(pFileName)
{
  return (read.table(pFileName, header=F, sep=""))
}


## 1 - merging datasets (test and train)
mergeTrainAndTest <- function()
{ 
  mainMerge <- getMergedTrainAndTest("MAIN")
  labelsMerge <- getMergedTrainAndTest("LABELS")
  
  dfActivityLabels <- loadFile("activity_labels.txt")
  labelsMerge[,1] <- factor(labelsMerge[,1], labels = dfActivityLabels[,2])
  
  subjectsMerge <- getMergedTrainAndTest("SUBJECTS")
  
  mainMerge <- getOnlyMeanAndStdDf(mainMerge)
  fullyMerged <- getHorizontalyMergedDf(mainMerge, labelsMerge, subjectsMerge)
  
  return(fullyMerged)
}

############################
## returns the X_[Train|Test].txt
getMainFileName <-function(pTrainOrTest)
{
  return (paste( paste(pTrainOrTest, paste("/X_", pTrainOrTest, sep=""), sep=""), ".txt", sep=""))
}

## returns the file path to y_[train|test].txt
getLabelsFileName <-function(pTrainOrTest)
{
  return(paste( paste(pTrainOrTest,paste("/y_", pTrainOrTest, sep=""), sep=""), ".txt", sep=""))
}

## returns subject_[train|test] filename
getSubjectsFilenaName <-function(pTrainOrTest)
{
  return(paste( paste(pTrainOrTest,paste("/subject_", pTrainOrTest, sep=""), sep=""), ".txt", sep=""))
}
###########################

### This functions reads files from either Train or Test
### and returns a merged dataset (train with test, subjectsTrain with sbujectsTest, labelsTrain with labelsTest)
getMergedTrainAndTest <- function(pDataSet)
{
  
  if (pDataSet=="MAIN")
  {
    #file containing the 561 columns
    mainTrainDf <- loadFile(getMainFileName("Train"))
    mainTestDf <- loadFile(getMainFileName("Test"))
    
    
    #return (merge(mainTrainDf, mainTestDf, all=T))
    return (rbind(mainTrainDf, mainTestDf))
  }
  
  
  else if (pDataSet=="LABELS")
  {
    #labels from 1 to 6
    labelsTrainDf <- loadFile(getLabelsFileName("Train"))
    labelsTestDf <- loadFile(getLabelsFileName("Test"))
    return (rbind(labelsTrainDf, labelsTestDf))
  }
  
  else if (pDataSet=="SUBJECTS")
  {
    #labels from 1 to 6
    subjectsTrainDf <- loadFile(getSubjectsFilenaName("Train"))
    subjectsTestDf <- loadFile(getSubjectsFilenaName("Test"))
    #return (merge(subctsTrainDf, subjectsTestDf, all=T))
    return (rbind(subjectsTrainDf, subjectsTestDf))
  }
  
 
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



####
#3 Horizontaly merged = adding subject and label columns
getHorizontalyMergedDf <-function(pMainDf, pLabelsDf, pSubjectsDf)
{ 
  
  #activity
  pMainDf <- cbind(pLabelsDf, pMainDf)
  
  #subject
  pMainDf <- cbind(pSubjectsDf, pMainDf)
  
  #renaming columns
  colnames(pMainDf)[1]<-"Subject"
  colnames(pMainDf)[2]<-"ActivityName"
  
  #adding the activity name
  #pMainDf$TrainActivityLabel <- dfLabelsNames[, 2]
  
  return(pMainDf)
}

## question 5
## This is to be called after obtaining a dataset (mergeTrainAndTest())
getTidyDataSet <-function(pMainDf)
{
  dfMeans <- aggregate(. ~ Subject + ActivityName, data = pMainDf, mean)
  ## first 2 columns are subject and activity. These don't need to be renamed
  
  lastCol <- 2+ncol(dfMeans[,3:ncol(dfMeans)])
  for(i in 3:lastCol)
  {
    colnames(dfMeans)[i] <- paste("AVG", colnames(dfMeans)[i], sep="_")    
    #print("colnames(dfMeans)[i]")
    #print(colnames(dfMeans)[i])
  }
  
  return (dfMeans)
}

#initializeEnvironment()
#trainAndTestDf <- mergeTrainAndTest()
#reducedDf <- getOnlyMeanAndStdDf(trainAndTestDf)
#joinedReducedDf <- tbl_df(reducedDf)
#sortedBySubjAct <- arrange(fullyMerged, Subject, ActivityLabel)
# using aggregate
#aggregate(sortedBySubjAct[,3:5], list(subj=sortedBySubjAct$Subject), mean)
# x <- aggregate(. ~ c(Subject, ActivityLabel), data = fullyMerged, mean)

#fullyMerged <- mergeTrainAndTest()
#x <- getTidyDataSet(fullyMerged)
###########
#mainMerge <- getMergedTrainAndTest("MAIN")
#labelsMerge <- getMergedTrainAndTest("LABELS")
#subjectsMerge <- getMergedTrainAndTest("SUBJECTS")
#mainMerge <- getOnlyMeanAndStdDf(mainMerge)
#fullyMerged <- getHorizontalyMergedDf(mainMerge, labelsMerge, subjectsMerge)
#tblFullyMerged <- tbl_df(fullyMerged)
# dfActivityLabelsNameCode <- merge (labelsMerge, dfActivityLabels)
# cut(fullyMerged$ActivityLabel, 6, dfActivityLabels[,2])
# aggregate(fullyMerged$ActivityLabel ~ fullyMerged$Subject, fullyMerged[, 3:ncol(fullyMerged)], mean)
# x <- aggregate(. ~ Subject + ActivityLabel, data = fullyMerged, mean)

# activ1 <- filter(tblFullyMerged, ActivityCode ==1)
# activ2 <- filter(tblFullyMerged, ActivityCode ==2)
# activ3 <- filter(tblFullyMerged, ActivityCode ==3)
# activ4 <- filter(tblFullyMerged, ActivityCode ==4)
# activ5 <- filter(tblFullyMerged, ActivityCode ==5)
# activ6 <- filter(tblFullyMerged, ActivityCode ==6)
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

##using aux to load files in train, test:
# dfYTrain <- loadFile("./train/y_train.txt")
# dfSubjectTrain <- loadFile("./train/subject_train.txt")
# dfActivityLabels <- loadFile("activity_labels.txt")
## getting unique values: unique()

##joining YTrain with activty labels
#dfTrainLabels <- merge(dfYTrain, dfActivityLabels)

## adding new column subject
#trainDf$subject <- dfSubjectTrain

## adding new columns train
# trainDf$activity <- dfYTrain

#test dataframe (pairing activities and subjects)
#subjAndActivLabels <- data.frame(c(labelsMerge), c(subjectsMerge))
