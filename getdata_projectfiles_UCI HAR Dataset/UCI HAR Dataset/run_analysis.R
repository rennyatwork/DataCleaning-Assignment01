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


## 1 - merging datasets
mergeTrainAndTest <- function()
{
  #test_x.csv is a modified file
  testDf<- read.table("./test/test_X.csv", header=FALSE, sep=",")
  #testDf <- getMergedTrainOrTest("test")
  
  trainDf <- read.table("./train/train_X.csv", header=FALSE, sep=",")
  #trainDf <- getMergedTrainOrTest("train")
  
  mainMerge <- getMergedTrainAndTest("MAIN")
  labelsMerge <- getMergedTrainAndTest("LABELS")
  subjectssMerge <- getMergedTrainAndTest("SUBJECTS")
  
  #print("testDf")
  #print(ncol(testDf))
  
  #print("trainDf")
  #print(ncol(trainDf))
  
  trainAndTestDf <- merge(testDf, trainDf, all=T)
  
  return(trainAndTestDf)
}

### This functions reads files from either Train or Test
### and returns a merged dataset (yTrain, subjectTrain, activityLabels)
getMergedTrainOrTest <- function(pTrainOrTest)
{
  #
  dfActivityLabels <- loadFile("activity_labels.txt")
  
  #file containing the 561 columns
  mainDf <- loadFile( paste( paste(pTrainOrTest, paste("/X_", pTrainOrTest, sep=""), sep=""), ".txt", sep=""))
  
  #leaving only mean and std
  mainDf <- getOnlyMeanAndStdDf(mainDf)
  
  
  #labels from 1 to 6
  labelsDf <- loadFile(paste( paste(pTrainOrTest,paste("/y_", pTrainOrTest, sep=""), sep=""), ".txt", sep=""))
  
  #subjects from 1 to 30
  subjectsDf <- loadFile(paste( paste(pTrainOrTest,paste("/subject_", pTrainOrTest, sep=""), sep=""), ".txt", sep=""))
  
  #merging all the activities with the corresponding labels
  dfLabelsNames <- merge(labelsDf, dfActivityLabels)
  
  #adding the subject column
  mainDf$subject <- subjectsDf
  
  #adding the activty code (1-6)
  mainDf$ActivityCode <- dfLabelsNames[, 1]
  
  #adding the activity name
  mainDf$TrainActivityLabel <- dfLabelsNames[, 2]
  
  return (mainDf)
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
    subctsTrainDf <- loadFile(getSubjectsFilenaName("Train"))
    subjectsTestDf <- loadFile(getSubjectsFilenaName("Test"))
    #return (merge(subctsTrainDf, subjectsTestDf, all=T))
    return (rbind(subctsTrainDf, subjectsTestDf))
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
  dfActivityLabels <- loadFile("activity_labels.txt")
 
  
  #merging all the activities with the corresponding labels
  dfLabelsNames <- merge(pLabelsDf, dfActivityLabels)
  
  #adding the subject column
  pMainDf$subject <- pSubjectsDf
  
  #adding the activty code (1-6)
  pMainDf$ActivityCode <- dfLabelsNames[, 1]
  
  #adding the activity name
  pMainDf$TrainActivityLabel <- dfLabelsNames[, 2]
  
  return(pMainDf)
}

#initializeEnvironment()
#trainAndTestDf <- mergeTrainAndTest()
#reducedDf <- getOnlyMeanAndStdDf(trainAndTestDf)
#joinedReducedDf <- tbl_df(reducedDf)


#mainMerge <- getMergedTrainAndTest("MAIN")
#labelsMerge <- getMergedTrainAndTest("LABELS")
#subjectsMerge <- getMergedTrainAndTest("SUBJECTS")
#mainMerge <- getOnlyMeanAndStdDf(mainMerge)
#fullyMerged <- getHorizontalyMergedDf(mainMerge, labelsMerge, subjectssMerge)
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