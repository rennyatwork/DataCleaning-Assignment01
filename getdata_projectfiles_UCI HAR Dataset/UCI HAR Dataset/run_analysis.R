## 0 - initializing environment
## This functions reads files into variables
loadFile <- function(pFileName)
{
  return (read.table(pFileName, header=F, sep=""))
}


## 1 - merging datasets (test and train)
## This function:
##  - gets a merge of the Main Test and Train datasets (the ones with 561 variables)
##  - gets a merge of the Activity labels (1-6) from Test and Train datasets
##  - gets the activity's names (WALKING, LAYING, etc)
##  - gets a merge of the Subjets Test and Train datasets
##  - Finally, with the Train and test datasets merged, it gets the merge of these 3 datasets
## (Main, Activity, Subject)

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
### Functions returning filenames to be read
###################

## This function returns the Main filename to be read.
## E.g., returns the X_[Train|Test].txt
getMainFileName <-function(pTrainOrTest)
{
  return (paste( paste(pTrainOrTest, paste("/X_", pTrainOrTest, sep=""), sep=""), ".txt", sep=""))
}

## This function returns the Activity's labelsfilename to be read.
## returns the file path to y_[train|test].txt
getLabelsFileName <-function(pTrainOrTest)
{
  return(paste( paste(pTrainOrTest,paste("/y_", pTrainOrTest, sep=""), sep=""), ".txt", sep=""))
}

## This function returns the Subject filename to be read
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
    #subjects from 1 to 30
    subjectsTrainDf <- loadFile(getSubjectsFilenaName("Train"))
    subjectsTestDf <- loadFile(getSubjectsFilenaName("Test"))    
    return (rbind(subjectsTrainDf, subjectsTestDf))
  }  
 
}



## This function trims the dataset so that it contains only columns having
## std or mean in its name.
## returns dataframe containing only measurements on mean and std
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
# This function merges the 3 datasets received as parameters.
# It merges "horizontaly" because it adds only columns
# Horizontaly merged = adding subject and label columns
getHorizontalyMergedDf <-function(pMainDf, pLabelsDf, pSubjectsDf)
{ 
  
  #activity
  pMainDf <- cbind(pLabelsDf, pMainDf)
  
  #subject
  pMainDf <- cbind(pSubjectsDf, pMainDf)
  
  #renaming columns
  colnames(pMainDf)[1]<-"Subject"
  colnames(pMainDf)[2]<-"ActivityName"
  
  return(pMainDf)
}

## question 5
## This function produces the tidy dataset with all the means per subject and per activity
## Besides creating a tidy dataset, it renames the columns by prefixing them with "AVG_"
## It then produces a filename calle tidyDataSet.txt
## The separator used in tidyDataset.txt is ";"
## This is to be called after obtaining a dataset (mergeTrainAndTest())
getTidyDataSet <-function(pMainDf)
{
  dfMeans <- aggregate(. ~ Subject + ActivityName, data = pMainDf, mean)
  ## first 2 columns are subject and activity. These don't need to be renamed
  
  lastCol <- 2+ncol(dfMeans[,3:ncol(dfMeans)])
  for(i in 3:lastCol)
  {
    colnames(dfMeans)[i] <- paste("AVG", colnames(dfMeans)[i], sep="_")        
  }
  
  write.table(dfMeans, "tidyDataSet.txt", sep=";", row.name=F)
  return (dfMeans)
}

# This function is a wrapper that generates the tidyDataset
# Simply run this function and check for the tidyDataset (tidyDataset.txt)
generateTidyDataset <- function()
{
  fullyMerged <- mergeTrainAndTest()
  x <- getTidyDataSet(fullyMerged)
}

##############
# To  test:
#fullyMerged <- mergeTrainAndTest()
#x <- getTidyDataSet(fullyMerged)
# or simply: generateTidyDataset()
###########
