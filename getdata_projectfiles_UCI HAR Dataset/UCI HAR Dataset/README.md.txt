README
In order to run, you can download the whole solution from: https://github.com/rennyatwork/DataCleaning-Assignment01/tree/master/getdata_projectfiles_UCI%20HAR%20Dataset/UCI%20HAR%20Dataset

The script run_analys.R can be run by simply calling: generateTidyDataset(). This will produce the tidy dataset in the running directory. The output file is tidyDataset.txt. It’s a text file and it’s separator is “;”
How the script works:
It begins by merging “vertically” the Train and Test datasets. That means, merging:
- X_test with X_train
- Y_test with y_train
- Subject_test with subject_train
It’s mergeTrainAndTest() that does all these merges. 
After obtaining these 3 datasets,  mergeTrainAndTest() does the following:
1) It strips off every column from the Main dataset (the one with 561 columns) that is not related to “mean” or “std”
2) It calls getHorizontalMergeDf() to merge the 3 datasets into one. This last one containing Subject, Activity and all the measures
In order to obtain the tidy dataset, we can call getTidyDataSet() passing as a parameter the dataset obtained from mergeTrainAndTest() function. getTidyDataSet() renames all the measure columns by prefixing them with “AVG_”. It’s output is a text file (tidyDataSet.txt) containing 180 rows (30 subjects * 6 activities)
If you are only interested in the final result, you can simply call generateTidyDataset().and check for the tidyDataset.txt file. It’s separator is: “;”.

