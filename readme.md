# Getting and Cleaning Data, Peer Assessment Project

## <u>How to regenerate tidy data:</u>
1. Clone the project, the data files are in UCI HAR Dataset folder
2. Execute the script run_analysis.R from the root directory
3. Examine tidyData.txt as the result of run_analysis.R in the root directory


## <u>Steps performed in R</u>

#### 0. Preparation: load packages and define path
```r
packages <- c("data.table", "reshape2")
sapply(packages, require, character.only=TRUE, quietly=TRUE)
path <- getwd()
```
#### 1. Merge the training and the test sets to create one data set.
```r
dataFilesPath       <- file.path(path, "UCI HAR Dataset")
dataTrainSubject    <- fread(file.path(dataFilesPath, "train", "subject_train.txt"))
dataTrainActivity   <- fread(file.path(dataFilesPath, "train", "Y_train.txt"))
dataTestSubject     <- fread(file.path(dataFilesPath, "test" , "subject_test.txt" ))
dataTestActivity    <- fread(file.path(dataFilesPath, "test" , "Y_test.txt" ))

toDataTable <- function (file) {
	dataFile    <- read.table(file)
	dataTable   <- data.table(dataFile)
}
trainDataTable  <- toDataTable(file.path(dataFilesPath, "train", "X_train.txt"))
testDataTable   <- toDataTable(file.path(dataFilesPath, "test" , "X_test.txt" ))

dataTableSubject <- rbind(dataTrainSubject, dataTestSubject)
setnames(dataTableSubject, "V1", "subject")
dataTableActivity <- rbind(dataTrainActivity, dataTestActivity)
setnames(dataTableActivity, "V1", "activityNum")
dataTable <- rbind(trainDataTable, testDataTable)
dataTableSubject <- cbind(dataTableSubject, dataTableActivity)
dataTable <- cbind(dataTableSubject, dataTable)
setkey(dataTable, subject, activityNum)
```
#### 2. Extract only the measurements on the mean and standard deviation for each measurement.
```r
dataTableFeatures <- fread(file.path(dataFilesPath, "features.txt"))
setnames(dataTableFeatures, names(dataTableFeatures), c("featureNum", "featureName"))
dataTableFeatures <- dataTableFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]

dataTableFeatures$featureCode <- dataTableFeatures[, paste0("V", featureNum)]
select <- c(key(dataTable), dataTableFeatures$featureCode)
dataTable <- dataTable[, select, with=FALSE]
```
#### 3. Use descriptive activity names to name the activities in the data set
```r
dataTableActivityNames <- fread(file.path(dataFilesPath, "activity_labels.txt"))
setnames(dataTableActivityNames, names(dataTableActivityNames), c("activityNum", "activityName"))
```
#### 4. Appropriately label the data set with descriptive variable names.
```r
dataTable        <- merge(dataTable, dataTableActivityNames , by="activityNum", all.x=TRUE)
setkey(dataTable, subject, activityNum, activityName)
dataTable        <- data.table(melt(dataTable, key(dataTable), variable.name="featureCode"))
dataTable           <- merge(dataTable, dataTableFeatures[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)
dataTable$activity  <- factor(dataTable$activityName)
dataTable$feature   <- factor(dataTable$featureName)
```
#### 5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.
```r
grepFeature <- function (regex) {
  grepl(regex, dataTable$feature)
}
n <- 2
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepFeature("^t"), grepFeature("^f")), ncol=nrow(y))
dataTable$featDomain <- factor(x %*% y, labels=c("Time", "Freq"))
x <- matrix(c(grepFeature("Acc"), grepFeature("Gyro")), ncol=nrow(y))
dataTable$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepFeature("BodyAcc"), grepFeature("GravityAcc")), ncol=nrow(y))
dataTable$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))

x <- matrix(c(grepFeature("mean()"), grepFeature("std()")), ncol=nrow(y))
dataTable$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))
dataTable$featJerk <- factor(grepFeature("Jerk"), labels=c(NA, "Jerk"))
dataTable$featMagnitude <- factor(grepFeature("Mag"), labels=c(NA, "Magnitude"))
n <- 3
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepFeature("-X"), grepFeature("-Y"), grepFeature("-Z")), ncol=nrow(y))
dataTable$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))
setkey(dataTable, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
tidyData <- dataTable[, list(count = .N, average = mean(value)), by=key(dataTable)]
```
#### 6. Write result to data file (tidyData.txt)
```r
write.table(tidyData, "tidyData.txt")
```
