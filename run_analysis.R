

#Loading the original data
xtrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
xtest <- read.table("./UCI HAR Dataset/test/X_test.txt")
#Features
features <- read.table("./UCI HAR Dataset/features.txt")
#Appending dataframes
wholeDs <- rbind(xtrain,xtest)
#Naming the dataset columns.
wholeDs <- setNames(wholeDs,features[,"V2"])
wantedCols <- grepl("-mean\\(\\)|-std\\(\\)",names(wholeDs))
#Taking just the mean and std columns.
filteredDs <- wholeDs[,wantedCols]
#Loading activity labels
ytrain <- read.table("./UCI HAR Dataset/train/y_train.txt", col.names = c("activityid"))
ytest <- read.table("./UCI HAR Dataset/test/y_test.txt", col.names = c("activityid"))
#Appending labels
wholeLabels <- rbind(ytrain,ytest)
filteredDs$activityid <- wholeLabels[,"activityid"]
#Label descriptions
actVect <- c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")
actFact <- factor(actVect,levels = actVect)
getActDesc <- function(idDesc){ 
  
  return(actFact[[idDesc]])
}
#Adding Activity Description Column
filteredDs$activitydescription  <- sapply(filteredDs[,"activityid"], FUN = getActDesc)
#Descriptive Variable Names
colNames <- names(filteredDs)
#colnames(filteredDs) <- sub("^t","time",colNames)
colNames <- sub("^t","time",colNames)
colNames <- sub("Acc","accelerometer",colNames)
colNames <- sub("Mag","magnitude",colNames)
colNames <- sub("Gyro","gyroscope",colNames)
colNames <- sub("-mean\\(\\)-*","mean",colNames)
colNames <- sub("^f","frequency",colNames)
colNames <- sub("-std\\(\\)-*","std",colNames)
colNames <- sub("BodyBody","body",colNames)
colNames <- tolower(colNames)

#####FILTERED DATA SET#####
colnames(filteredDs) <- colNames

###Second DataSet
avgDs <- filteredDs[,]
#Subjects
subjectTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt", col.names = c("subjectid"))
subjectTest <- read.table("./UCI HAR Dataset/test/subject_test.txt", col.names = c("subjectid"))
#Appending Subjects
wholeSubjects <- rbind(subjectTrain,subjectTest)
avgDs$subjectid <- wholeSubjects[,"subjectid"]
groupAvgDs <- group_by(avgDs,activitydescription,subjectid)

#####SUMMARIZED DATA SET#####
sumAvgDs <- summarize_each(groupAvgDs, funs(mean))

write.table(sumAvgDs,"activityRecognitionDataset.txt", row.names = FALSE)


