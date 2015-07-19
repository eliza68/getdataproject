##Coursera Getting and Cleaning Data Course Project

##PART 1. download, read in and merge test and train datasets

##first read in data from local files
##this may need to be changed in the final project if we are supposed to write code to get them directly from the internet

X_train <- read.table("X_train.txt", quote="\"", stringsAsFactors=FALSE)
X_test <- read.table("X_test.txt", quote="\"", stringsAsFactors=FALSE)
subject_train<-read.table("subject_train.txt", quote="\"", stringsAsFactors=FALSE)
subject_test<-read.table("subject_test.txt", quote="\"", stringsAsFactors=FALSE)
y_train<-read.table("y_train.txt", quote="\"", stringsAsFactors=FALSE)
y_test<-read.table("y_test.txt", quote="\"", stringsAsFactors=FALSE)



##create id and activity variables in both train and test datasets
train <- X_train
train$id <- as.vector(subject_train[["V1"]])
train$act <-as.vector(y_train[["V1"]])

test <-X_test
test$id <- as.vector(subject_test[["V1"]])
test$act <-as.vector(y_test[["V1"]])

##Create merged dataset called “merged”

merged <- merge(train, test, all=TRUE)

##add variable or column names
features <- read.table("~/Desktop/Rprogramming/UCI HAR Dataset/features.txt", quote="\"", stringsAsFactors=FALSE)
features2<-as.vector(features[["V2"]])
features2<-make.names(features2, unique=T)
features2<-c(features2, "id", "act")
colnames(merged)<-features2


## PART 2. Extract only the measurements on the mean and standard deviation for each measurement.
library(dplyr)
meanvars<-select(merged, contains("mean"), id, act)
meanvars<-arrange(meanvars, id, act)
meanvars$order <- 1:nrow(meanvars) 
stdvars<-select(merged, contains("std"), id, act)
meanvars<-arrange(meanvars, id, act)
stdvars$order <- 1:nrow(stdvars) 
mergemeanstd<-merge(meanvars, stdvars, by="order")
yidact<-names(mergemeanstd) %in% c("id.y", "act.y") 
mergemeanstd<-mergemeanstd[,(!yidact)]

## PART 3. Uses descriptive activity names to name the activities in the data set

activity_labels <- read.table("~/Desktop/Rprogramming/UCI HAR Dataset/activity_labels.txt", quote="\"", stringsAsFactors=FALSE)
as.factor(activity_labels$V2)
mergemeanstd$act.x<-factor(mergemeanstd$act.x, levels=c(1, 2, 3, 4, 5, 6), labels=activity_labels$V2)

## PART 4. Appropriately label the data set with descriptive variable names.

newnames<- c("key", "BodyAccelerator_TimeXaxis.mean", "BodyAccelerator_TimeYaxis.mean", "BodyAccelerator_TimeZaxis.mean", "GravityAccelerator_TimeXaxis.mean", "GravityAccelerator_TimeYaxis.mean", "GravityAccelerator_TimeZaxis.mean", "BodyAcceleratorJerk_TimeXaxis.mean", "BodyAcceleratorJerk_TimeYaxis.mean", "BodyAcceleratorJerk_TimeZaxis.mean", "BodyGyroscope_TimeXaxis.mean", "BodyGyroscope_TimeYaxis.mean", "BodyGyroscope_TimeZaxis.mean", "BodyGyroscopeJerk_TimeXaxis.mean", "BodyGyroscopeJerk_TimeYaxis.mean", "BodyGyroscopeJerk_TimeZaxis.mean", "BodyAcceleratorMagnitude_Time.mean", "GravityAccMagnitude_Time.mean", "BodyAccJerkMagnitude_Time.mean", "BodyGyroMagnitude_Time.mean", "BodyGyroJerkMagnitude_Time.mean", "BodyAccelerator_FrequencyXaxis.mean", "BodyAccelerator_FrequencyYaxis.mean", "BodyAccelerator_FrequencyZaxis.mean", "BodyAccelerator_meanFreqXaxis", "BodyAccelerator_meanFreqYaxis", "BodyAccelerator_meanFreqZaxis", "BodyAccJerk_FrequencyXaxis.mean", "BodyAccJerk_FrequencyYaxis.mean", "BodyAccJerk_FrequencyZaxis.mean", "BodyAccJerk_meanFreqXaxis", "BodyAccJerk_meanFreqYaxis", "BodyAccJerk_meanFreqZaxis", "BodyGyro_FrequencyXaxis.mean", "BodyGyro_FrequencyYaxis.mean", "BodyGyro_FrequencyZaxis.mean", "BodyGyro_meanFreqXaxis", "BodyGyro_meanFreqYaxis", "BodyGyro_meanFreqZaxis", "BodyAccMagnitude_Frequency.mean", "BodyAccMagnitude_meanFreq", "BodyAccJerkMagnitude.mean", "BodyAccJerkMag_meanFreq", "BodyGyroMag_Frequency.mean", "BodyGyroMag_meanFreq", "BodyGyroJerkMag_Frequency.mean", "BodyGyroJerkMag_meanFreq", "BodyAccMean_Timeangle.gravity", "BodyAccJerkMean_Timeangle.gravityMean", "BodyGyroMean_Timeangle.gravityMean", "BodyGyroJerkMean_Timeangle.gravityMean", "angle.Xaxis.gravityMean", "angle.Yaxis.gravityMean", "angle.Zaxis.gravityMean", "subjectid", "activity", "BodyAcc_TimeXaxis.stdX", "BodyAcc_TimeYaxis.std", "BodyAcc_TimeZaxis.std", "GravityAcc_TimeXaxis.std", "GravityAcc_TimeYaxis.std", "GravityAcc_TimeZaxis.std",  "BodyAccJerk_TimeXaxis.std", "BodyAccJerk_TimeYaxis.std", "BodyAccJerk_TimeZaxis.std", "BodyGyro_TimeXaxis.std", "BodyGyro_TimeYaxis.std", "BodyGyro_TimeZaxis.std", "BodyGyroJerk_TimeXaxis.std", "BodyGyroJerk_TimeYaxis.std", "BodyGyroJerk_TimeZaxis.std", "BodyAccMag_Time.std", "GravityAccMag_Time.std", "BodyAccJerkMag_Time.std", "BodyGyroMag_Time.std", "BodyGyroJerkMag_Time.std", "BodyAccelerator_FrequencyXaxis.std", "BodyAccelerator_FrequencyYaxis.std", "BodyAccelerator_FrequencyZaxis.std", "BodyAccJerk_FrequencyXaxis.std", "BodyAccJerk_FrequencyYaxis.std", "BodyAccJerk_FrequencyZaxis.std", "BodyGyroscope_FrequencyXaxis.std", "BodyGyroscope_FrequencyYaxis.std", "BodyGyroscope_FrequencyZaxis.std", "BodyAccMagnitude_Frequency.std", "BodyAccJerkMag_Frequency.std", "BodyGyroMag_Frequency.std", "BodyGyroJerkMag_Frequency.std")
colnames(mergemeanstd)<-newnames


## PART 5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.

columns<-c("subjectid", "activity")
grouped<-group_by(mergemeanstd, subjectid, activity)
summary<-summarise_each(grouped, funs(mean))
summary<-select(summary, -key)
write.table(summary, file = "finaltidy.txt", row.names=FALSE)
