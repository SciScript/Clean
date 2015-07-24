## 1. Setting the working directory and load the libraries.
setwd("C:/Users/Nina/Documents/DataScience/Coursera Course/3. Getting and Cleaning Data/Projekt/UCI HAR Dataset")
library(plyr)
library(dplyr)
library(tidyr)

## 2.Read in the main datasets X_test.txt and X_train.txt.

test <- read.table("test/X_test.txt")
train <- read.table("train/X_train.txt")

## 3. Introduce the column names from features.txt to the datasets test and train.

features <- read.table("features.txt", stringsAsFactors = FALSE)
names(test) <- features$V2
names(train) <- features$V2

## 4. Reduction of the datasets to columns containing the mean and the standard deviation
# of the respective values. 

seletest <- test[,grepl("mean()|std()", names(test))] 
seltest <- seletest[,!grepl("meanFreq", names(seletest))] 
seletrain <- train[,grepl("mean()|std()", names(train))]
seltrain <- seletrain[,!grepl("meanFreq", names(seletrain))]

## 5. Introduction and tidying of the activity labels.
# Read in the files y_test or y_train that connect the coded activity with an index for the
# respective observation. 
activitytrain <- read.table("train/y_train.txt")
activitytest <- read.table("test/y_test.txt")
# Read in the dataset activity_labels that connects the activities with their codes.
activitylink <- read.table("activity_labels.txt")
# Introduce an index variable for the observations of the datasets activitytrain and activitytest.
activitytrain$index <- seq(1:nrow(activitytrain))
activitytest$index <- seq(1:nrow(activitytest))
# Rename the activities with "tidy names" (all lowercase letters).
activitylink$V2 <- tolower(activitylink$V2)   
# Creat a final dataset of the row labels by merging the datasets activitylink and 
# activitytest/activitytrain, respectively, by their index.
testlab <- merge(activitylink, activitytest, by.x="V1", by.y="V1")
trainlab <- merge(activitylink, activitytrain, by.x="V1", by.y="V1")
# Sort the datasets testlab and trainlab by the index.
testlab <- arrange(testlab, index)
trainlab <- arrange(trainlab, index)

## 6. Indroduction of the subject labels to the datasets testlab and trainlab.
# Read in the files subject_train.txt and subject_test.txt that connect the subject number with 
# the observation index. 
subjecttrain <- read.table("train/subject_train.txt")
subjecttest <- read.table("test/subject_test.txt")
# Add a column with the subject labels to the datasets testlab and trainlab. 
testlab$V3 <- subjecttest$V1
trainlab$V3 <- subjecttrain$V1
# Rename the columns with tidy and descriptive names
names(testlab) <- c("labelcode", "activity", "index", "subject")
names(trainlab) <- c("labelcode", "activity", "index", "subject")

## 7. Introduction of the respective subject and activity labels to the datasets seltest and 
# seltrain, respectively.
subject <- testlab$subject
activity <- testlab$activity
seltest <- cbind(subject, activity, seltest)

subject <- trainlab$subject
activity <- trainlab$activity
seltrain <- cbind(subject, activity, seltrain)

## 8. Combines the datasets seltest and seltrain to a complete dataset containing all test and
# train measurements.
final <- rbind(seltest, seltrain)      

## 9. Cleaning up the column names of the final dataset to "tidy names"(removal of special 
# symbols, keep upper case letters only for clarity)
names(final) <- sub("\\()", "", names(final))
names(final) <- sub("-X", "X", names(final))
names(final) <- sub("-Y", "Y", names(final))
names(final) <- sub("-Z", "Z", names(final))
names(final) <- sub("-", "\\.", names(final))
names(final) <- sub(".mean", "Mean", names(final))
names(final) <- sub(".std", "Std", names(final))
names(final) <- sub("fBodyBody", "fBody", names(final))

## 10. Group the resulting dataset final by subject and activity.
group <- group_by(final, subject, activity)

## 11. Summarise the dataset group to receive the mean of evry column in the dataset group for every 
# activity/subject pair.
tab <- summarise_each(group, funs(mean))

## 12. Create the final text file
write.table(tab, file = "NinaDataset.txt", sep = " ", row.name=FALSE)


