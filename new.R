setwd("C:/Users/Nina/Documents/DataScience/Coursera Course/3. Getting and Cleaning Data/Projekt/UCI HAR Dataset")

library(plyr)
library(dplyr)
library(tidyr)

# read in the main datasets X_test.txt and X_train.txt

test <- read.table("test/X_test.txt")
train <- read.table("train/X_train.txt")

# Introduce the column names from features.txt

features <- read.table("features.txt", stringsAsFactors = FALSE)
names(test) <- features$V2
names(train) <- features$V2

## Reduction of the datasets to columns containing the mean and the standard deviation
## of the respective values. 

seletest <- test[,grepl("mean()|std()", names(test))] 
seltest <- seletest[,!grepl("meanFreq", names(seletest))] 
seletrain <- train[,grepl("mean()|std()", names(train))]
seltrain <- seletrain[,!grepl("meanFreq", names(seletrain))]

## Introduce the row names.
# Read in the files activity_labels.txt and y_test or y_train for labeling the rows with the
# respective activity.
activitylink <- read.table("activity_labels.txt")
activitytrain <- read.table("train/y_train.txt")
activitytest <- read.table("test/y_test.txt")

# Read in the files subject_train.txt and subject_test.txt labeling the rows with the 
# subject number.
subjecttrain <- read.table("train/subject_train.txt")
subjecttest <- read.table("test/subject_test.txt")

# Introduce an index for activitytrain and activitytest.
activitytrain$index <- seq(1:nrow(activitytrain))
activitytest$index <- seq(1:nrow(activitytest))

# Rename the activities with "tidy names" (all lowercase letters).
activitylink$V2 <- tolower(activitylink$V2)               

# Merge the datasets activitylink and activitytest/activitytrain, respectively.
testlab <- merge(activitylink, activitytest, by.x="V1", by.y="V1")
trainlab <- merge(activitylink, activitytrain, by.x="V1", by.y="V1")

# Sort datasets by the index.
testlab <- arrange(testlab, index)
trainlab <- arrange(trainlab, index)

# Attach the subject list to the datasets testlab and trainlab.
testlab$V3 <- subjecttest$V1
trainlab$V3 <- subjecttrain$V1

# rename the columns
names(testlab) <- c("labelcode", "activity", "index", "subject")
names(trainlab) <- c("labelcode", "activity", "index", "subject")

# Introduce the subject and activity columns to the seltest and seltrain dataset.
subject <- testlab$subject
activity <- testlab$activity
seltest <- cbind(subject, activity, seltest)

subject <- trainlab$subject
activity <- trainlab$activity
seltrain <- cbind(subject, activity, seltrain)

# Combines the datasets seltest and seltrain
final <- rbind(seltest, seltrain)      

# Cleaning up the column names to "tidy names"(removal of special symbols, keep upper case
# letters only for clarity)

names(final) <- sub("\\()", "", names(final))
names(final) <- sub("-X", "X", names(final))
names(final) <- sub("-Y", "Y", names(final))
names(final) <- sub("-Z", "Z", names(final))
names(final) <- sub("-", "\\.", names(final))
names(final) <- sub(".mean", "Mean", names(final))
names(final) <- sub(".std", "Std", names(final))

# Group the resulting dataset final by subject and activity.
group <- group_by(final, subject, activity)

# Remove all datasets
rm("final", 
   "seltest", 
   "seltrain", 
   "activity", 
   "subject", 
   "testlab", 
   "trainlab", 
   "subjecttrain", 
   "subjecttest", 
   "seletest", 
   "seletrain")

# Summarise the dataset group to receive the mean of avery column in the dataset group for every 
# activity/subject pair.
tab1 <- summarise_each(group, funs(mean))



