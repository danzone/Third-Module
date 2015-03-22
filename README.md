---
title: "Coursera Data Scientist Project 3"
author: "Philip Jones"
date: "Sunday, March 22, 2015"
output: html_document
---
---
title: "Coursera Project 3"
author: "Philip Jones"
date: "Sunday, March 22, 2015"
output: word_document
---
# Coursera Data Scientist Tool Box 
## Getting and Cleaning Data 
### Course Project
### Philip Jones
###  3/10/2015


###  The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit:
###       1) a tidy data set as described below, 
###       2) a link to a Github repository with your script for performing the analysis, and 
###       3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.
## 
## 
###   One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 
#   
###    http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
# 
###  Here are the data for the project: 
#   
###   https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 
###  You should create one R script called run_analysis.R that does the following. 
###  1) Merges the training and the test sets to create one data set.
###  2) Extracts only the measurements on the mean and standard deviation for each measurement. 
###  3) Uses descriptive activity names to name the activities in the data set
###  4) Appropriately labels the data set with descriptive variable names. 
###  5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#  
#  
#  

#########################################
# Pseudo Code
### Go to Course Website Download file to working Directory
### Unzip File and Set Working Directory
### Read in Features, Activity and Subjects for both Training and Testing Datasets
### Merge the Training and Testing Datasets name Columns
### Use only the Mean and Standard Deviation Columns
### From the Reference File
#####     Acc is short for Accelerometer
#####     Gyro is short for Gyroscope
#####     BodyBody is a Global Find/Replace so Replacing back to Body
#####     ^t is short for Time
#####     ^f is short for Frequency
#####     tBody is short for Body Jerk
#####     -mean() is short for Mean
#####     -std() is short for Std_Dev
#####     -freq() is short for Frequency
#####     angle is short for Angle
#####     gravity is short for Gravity
###  Convert to Tidy Names
###  Aggregate the Data by Subject and Activity
###  Write the Tidy Data Table


#### Load Libraries

library("downloader")
library("plyr")
library(data.table)
library("dplyr")



#### Set Working Directory, Clean up Project Directory,Create New Directory for Project 3``

unlink("./project3",recursive=TRUE,force=TRUE)
dir.create("./project3")
setwd("./project3")

####  Set the Course URL to Load Data and Set WD to the UCI HAR Data Set

url="https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

download(url, dest="dataset.zip", mode="wb") 

unzip ("dataset.zip", exdir = "./UCI HAR Dataset")

setwd("./UCI HAR Dataset")



#### Read in the Features and  Activity Labels

features_names <- read.table("UCI HAR Dataset/features.txt")

activity_labels<- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)


####  Read in the Training Data

subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activity_train <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
features_train <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)


####  Read in the Test Data

subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activity_test <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
features_test <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)


####  Merge Tables Using R Bind

subject <- rbind(subject_train, subject_test)

activity <- rbind(activity_train, activity_test)

features <- rbind(features_train, features_test)



####  Name the Column Headers

colnames(features) <- t(features_names[2])


####  Rename the Column Headers for Activity and Subject

colnames(activity) <- "Activity"

colnames(subject) <- "Subject"

Complete_Data <- cbind(activity,subject,features)


####  Print Structure of the Complete Data

str(Complete_Data)


#### Identify only columns with Mean or Stadard Deviation already in the Title

columnswithMean_STD <- grep(".*Mean.*|.*Std.*", names(Complete_Data), ignore.case=TRUE)


#### Add back in the Acitivty and Subject

required_Columns <- c(columnswithMean_STD, 562, 563)


####  Extract Data for the Required Columns

extracted_Data <- Complete_Data[,required_Columns]


#### Count Number of Activities

Num_Actvities<-count(activity_labels)


####  Extract out the Activities as Characters

extracted_Data$Activity <- as.character(extracted_Data$Activity)


#### Loop Through the Activities

for (i in 1:Num_Activities){
  extracted_Data$Activity[extracted_Data$Activity == i] <- as.character(activity_labels[i,2])
}


#### Convert Activities as Factors

extracted_Data$Activity <- as.factor(extracted_Data$Activity)


#### Identify the Header Names

names(extracted_Data)


#### From the Reference File
#####     Acc is short for Accelerometer
#####     Gyro is short for Gyroscope
#####     BodyBody is a Global Find/Replace so Replacing back to Body
#####     ^t is short for Time
#####     ^f is short for Frequency
#####     tBody is short for Body Jerk
#####     -mean() is short for Mean
#####     -std() is short for Std_Dev
#####     -freq() is short for Frequency
#####     angle is short for Angle
#####     gravity is short for Gravity
#
#### Convert to Tidy Names

names(extracted_Data)<-gsub("Acc", "Accelerometer", names(extracted_Data))

names(extracted_Data)<-gsub("Gyro", "Gyroscope", names(extracted_Data))

names(extracted_Data)<-gsub("BodyBody", "Body", names(extracted_Data))

names(extracted_Data)<-gsub("Mag", "Magnitude", names(extracted_Data))

names(extracted_Data)<-gsub("^t", "Time", names(extracted_Data))

names(extracted_Data)<-gsub("^f", "Frequency", names(extracted_Data))

names(extracted_Data)<-gsub("tBody", "Jerk", names(extracted_Data))

names(extracted_Data)<-gsub("-mean()", "Mean", names(extracted_Data), ignore.case = TRUE)

names(extracted_Data)<-gsub("-std()", "Std_Dev", names(extracted_Data), ignore.case = TRUE)

names(extracted_Data)<-gsub("-freq()", "Frequency", names(extracted_Data), ignore.case = TRUE)

names(extracted_Data)<-gsub("angle", "Angle", names(extracted_Data))

names(extracted_Data)<-gsub("gravity", "Gravity", names(extracted_Data))

#### Extract Subject as Factor

extracted_Data$Subject <- as.factor(extracted_Data$Subject)

#### Create Extracted Data as Data Table

extracted_Data <- data.table(extracted_Data)

#### Create Tidy Data Set using the Factors of Subject and Activities for all Measurements

tidyData <- aggregate(. ~Subject + Activity, extracted_Data, mean)

#### Order Data by Subject and Activity

tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]


#### Write Table

write.table(tidyData, file = "Tidy.txt", row.names = FALSE)
