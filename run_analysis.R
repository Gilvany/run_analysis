#Grading Criteria Overview -----------------------------------------------
  #The submitted data set is tidy. 
  #The Github repo contains the required scripts.
  #GitHub contains a code book that modifies and updates the available codebooks 
  #with the data to indicate all the variables and summaries calculated, along 
  #with units, and any other relevant information.
  #The README that explains the analysis files is clear and understandable.
  #The work submitted for this project is the work of the student who submitted it.

#Getting and Cleaning Data Course Project --------------------------------
  #The purpose of this project is to demonstrate your ability to collect, work 
  #with, and clean a data set. The goal is to prepare tidy data that can be used 
  #for later analysis. You will be graded by your peers on a series of yes/no 
  #questions related to the project. You will be required to submit: 
    #1) a tidy data set as described below, 
    #2) a link to a Github repository with your script for performing the 
    #analysis, and 
    #3) a code book that describes the variables, the data, and any 
  #transformations or work that you performed to clean up the data called 
  #CodeBook.md. 
  
  #You should also include a README.md in the repo with your scripts. This repo 
  #explains how all of the scripts work and how they are connected.
  
  #One of the most exciting areas in all of data science right now is wearable 
  #computing - see for example this article. Companies like Fitbit, Nike, and 
  #Jawbone Up are racing to develop the most advanced algorithms to attract new 
  #users. The data linked to from the course website represent data collected 
  #from the accelerometers from the Samsung Galaxy S smartphone. A full 
  #description is available at the site where the data was obtained:
      #http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
  
  #Here are the data for the project:
      #https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
  
  #You should create one R script called run_analysis.R that does the following. 

# Script for Answers ------------------------------------------------------

if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
} else {
  library(dplyr)
}

#1. Merges the training and the test sets to create one data set.

#Main Path
Input.Path <- paste0("C:/Users/Gilva/OneDrive/ESTUDIOS (PAPERS)/Cursos y ",
                     "Certificados/Data Science/Clase 3 - Getting and Cleaning",
                     " Data/Week 4/UCI HAR Dataset/")

#Part 1.1 - Train Merge
#Gather
Train <- "train/"
Train.Input.Path.1 <- paste0(Input.Path, Train, "subject_train.txt")
Train.Input.Path.2 <- paste0(Input.Path, Train, "X_train.txt")
Train.Input.Path.3 <- paste0(Input.Path, Train, "Y_train.txt")
Features.Path <- paste0(Input.Path, "features.txt")
Features <- read.table(Features.Path)
#Prepare
Train.Input.1 <- read.table(Train.Input.Path.1)
colnames(Train.Input.1)[colnames(Train.Input.1) %in% 
                          "V1"] <- "subject" #Renamed
Train.Input.2 <- read.table(Train.Input.Path.2)
colnames(Train.Input.2) <- Features[,2] #Renamed
Train.Input.3 <- read.table(Train.Input.Path.3)
colnames(Train.Input.3)[colnames(Train.Input.3) %in%
                          "V1"] <- "activity" #Renamed
#Merge
All.Train.Inputs <- cbind(Train.Input.1, Train.Input.3, Train.Input.2)
All.Train.Inputs$Type <- "Train"
#Quality Test
ncol(Train.Input.1) + ncol(Train.Input.2) + ncol(Train.Input.3) + 1 == ncol( #+1 for Type col
  All.Train.Inputs) 

#Part 1.2 - Test Merge
#Gather
Test <- "test/"
Test.Input.Path.1 <- paste0(Input.Path, Test, "subject_test.txt")
Test.Input.Path.2 <- paste0(Input.Path, Test, "X_test.txt")
Test.Input.Path.3 <- paste0(Input.Path, Test, "Y_test.txt")
#Prepare
Test.Input.1 <- read.table(Test.Input.Path.1)
colnames(Test.Input.1)[colnames(Test.Input.1) %in%
                         "V1"] <- "subject" #Renamed
Test.Input.2 <- read.table(Test.Input.Path.2)
colnames(Test.Input.2) <- Features[,2] #Renamed
Test.Input.3 <- read.table(Test.Input.Path.3)
colnames(Test.Input.3)[colnames(Test.Input.3) %in%
                         "V1"] <- "activity" #Renamed
#Merge
All.Test.Inputs <- cbind(Test.Input.1, Test.Input.3, Test.Input.2)
All.Test.Inputs$Type <- "Test"
#Quality Test
ncol(Test.Input.1) + ncol(Test.Input.2) + ncol(Test.Input.3) + 1 == ncol( #+1 for Type col
  All.Test.Inputs)

#Part 1.3 - Full Merge
All.Inputs <- rbind(All.Train.Inputs, All.Test.Inputs)

#2. Extracts only the measurements on the mean and standard deviation for 
#each measurement. 

x <- colnames(All.Inputs)
original.col.length <- length(x)
paste("original.col.length.is:", original.col.length)
Variables <- "activity|subject|mean|std"
y <- grepl(Variables, x)
cols.that.will.be.used <- table(y)
paste("cols.that.will.be.used:", cols.that.will.be.used)
z <- All.Inputs[, y == TRUE]
final.col.length <- length(z)
paste("final.col.length:", final.col.length)

#3. Uses descriptive activity names to name the activities in the data set

#Path
Activity.Input.Path.1 <- paste0(Input.Path, "activity_labels.txt")
#New Variable
Activity.File <- read.table(Activity.Input.Path.1)
colnames(Activity.File)
colnames(Activity.File)[colnames(Activity.File) %in%
                          "V2"] <- "activity" #Renamed
colnames(Activity.File)
Activities <- merge(z, Activity.File, by = 'activity', all.x= TRUE) 
colnames(Activities)

#4. Appropriately labels the data set with descriptive variable names. 

colnames(Activities)
names(Activities)<-gsub("^t", "Time.", names(Activities))
names(Activities)<-gsub("^f", "Frequency.", names(Activities))
names(Activities)<-gsub("Acc", ".accelerometer.", names(Activities))
names(Activities)<-gsub("Gyro", ".gyroscope.", names(Activities))
names(Activities)<-gsub("Mag", ".magnitude.", names(Activities))
names(Activities)<-gsub("Body", ".body.", names(Activities))
names(Activities)<-gsub("body.body.", ".body.", names(Activities))
names(Activities)<-gsub("Jerk", ".jerk.", names(Activities))
Activities <- select(Activities, -V1)
colnames(Activities)

#5. From the data set in step 4, creates a second, independent tidy data set 
#with the average of each variable for each activity and each subject.

original.col.length <- nrow(Activities)  
paste("original.row.length.is:", original.col.length)
Activity.2 <- Activities %>%
  group_by(subject, activity) %>%
  summarize_all(mean, na.rm = TRUE)
final.col.length <- nrow(Activity.2)
paste("final.row.length.is:", final.col.length)
Activity.2 <- Activity.2[order(Activity.2$subject, Activity.2$activity),]
write.table(Activity.2, paste0(
  Input.Path,"mean.txt"), row.name=FALSE)



