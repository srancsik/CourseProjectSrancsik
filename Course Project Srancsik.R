#Make work directory and download data
  if(!file.exists("C:/CourseraWork")){dir.create("C:/CourseraWork")}
  Path <- "C:/CourseraWork"
  setwd(Path)
  if(!file.exists("./data")){dir.create("./data")}
  Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(Url,destfile="./data/Data.zip")
  unzip(zipfile="./data/Data.zip",exdir="./data")

#Load packages
  library(dplyr)
  library(tidyr) 
  library(data.table)
  
#Read tables
  SourceFilePath <- "C:/CourseraWork/data/UCI HAR Dataset"
  SubjectTrain <- tbl_df(read.table(file.path(SourceFilePath, "train", "subject_train.txt")))
  SubjectTest  <- tbl_df(read.table(file.path(SourceFilePath, "test" , "subject_test.txt" )))
  ActivityTrain <- tbl_df(read.table(file.path(SourceFilePath, "train", "Y_train.txt")))
  ActivityTest  <- tbl_df(read.table(file.path(SourceFilePath, "test" , "Y_test.txt" )))
  Train <- tbl_df(read.table(file.path(SourceFilePath, "train", "X_train.txt" )))
  Test  <- tbl_df(read.table(file.path(SourceFilePath, "test" , "X_test.txt" )))
  
#Task 1. Merges the training and the test sets to create one data set.
  #Merge data sets
  Subject <- rbind(SubjectTrain, SubjectTest)
  setnames(Subject, "V1", "subject")
  Activity<- rbind(ActivityTrain, ActivityTest)
  setnames(Activity, "V1", "activityNum")
  Table <- rbind(Train, Test)
  
  # Change namings
  Features <- tbl_df(read.table(file.path(SourceFilePath, "features.txt")))
  setnames(Features, names(Features), c("featureNum", "featureName"))
  colnames(Table) <- Features$featureName
  Labels<- tbl_df(read.table(file.path(SourceFilePath, "activity_labels.txt")))
  setnames(Labels, names(Labels), c("activityNum","activityName"))
  SubjectAndActivity<- cbind(Subject, Activity)
  Table <- cbind(SubjectAndActivity, Table)
  
  
#Task 2. Extracts only the measurements on the mean and standard deviation for each measurement.
  
  MeanStd <- grep("mean\\(\\)|std\\(\\)",Features$featureName,value=TRUE) 
  MeanStd <- union(c("subject","activityNum"), MeanStd)
  Table<- subset(Table,select=MeanStd) 
  
#Task 3. Uses descriptive activity names to name the activities in the data set
  Table <- merge(Labels, Table , by="activityNum", all.x=TRUE)
  Table$activityName <- as.character(Table$activityName)
  Aggregated<- aggregate(. ~ subject - activityName, data = Table, mean) 
  Table<- tbl_df(arrange(Aggregated,subject,activityName))

#Task 4. Appropriately labels the data set with descriptive variable names.  
  #For easier readability ">" is used as a divider
  names(Table)<-gsub("^t", "Time>", names(Table))
  names(Table)<-gsub("^f", "Frequency>", names(Table))
  names(Table)<-gsub("BodyBody", "Body>", names(Table))
  names(Table)<-gsub("Gravity", "Acceleration_Of_Gravity>", names(Table))
  names(Table)<-gsub("Acc", "Accelerometer>", names(Table))
  names(Table)<-gsub("Gyro", "Gyroscope>", names(Table))
  names(Table)<-gsub("Jerk", "Sudden_Movement_Acceleration>", names(Table))
  names(Table)<-gsub("Mag", "Magnitude>", names(Table))
  names(Table)<-gsub("std()", "SD>", names(Table))
  names(Table)<-gsub("mean()", "Mean>", names(Table))
  
#Task 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  write.table(Table, "Tidy_Data.txt", row.name=FALSE)
  
  
  
