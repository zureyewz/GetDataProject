## 1. Merges the training and the test sets to create one data set.
subject_train <- read.table("./train/subject_train.txt")      #import training data set
X_train <- read.table("./train/X_train.txt")      #import training data set
y_train <- read.table("./train/y_train.txt")      #import training data set
subject_test <- read.table("./test/subject_test.txt")         #import testing data set
X_test <- read.table("./test/X_test.txt")         #import testing data set
y_test <- read.table("./test/y_test.txt")         #import testing data set
train <- cbind(X_train,y_train,subject_train)   #merge training data set
test <- cbind(X_test,y_test,subject_test)       #merge testing data set
fulldata <- rbind(train,test)                   #merge data set

## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
features <- read.table("features.txt",stringsAsFactors=FALSE)           #import features
features <-rbind(features,c(562,"y"),c(563,"subject"))                  #add in feature name for y & subject
colnames(fulldata) <- features$V2
#find columns with mean() and std()        
temp <- grepl("mean()",features$V2,fixed=TRUE) | grepl("std()",features$V2,fixed=TRUE)  
temp[562]<-TRUE #setting y column to be included
temp[563]<-TRUE #setting subject column to be included
data <- fulldata[,temp] #subsetting only mean and std columns with y and subject column

## 3. Uses descriptive activity names to name the activities in the data set
activity_names <- read.table("activity_labels.txt")
#substitute each y activity with activity name
data$y<-sub(1,activity_names$V2[1],data$y)
data$y<-sub(2,activity_names$V2[2],data$y)
data$y<-sub(3,activity_names$V2[3],data$y)
data$y<-sub(4,activity_names$V2[4],data$y)
data$y<-sub(5,activity_names$V2[5],data$y)
data$y<-sub(6,activity_names$V2[6],data$y)

## 4. Appropriately labels the data set with descriptive variable names. 
library(stringr)
colnames(data) <- str_replace(colnames(data),"\\(.*\\)","")
colnames(data) <- sub("t","",colnames(data))
colnames(data) <- sub("f","FFT ",colnames(data))
colnames(data) <- sub("Acc"," Accelerometer",colnames(data))
colnames(data) <- sub("Gyro"," Gryoscope",colnames(data))
colnames(data) <- sub("Mag"," Magnitude",colnames(data))
colnames(data) <- sub("Jerk"," Jerk",colnames(data))
colnames(data) <- sub("-mean-"," Average",colnames(data))
colnames(data) <- sub("-mean"," Average",colnames(data))
colnames(data) <- sub("-std-"," Standard Deviation",colnames(data))
colnames(data) <- sub("-sd"," Standard Deviation",colnames(data))
colnames(data) <- sub("-std"," Standard Deviation",colnames(data))
colnames(data) <- sub("X"," X-axial",colnames(data))
colnames(data) <- sub("Y"," Y-axial",colnames(data))
colnames(data) <- sub("Z"," Z-axial",colnames(data))
colnames(data)[67] <- "Activity"
colnames(data)[68] <- "Subject"

## 5. From the data set in step 4, creates a second, independent tidy data set with the 
# average of each variable for each activity and each subject.
subsetdata <- data %>% group_by(Subject,Activity) %>% summarise_each(funs(mean))
write.table(subsetdata,file="subsetdata.txt",row.name=FALSE)