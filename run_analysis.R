# download file 
fileUrl <-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile= "cfdata.zip", method= "auto")

# unzip downloaded file into a folder named cfdata

unzip("cfdata.zip", exdir= "cfdata")

## set working directory to unziped folder

setwd ("~/R/Myproject/cfdata/UCI HAR Dataset")  

## reading training tables

x_train <- read.table ("train/X_train.txt", sep = "")
y_train <- read.table ("train/Y_train.txt", sep= "")
sub_train <- read.table ("train/subject_train.txt", sep= " ")

# reading test tables

x_test <- read.table ("test/X_test.txt", sep = "")
y_test <- read.table ("test/Y_test.txt", sep = "")
sub_test <-read.table ("test/subject_test.txt", sep = "")

# reading feature vector
feature <- read.table ("features.txt", sep = "")

# reading activity labels

act_label <- read.table ("activity_labels.txt", sep= "")

# assign the name to x_test and x_train.

colnames (x_test) <- feature [,2]
colnames (x_train) <- feature [,2]
colnames (y_test) <- "Activity_ID"
colnames (y_train) <- "Activity_ID"
colnames (sub_test) <- "Subject_ID"
colnames (sub_train) <- "Subject_ID"
colnames (act_label) <- c("Activity_ID", "Activity_Type")

# combine the test and train group data seperately 

y_group <- rbind(y_test, y_train)
sub_group <- rbind(sub_test, sub_train)
x_group<- rbind(x_test,x_train)

# combine the x,y and subject data together.

All_data <- cbind(y_group, sub_group,x_group)

# question 2
# first, construct a vector contains the colnames

col_name <- colnames(All_data)

#find the columns with name  mean and std
mean_std <- grepl(".*Mean.*|.*std.*", col_name,ignore.case=TRUE)
# extract the columns with mean and std
mean_std_data <- All_data [,mean_std]

# question 3
# use the activity_types stored in act_label to replace the numbers in All_data$Activity_ID

act_label$Activity_ID <- as.integer(act_label$Activity_ID)
act_label$Activity_Type <- as.character(act_label$Activity_Type)
# this function assign the activity_type in act_label to the integers in
# activity_id variable in y_test and y_train 
exchange <- function(i){
  data.frame(act_label[i,2])
}
Activity_ID<- exchange (y_group$Activity_ID)

# name the new variable as "Activity"

names(Activity_ID)="Activity"

# combine the activity variable with mean and std data.

Actdata<- cbind(Activity_ID,sub_group, mean_std_data)


# question 4
# by examing the column names. we can replace
# Acc with Accelerometer
# Gyro with Gyroscope
# BodyBody with Body
# Mag with Magnitude
# f with Frequency
# t with Time

names(Actdata)<- gsub("Acc","Accelerometer", names(Actdata))
names(Actdata)<- gsub("Gyro","Gyroscope", names(Actdata))
names(Actdata)<- gsub("BodyBody","Body", names(Actdata))
names(Actdata)<- gsub("Mag","Magnitude", names(Actdata))
names(Actdata)<- gsub("^t","Time", names(Actdata))
names(Actdata)<- gsub("^f","Frequency", names(Actdata))
names(Actdata)<- gsub("tBody","Timebody", names(Actdata))
names(Actdata)<- gsub("-mean()","Mean", names(Actdata))
names(Actdata)<- gsub("-std","STD", names(Actdata))
names(Actdata)<- gsub("-freq","Frequency", names(Actdata))


# question 5

## set the subject_id as factor variable
 Actdata$Subject_ID<- as.factor(Actdata$Subject_ID)
 Actdata<- data.frame(Actdata)
 ## create the tidydata with average for each activity and subject. 
 tidydata <- aggregate(.~Subject_ID+Activity,Actdata,mean)
 tidydata <- tidydata[order(tidydata$Subject_ID,tidydata$Activity), ]
 write.table(tidydata,file="tidy.txt",row.names=FALSE)





