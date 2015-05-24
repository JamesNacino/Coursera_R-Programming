#setwd("C:\\Users\\james\\SkyDrive\\Data Science\\DataScience_Coursera\\R Programming\\Getting and Cleaning Data-Project")

##Training Data
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", header=FALSE) 
x_train <- read.table("./UCI HAR Dataset/train/x_train.txt", header=FALSE) 
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", header=FALSE) 
#Combine Training Data into one dataset
training_dataset <- cbind(subject_train, x_train, y_train) 

##Test Data
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", header=FALSE) 
x_test <- read.table("./UCI HAR Dataset/test/x_test.txt", header=FALSE) 
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", header=FALSE) 
#Combine Test Data into one dataset
test_dataset <- cbind(subject_test, x_test, y_test) 

##Combined training and test datasets
combined_data <- rbind(training_dataset, test_dataset)

##Read the labels
features <- read.table("./UCI HAR Dataset/features.txt", header=FALSE, colClasses="character")
labels <- features[,2]

#Get only the mean and standard deviation values from 'labels'
count_labels <- length(labels)
location_valid_labels <- grepl("mean|std", labels)
labels2 <- vector()
for (i in 1:count_labels){
  if (location_valid_labels[i] == TRUE){
    labels2 <- append(labels2, labels[[i]])
  }
}

##'combined_data' needs to only keep the activities which values are equal to numbers 1-6 in the first column
##The rest of the rows which have the value (7-...) need to be removed from the data frame
tempdata1 <- combined_data[combined_data$V1==1,]
tempdata2 <- combined_data[combined_data$V1==2,]
tempdata3 <- combined_data[combined_data$V1==3,]
tempdata4 <- combined_data[combined_data$V1==4,]
tempdata5 <- combined_data[combined_data$V1==5,]
tempdata6 <- combined_data[combined_data$V1==6,]
combined_data <- rbind(tempdata1, tempdata2, tempdata3, tempdata4, tempdata5, tempdata6)

##'combined_data' needs to be subsetted to only include columns that include mean and standard deviation
location_valid_labels2 <- grep("mean|std", labels)
combined_data <- combined_data[,location_valid_labels2]

##Change the column names in the 'combined_data' dataset
labels2 <- labels2[1:78]
colnames(combined_data) <- "Activities"
colnames(combined_data)[2:79] <- labels2

##Change the values in the 'Activities' column
#Numbers correspond to activity (1=walking,2=walking_upstairs,3=walking_downstairs,4=sitting,5=standing,6=laying)
combined_data$Activities[combined_data$Activities == 1] <- "walking"
combined_data$Activities[combined_data$Activities == 2] <- "walking_upstairs"
combined_data$Activities[combined_data$Activities == 3] <- "walking_downstairs"
combined_data$Activities[combined_data$Activities == 4] <- "sitting"
combined_data$Activities[combined_data$Activities == 5] <- "standing"
combined_data$Activities[combined_data$Activities == 6] <- "laying"

##Get the mean for each variable given all six activities
#Use split on data frame, 'combined_data', and separate the column, 'Activities'
final_mean <- split(combined_data, combined_data$Activities)
final_dataset <- lapply(final_mean, function(combined_data) colMeans(combined_data[,2:79]))
final_data <- data.frame(final_dataset)



