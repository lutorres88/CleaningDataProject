
# download the data

library(data.table)
fileURL = 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'

if (!file.exists('./UCI HAR Dataset.zip')){
  download.file(fileURL,'./UCI HAR Dataset.zip', mode = 'wb')
  unzip("UCI HAR Dataset.zip", exdir = getwd())
}

# read and convert the data

features <- read.csv('./UCI HAR Dataset/features.txt', header = FALSE, sep = ' ')
features <- as.character(features[,2])

x_train <- read.table('./UCI HAR Dataset/train/X_train.txt')
y_train <- read.csv('./UCI HAR Dataset/train/y_train.txt', header = FALSE, sep = ' ')
subject_train <- read.csv('./UCI HAR Dataset/train/subject_train.txt',header = FALSE, sep = ' ')

train_data <-  data.frame(subject_train, y_train, x_train)
names(train_data) <- c(c('subject', 'activity'), features)

x_test <- read.table('./UCI HAR Dataset/test/X_test.txt')
y_test <- read.csv('./UCI HAR Dataset/test/y_test.txt', header = FALSE, sep = ' ')
subject_test <- read.csv('./UCI HAR Dataset/test/subject_test.txt', header = FALSE, sep = ' ')

test_data <-  data.frame(subject_test, y_test, x_test)
names(test_data) <- c(c('subject', 'activity'), features)


# merge the training and test data into one data set
data <- rbind(train_data, test_data)

# extract the mean and standard deviation 

features_avg_std <- grep('mean|std', features)
data_avg_std <- data[,c(1,2,features_avg_std + 2)]

# read activity labels to provide descriptive names for each activity

activity_labels <- read.table('./UCI HAR Dataset/activity_labels.txt', header = FALSE)
activity_labels <- as.character(activity_labels[,2])
data_avg_std$activity <- activity_labels[data_avg_std$activity]

# assign descriptive names to all variables

new_names <- names(data_avg_std)
new_names <- gsub("[(][)]", "", new_names)
new_names <- gsub("^t", "TimeDomain_", new_names)
new_names <- gsub("^f", "FrequencyDomain_", new_names)
new_names <- gsub("Acc", "Accelerometer", new_names)
new_names <- gsub("Gyro", "Gyroscope", new_names)
new_names <- gsub("Mag", "Magnitude", new_names)
new_names <- gsub("-mean-", "_Mean_", new_names)
new_names <- gsub("-std-", "_StandardDeviation_", new_names)
new_names <- gsub("-", "_", new_names)
names(data_avg_std) <- new_names 

# create tidy data set with the average of each variable for each activity and each subject.

tidy_data <- aggregate(data_avg_std[,3:81], by = list(activity = data_avg_std$activity, subject = data_avg_std$subject),FUN = mean)

write.table(x = tidy_data, file = "tidy_data.txt", row.names = FALSE)

