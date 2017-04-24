packages <- c('data.table','reshape2')

sapply(packages, require, character.only=TRUE, quietly=TRUE)

path <- getwd()
path


fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

filename <- "Dataset.zip"

if (!file.exists(path)) {dir.create(path)}

download.file(fileURL, file.path(path, filename))

executable <- file.path("C:", "Program Files (x86)", "7-Zip", "7z.exe")
parameters <- "x"
cmd <- paste(paste0("\"", executable, "\""), parameters, paste0("\"", file.path(path, filename), "\""))
system(cmd)

pathIn <- file.path(path, "UCI HAR Dataset")

train <- fread(file.path(pathIn, "train", "subject_train.txt"))
test  <- fread(file.path(pathIn, "test" , "subject_test.txt" ))
trainActivity <- fread(file.path(pathIn, "train", "Y_train.txt"))
testActivity  <- fread(file.path(pathIn, "test" , "Y_test.txt" ))

fileToDataTable <- function (f) {
	df <- read.table(f)
	dt <- data.table(df)
}
trainTable <- fileToDataTable(file.path(pathIn, "train", "X_train.txt"))
testTable  <- fileToDataTable(file.path(pathIn, "test" , "X_test.txt" ))

subjectTable <- rbind(train, test)
setnames(subjectTable, "V1", "subject")
activityTable <- rbind(trainActivity, testActivity)
setnames(activityTable, "V1", "activityNum")
data <- rbind(trainTable, testTable)

subjectTable <- cbind(subjectTable, activityTable)
data <- cbind(subjectTable, data)

setkey(data, subject, activityNum)

features <- fread(file.path(pathIn, "features.txt"))
setnames(features, names(features), c("featureNum", "featureName"))

features <- features[grepl("mean\\(\\)|std\\(\\)", featureName)]

features$featureCode <- features[, paste0("V", featureNum)]

select <- c(key(data), features$featureCode)
data <- data[, select, with=FALSE]

activityNames <- fread(file.path(pathIn, "activity_labels.txt"))
setnames(activityNames, names(activityNames), c("activityNum", "activityName"))

data <- merge(data, activityNames, by="activityNum", all.x=TRUE)

setkey(data, subject, activityNum, activityName)

data <- data.table(melt(data, key(data), variable.name="featureCode"))

data <- merge(data, features[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)

data$activity <- factor(data$activityName)
data$feature <- factor(data$featureName)


grepthis <- function (regex) {
  grepl(regex, data$feature)
}
## Features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol=nrow(y))
data$featDomain <- factor(x %*% y, labels=c("Time", "Freq"))
x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol=nrow(y))
data$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol=nrow(y))
data$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol=nrow(y))
data$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))
## Features with 1 category
data$featJerk <- factor(grepthis("Jerk"), labels=c(NA, "Jerk"))
data$featMagnitude <- factor(grepthis("Mag"), labels=c(NA, "Magnitude"))
## Features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol=nrow(y))
data$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))

setkey(data, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
dataTidy <- data[, list(count = .N, average = mean(value)), by=key(data)]


