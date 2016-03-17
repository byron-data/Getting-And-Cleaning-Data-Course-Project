###
# 1. Merges the training and the test sets to create one data set.
###

# Download the data
download.file(url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", 
              destfile = "dataset.zip")
			  
# Unpack the zip file
unzip(zipfile="dataset.zip")

# Set variable with path for top level data folder
folder <- "UCI HAR Dataset"
			  
# Read all test data, x (features), y (activities) and subjects
x_test <- read.table(file.path(folder, "test", "X_test.txt"), header=FALSE)
y_test <- read.table(file.path(folder, "test", "y_test.txt"), header=FALSE)
subject_test <- read.table(file.path(folder, "test", "subject_test.txt"), header=FALSE)
			  
# Read all train data, x (features), y (activities) and subjects
x_train <- read.table(file.path(folder, "train", "X_train.txt"), header=FALSE)
y_train <- read.table(file.path(folder, "train", "y_train.txt"), header=FALSE)
subject_train <- read.table(file.path(folder, "train", "subject_train.txt"), header=FALSE)

# Combine test, train and subject data by rows
x_combined <- rbind(x_test, x_train)
y_combined <- rbind(y_test, y_train)
subject_combined <- rbind(subject_test, subject_train)

###
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
###

# Read the features
features <- read.table(file.path(folder, "features.txt"), header=FALSE)

# Setup a search string for finding measurements on the mean and standard deviation
mean_std_only <- grep("-(mean|std)\\(\\)", features[,2])

# This will take only those columns on the mean and standard deviation for each measurement
x_combined <- x_combined[,mean_std_only]

###
# 3. Uses descriptive activity names to name the activities in the data set
###

# Read the activity labels
activity_labels <- read.table(file.path(folder, "activity_labels.txt"), header=FALSE)

# Substitute the activity number in y with the descriptive activity name
y_combined[,1] <- activity_labels[y_combined[,1],2]

###
# 4. Appropriately labels the data set with descriptive variable names
###

# Only take the feature names with mean and standard deviation for each measurement
features_names <- features[mean_std_only, 2]

# Tidy up mean and standard deviation variable names including removing parenthesis
features_names <- gsub("-mean\\(\\)", "Mean", features_names)
features_names <- gsub("-std\\(\\)", "StdDev", features_names)

# Expand on abbreviations with something more descriptive
features_names <- gsub("^t", "time", features_names)
features_names <- gsub("^f", "frequency", features_names)
features_names <- gsub("Acc", "accelerometer", features_names)
features_names <- gsub("Gyro", "gyroscope", features_names)
features_names <- gsub("Mag", "magnitude", features_names)
features_names <- gsub("BodyBody", "body", features_names)

# Make all variable names lowercase
features_names <- tolower(features_names)

# Now use the descriptive variable names for the x (features) data
colnames(x_combined) <- features_names

# Explicity use "activity" and "subject" as the column names
colnames(y_combined) <- "activity"
colnames(subject_combined) <- "subject"

# Combine data from x (features), y (activities) and subjects
dataset <- cbind(x_combined, y_combined, subject_combined)

###
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each
# activity and each subject.
###

# Get the mean (average) on every variable of the dataset except activity and subject
tidy_data <- aggregate(dataset[,names(dataset) != c("activity","subject")], by=list(activity=dataset$activity,subject=dataset$subject), mean);

# Write out the tidy data set to disk
write.table(tidy_data, "tidy_data.txt", row.name=FALSE)
