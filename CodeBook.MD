# CodeBook

This code book describes the variables, the data, and any transformations or work that was performed to clean up the data.

## The data

The data represent data collected from the accelerometers from the Samsung Galaxy S smartphone.

* A full description is available at the site where the data was obtained: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
* The data for the project: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

## Data Set Information

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.

Dataset file information is included at the bottom of this guide.

## Transformations or work performed to clean up the data

* The data set (zip file) is downloaded and unpacked.
* The x data is read separately from the test set and train set and then combined to make one set of x data, the same is done for y data and subject data.
* Which measurements are on the mean and standard deviation for each measurement are determined by reading the descriptive names from "features.txt" and then choosing only the corresponding columns from the x data.
* The activity labels are read from "activity_labels.txt" and this descriptive activity name is used to replace the activity number in the y data.
* Reduce the features names as read from "features.txt" to be only the ones with mean and standard deviation for each measurement.
* The reduced set of features names are then "tidied" up by making the names more readable as per tidy data rules.
* Descriptive variable names are applied to the data set: features names are used to replace the x data variable names; y data variable is named "activity" and subject data variable is named "subject".
* Combine the data from x (features), y (activities) and subjects into one data set and calculate the mean (average) on every variable of the data set except activity and subject.
* Write the data set to disk.

## Dataset file information:

The dataset includes the following files:

- 'README.txt'

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent. 

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

- 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 

- 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 

- 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second.
