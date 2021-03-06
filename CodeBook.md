
# Code Book
=================

This code book contains some original metadata from smartlab project and the information about the transformation of the original dataset and the resulting data.

# Original Description
The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

#New variable names and new variables

timebodyaccelerometer-XYZ 
timebravityaccelerometer-XYZ 
timebodyaccelerometerjerk-XYZ 
timebodygyroscope-XYZ 
timebodygyroscopejerk-XYZ 
timebodyaccelerometermagnitude 
timegravityaccelerometermagnitude 
timebodyaccelerometerjerkmagnitude
timebodygyroscopemagnitude
timebodygyroscopejerkmagnitude
timebodyaccelerometer-XYZ
frequencybodyaccelerometerjerk-XYZ
frequencybodygyroscope-XYZ
frequencybodyaccelerometermagnitude
frequencybodyaccelerometerjerkmagnitude
frequencybodygyroscopemagnitude
frequencybodygyroscopejerkmagnitude

The set of variables that were estimated from these signals are: 

mean: Mean value
std: Standard deviation

#Activity
Two new variables of the final dataset are:

activityid: The index of this possible values ["WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"] 
activitydescription: Any of this possible values ["WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"]

The complete list of variables of each feature vector is available in 'features.txt'

#Subject
This variable identifies the volunteer related with the data row. Possible values are values from 1 to 30.

#activityRecognitionDataset.txt

This dataset is the tidy dataset result of this project. This dataset is a data set with the average of each variable for each activity and each subject.
