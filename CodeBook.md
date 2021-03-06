averages.txt is a data table format file suitable for reading into R by running:

read.table("averages.txt", header = TRUE)

This dataset intends to meet the specification for the class project
of the ninth instance of the Johns Hopkins University _Getting and
Cleaning Data_ course.

The observations are the computed averages of the mean and standard
deviation estimates Smartlab derived from time domain accelerometer
and gyroscope observations released as the _Human Activity Recognition
Using Smartphones Dataset_.  For averages.txt, the Training and
Test sets from Smartlab are considered as one. Each of sixty-six
such measurement variables are presented for each combination of thirty Subjects
and six Activities.

Variable descriptions:

Name	|	Description
------- | -----------------
"Subject"|	integer; range of 1 to 30
"Activity"|	factor; one of WALKING WALKING_UPSTAIRS WALKING_DOWNSTAIRS SITTING STANDING LAYING
	
For the variables below, {X,Y,Z} expands each variable to three,
labeled identically except one uses "X," one "Y," and one "Z."
Similarly, {mean,stddev} expands to two variables. These names were
automatically derived from the Smartlab features.txt file, converted
for syntactic compatibility with R.

Mean of means and standard deviations of time domain estimates,
across three axes:

Name|Expands To
------------------------------------------ | -----------------
mean.tBodyAcc.{mean,stddev}.{X,Y,Z} | mean.tBodyAcc.mean.X, mean.tBodyAcc.mean.Y, mean.tBodyAcc.mean.Z, mean.tBodyAcc.stddev.X, mean.tBodyAcc.stddev.Y, mean.tBodyAcc.stddev.Z
mean.tGravityAcc.{mean,stddev}.{X,Y,Z} | _etc_
mean.tBodyAccJerk.{mean,stddev}.{X,Y,Z} |
mean.tBodyGyro.{mean,stddev}.{X,Y,Z} |
mean.tBodyGyroJerk.{mean,stddev}.{X,Y,Z} |
mean.tBodyAccMag.{mean,stddev} | mean.tBodyAccMag.mean, mean.tBodyAccMag.stddev
mean.tGravityAccMag.{mean,stddev} | _etc_
mean.tBodyAccJerkMag.{mean,stddev} |
mean.tBodyGyroMag.{mean,stddev} |
mean.tBodyGyroJerkMag.{mean,stddev} |

Mean of means and standard deviations of frequency domain estimates,
across three axes:

- mean.fBodyAcc.{mean,stddev}.{X,Y,Z}
- mean.fBodyAccJerk.{mean,stddev}.{X,Y,Z}
- mean.fBodyGyro.{mean,stddev}.{X,Y,Z}

Mean of means and standard deviations of frequency domain magnitude
estimates:

- mean.fBodyAccMag.{mean,stddev}
- mean.fBodyBodyAccJerkMag.{mean,stddev}
- mean.fBodyBodyGyroMag.{mean,stddev}
- mean.fBodyBodyGyroJerkMag.{mean,stddev}
