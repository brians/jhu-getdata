jhu-getdata
===========

JHU Getting and Cleaning Data Project

run_analysis.R merges smart phone accelerometer and gyroscope data
in several files released from the _Human Activity Recognition Using
Smartphones Dataset_ by Smartlab (www.smartlab.ws), collapsing the
mean and standard deviation variables for each (Subject, Activity)
pair into means of those variables. The script concatenates the Train and Test sets
and thus does not distinguish between them. The resulting data are written
out into a single table format file suitable for reading into R by
running:

read.table("averages.txt", header = TRUE)

The single script requires that the files from the Smartlab _Dataset_:

- X_train.txt
- subject_train.txt
- y_train.txt
- X_test.txt
- subject_test.txt
- y_test.txt
- features.txt
- activity_labels.txt

exist in the working directory of run_analysis.R. Sourcing the file suffices to produce "activity.txt," as described in the included codebook.

run_analysis.R also includes a function spotCheck() not used to
produce activity.txt, but to check correctness of results from any
reshaping functions under evaluation for future use.
