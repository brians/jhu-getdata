setwd("~/Documents/jhu-datascience/getdata/jhu-getdata")

if (!file.exists("data")) {
    dir.create("data")
    url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(url, destfile = "./data/Dataset.zip", method = "curl")
    list.files("./data")
    dateDownloaded <- date()
    dateDownloaded
    # from shell, unzip data archive
} else {
    print("./data file (directory) already exists.")
}
if (!file.exists("X_train.txt")) {
    stop("Please unzip data/Dataset.zip and copy required .txt files per README.md")
    exit
}

#
# 1.
# Merge the training and the test sets to create one data set.
#
# read the training data.
xTrain <- read.table("./X_train.txt")
subjTrain <- read.table("./subject_train.txt")
actTrain <- read.table("./y_train.txt")

# read the test data.
xTest <- read.table("./X_test.txt")
subjTest <- read.table("./subject_test.txt")
actTest <- read.table("./y_test.txt")

# assemble into single data frame.
all <- rbind(cbind(subjTrain, actTrain, xTrain),
             cbind(subjTest, actTest, xTest))

#
# 2.
# Add raw variable names from features.txt. Use these names to
# identify the measurements on the mean and standard deviation for each
# observation, and retain only those.
#
features <- read.table("./features.txt",
                       stringsAsFactors = FALSE)
names(features) <- c("featureIndex", "featureName")

# add headings for subject and activity code to the extracted feature names.
# we extract numeric activity codes here, but we will update them in step (3)
# to levels labeled with the activity.
varNames <- c("Subject", "Activity", features$featureName)

# identify columns to retain based on RE pattern.
meanOrSD <- grep("(-mean|-std)\\(\\)", features$featureName)
# add the subject and activity indices to the matched features.
varIndices <- c(1, 2, meanOrSD + 2)
# prepare for step (4), which corrects the syntactically dubious raw names.
names(all) <- varNames

# keep the desired columns.
all <- all[, varIndices]

#
# 3.
# Convert activity indices to descriptive factor labels matching
# activity names.
#
labels <- read.table("./activity_labels.txt")
names(labels) <- c("Index", "Label" )
# thanks to scot k waye, PhD, from the forum:
all$Activity <- factor(all$Activity, labels = labels$Label)

#
# 4.
# Appropriately label the data set with descriptive variable names. 
#
# "stddev" is more descriptive than "std"
newNames <- gsub("std\\(\\)", "stddev", varNames[varIndices])
# drop parens
newNames <- gsub("\\(\\)", "", newNames)
# make names syntactically valid
names(all) <- make.names(newNames)

#
# 5.
# From the data set in step 4, creates a second, independent tidy data set with
# the average of each variable for each activity and each subject.
#
library(reshape2)
allmelt <- melt(all, id.vars = c("Subject","Activity"))
averages <- dcast(allmelt, Subject+Activity ~ ..., mean)
newNames <- names(averages)
# prepend "mean." to variable names.
newNames[3:length(newNames)] <- gsub("(.*)", "mean.\\1",
                                     newNames[3:length(newNames)])
# newNames should already be valid, but just in case.
names(averages) <- make.names(newNames)
write.table(averages, file="averages.txt", row.names = FALSE)

# end of assignment.
# spotCheck() is available from the console in case of any worries.


# function to check averages data table
spotCheck <- function(n = 14, e = 1e-6, verbose = F) {
    # n random trials to verify we called melt / dcast correctly
    # for results within error e.
    for (i in 1:n) {
        sVar <- sample(names(all)[3:length(names(all))], 1)
        # ``all'' is the datatable constructed as part 4
        sSubj <- sample(unique(all$Subject), 1)
        # ``labels$Label'' is a vector of factors from activity_labels.txt
        sLabel <- sample(labels$Label, 1)
        if (verbose)
            message("checking", " ", sSubj, ", ", sLabel, ", ", sVar)
        # ``averages'' is the datatable intended for submission for part 5
        sAverageVar <- paste("mean.", sVar, sep = "")
        if (abs(mean(all[all$Subject == sSubj & all$Activity == sLabel, sVar])
                - averages[averages$Subject == sSubj &
                               averages$Activity == sLabel, sAverageVar]) > e)
            return(F)
    }
    return(T)
}


