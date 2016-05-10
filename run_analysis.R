## read test dataset
testdat <- read.table("test/X_test.txt")
testlabel <- read.table("test/y_test.txt")
## read train dataset
trainlabel <- read.table("train/y_train.txt")
traindat <- read.table("train/X_train.txt")
## merging train and test data rows
dat <- rbind(traindat, testdat)
dim(dat)
##[1] 10299   561
dim(testdat)
##[1] 2947  561
dim(traindat)
##[1] 7352  561
## merging train and test label rows
label <- rbind(trainlabel, testlabel)
dim(label)
##[1] 10299     1
##Uses descriptive activity names to name the activities in the data set.
trainsub <- read.table("train/subject_train.txt")
testsub <- read.table("test/subject_test.txt")
allsub <- rbind(trainsub, testsub)   
actLabel  <- read.table("activity_labels.txt")
act <- as.factor(label$V1)
levels(act) <- actLabel$V2
sub <- as.factor(allsub$V1)

## merging data and label columns
totdat <- cbind(sub, act, dat)
dim(totdat)
## [1] 10299   563

## read the feature names
featurenames   <- read.table("features.txt")
head(featurenames)
## find the mean and standard deviation features 
meanstdfeatures  <- grepl("(-std\\(\\)|-mean\\(\\))",featurenames$V2)

## Extracts only the measurements on the mean and standard deviation for each measurement
filteredActivity <- totdat[, which(meanstdfeatures == TRUE)]
head(filteredActivity)

## Appropriately label the data set with descriptive variable names
filteredfeatures <- (cbind(featurenames,meanstdfeatures)[meanstdfeatures==TRUE,])$V2
head(filteredfeatures)
## remove the hyphens and paranthesis in filtered feature names
rmhyphen <- function(featurename) {
    tolower(gsub("(\\(|\\)|\\-)","",featurename))
}
filteredfeatures <- sapply(filteredfeatures,rmhyphen)

## replace the column names of the filtered activity data with filter feature names
names(filteredActivity)[3:ncol(filteredActivity)] <- filteredfeatures
head(filteredActivity)
## write filtered data into a csv file
write.csv(filteredActivity,file="cleandataset.csv")

##From the data set in step 4, creates a second, independent tidy data set with the 
## average of each variable for each activity and each subject
library(reshape2)
## "melt" data so that each row is a unique id-variable combination. 
## Then you "cast" the melted data into any shape you would like.
molt <- melt(filteredActivity,id.vars=c("sub","act"))
head(molt)
##  sub      act      variable     value
##1   1 STANDING tbodyaccmeanx 0.2885845
##2   1 STANDING tbodyaccmeanx 0.2784188
##3   1 STANDING tbodyaccmeanx 0.2796531
##4   1 STANDING tbodyaccmeanx 0.2791739
##5   1 STANDING tbodyaccmeanx 0.2766288
##6   1 STANDING tbodyaccmeanx 0.2771988

dim(molt)
##[1] 659136      4
tidy <- dcast(molt,sub + act ~ variable,mean)
head(tidy)
## write tidy dataset to a text file
write.table(tidy, "tidydataset.txt", sep="\t")

