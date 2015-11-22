##Read test dataset
X_test <- read.table("./test/X_test.txt")
Y_test <- read.table("./test/y_test.txt")
subject_test <- read.table("./test/subject_test.txt")
names(Y_test) <- "activity"
names(subject_test) <- "subject"
test <- cbind(subject_test,Y_test,X_test)
##Read Train dataset
X_train <- read.table("./train/X_train.txt")
Y_train <- read.table("./train/y_train.txt")
subject_train <- read.table("./train/subject_train.txt")
names(Y_train) <- "activity"
names(subject_train) <- "subject"
train <- cbind(subject_train,Y_train,X_train)
##Merge data
data <- merge(test,train,by=c("subject","activity"),all=T)
##Extract particular measurements
#Read features
features <- read.table("features.txt",col.names = c("seq","name"))
selected_featuresNo <- grep(".*[Mm]ean.*|.*std.*",features$name)
extractIndex <- c(1,2,selected_featuresNo+2,selected_featuresNo+561+2)
data_extracted <- data[extractIndex]
#Uses descriptive activity names to name the activities in the data set
activity_labels <- read.table("activity_labels.txt",col.names = c("seq","name"))
data_extracted$activity <- activity_labels$name[data_extracted$activity]
#Labels the data set with descriptive variable names
feature_name <- features$name[selected_featuresNo]
names(data_extracted)[3:(length(selected_featuresNo)+2)] <- as.character(feature_name)
names(data_extracted)[(length(selected_featuresNo)+3):length(data_extracted)] <- paste(as.character(feature_name),".y")
dataset1 <- data_extracted
rm(data_extracted)
##step 5
smean <- function(a){mean(a , na.rm = T)}
dataset2 <- dataset1 %>% group_by(activity,subject) %>%
          summarise_each(funs(smean))
#save as text
write.table(dataset2,"dataset.txt",row.name=FALSE) 