run_analysis <- function () {
  ############ Point 1: Merges the training and the test sets to create one data set.
  ## read X_train & X_test data sets and combine them
  X_train <- read.table("train//X_train.txt")
  X_test <- read.table("test//X_test.txt")
  combinedX <- rbind(X_train,X_test)
  ## read activities data sets and combine them
  Y_train <- read.table("train//Y_train.txt")
  Y_test <- read.table("test//Y_test.txt")
  combinedY <- rbind(Y_train, Y_test)
  
  ## read subject data sets and combine them
  subject_train <- read.table("train//subject_train.txt")
  subject_test <- read.table("test//subject_test.txt")
  combinedsubject <- rbind(subject_train, subject_test)
  
  ## Combine subject, Y data and X data
  #bigdataset <- cbind(combinedsubject,combinedY,combinedX)
  
  ## Subject into factors
  #bigdataset$Subject <- as.factor(bigdataset$Subject) 
  
  
  
  ############ Point 2: Extracts only the measurements on the mean and standard deviation for each measurement. 
  ## read column names
  features <- read.table("features.txt")
    
  ## Find the position with the measurements with "mean()" or std() in the name
  meanstdpositions <- grep("mean\\(\\)|std\\(\\)",features[,2])
  
  ## Reduce combinedX dataset to have only mean() and std() variables
  combinedX <- combinedX[,meanstdpositions]
  
  
  ############ Point 3: Uses descriptive activity names to name the activities in the data set
  ## read activity names from file
  labels <- read.table("activity_labels.txt")
  ## copy the activity names into the combined activity dataset as text
  for (i in 1:length(combinedY[,1])) combinedY[i,1] <- as.character(labels[combinedY[i,1],2])
  ## Activities into factors
  combinedY[,1] <- as.factor(combinedY[,1])
  
  
  ############ Point 4: Appropriately labels the data set with descriptive variable names. 
  ## write column names into combinedX, using the features variable read in point 2
  names(combinedX) <- features[meanstdpositions,2]
  # write the name of the colums "Activity" and "Subject"
  names(combinedY) <- "Activity"
  
  names(combinedsubject) <- "Subject"
  # combine all the data
  combineddata <- cbind(combinedsubject,combinedY,combinedX)
  
  
  ############ Point 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  # get dimentions to create the result matrix
  sub <- length(table(combinedsubject))
  act <- length(table(combinedY))
  col <- dim(combineddata)[2]
  # create the result matrix and make it Data Frame
  result <- matrix(NA, nrow=sub*act,ncol=col)
  result <- as.data.frame(result)
  # copy column names
  colnames(result) <- colnames(combineddata)
  
  row <- 1
  # fill the result table with the averages
  for (i in 1:sub){
    for (j in 1:act){
      result[row,1] <- sort(unique(combinedsubject)[,1])[i]
      result[row,2] <- as.character(labels[j,2])
      bool1<- i == combineddata$Subject
      bool2 <- labels[j,2] == combineddata$Activity
      result[row, 3:col] <- colMeans(combineddata[bool1&bool2, 3:col])
      row <- row +1
    }
  }
  
  # write the result table into finaltable.txt
  write.table(result, "finaltable.txt", row.name = FALSE)
  
}
