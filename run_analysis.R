#Course Project

# Goals:
# 1) run_analysis.R script for Course Project
# 2) README.md file for repo on Github describing how all of the scripts work and how they are connected
# 3) Codebook.md file to describe variables (units also)


#run_analysis.R script for Course Project accomplishes 1) above
#1. Merges the training and the test sets to create one data set

# Read in the training folder for subject_train.txt, X_train.txt, and y_train.txt. 
subject_train<- read.table(subject_train.txt)) # create a data frame called subject_train by reading in the text file, on local computer read.table("./courseproject/train/subject_train.txt")
x_train<- read.table(X_train.txt)) # create a data frame called x_train by reading in the text file, on local computer read.table("./courseproject/train/X_train.txt")
y_train<- read.table(y_train.txt)) # create a data frame called y_train by reading in the text file, on local computer read.table("./courseproject/train/y_train.txt")

#Combine columns of subject_train, x_train, y_train to a new data frame called traindata
traindata<- cbind(subject_train, y_train, x_train)

#Add column labels to traindata. Column 1 is the subject ID, Column 2 is the activity number (using activity_labels can add another column), column 3 through 563 can use features.txt to label
features<- read.table(features.txt)) #read in features.txt; on local computer read.table("./courseproject/features.txt")
column_names<- as.character(features[,2]) #2nd column has names, also set as character vector
column_names2<- c("subjectID","activity",column_names) #column_names2 vector adding "subjectID" and"activity" to the column_names vector
colnames(traindata) <- column_names2 # Name columns in traindata

# Repeat the above steps for test data
subject_test<- read.table(subject_test.txt)) # create a data frame called subject_train by reading in the text file, on local computer read.table("./courseproject/test/subject_test.txt")
x_test<- read.table(X_test.txt)) # create a data frame called x_train by reading in the text file, on local computer read.table("./courseproject/test/X_test.txt")
y_test<- read.table(y_test.txt)) # create a data frame called y_train by reading in the text file, on local computer read.table("./courseproject/test/y_test.txt")
testdata<- cbind(subject_test, y_test, x_test)
colnames(testdata) <- column_names2 

# Combine the two data sets by rbind into a new data frame called totalset
totalset<- rbind(traindata, testdata)

#2. Extracts only the measurements on the mean and standard deviation for each measurement. Subset totalset to only include subjectID, activity, and only mean and std() of all measurements
substring="mean|std" # set a variable to check if features contain "mean" or std" in character
meanstd<- grepl(substring, column_names) #check column_names vector to see if characters contains substring values
colselect<- c(TRUE, TRUE, meanstd) # Vector that identifies subjectID, activity, and column names that contain "mean" or "std" in character
meansubset<- subset(totalset, select= colselect)        # subset for desire data frame
nofreq<- grepl("Freq", colnames(meansubset))		#identify columns with "Freq"" in the column name since there are columns with both "mean" and "Freq" in the name
meansubset<- subset(meansubset, select= !nofreq)	# subset to remove columns with ""Freq" in the column name

#3. Uses descriptive activity names to replace the activity numbers in the data set
# Create a new column called activityname that matches label with number
meansubset$activityname<-"not yet set"
meansubset$activityname[meansubset$activity == 1] <- "WALKING"
meansubset$activityname[meansubset$activity == 2] <- "WALKING_UPSTAIRS"
meansubset$activityname[meansubset$activity == 3] <- "WALKING_DOWNSTAIRS"
meansubset$activityname[meansubset$activity == 4] <- "SITTING"
meansubset$activityname[meansubset$activity == 5] <- "STANDING"
meansubset$activityname[meansubset$activity == 6] <- "LAYING"
# Get rid of the original activity column, move activityname column to column number 2. Save into meansubset2
meansubset2<-meansubset[,-2]
meansubset2<- meansubset2[,c(1,68,2:67)]

#4. Appropriately label- the data set with descriptive variable names - name the columns with better descriptive names
names(meansubset2)[3] <- "bodyaccelX_meantime"
names(meansubset2)[4] <- "bodyaccelY_meantime"
names(meansubset2)[5] <- "bodyaccelZ_meantime"
names(meansubset2)[6] <- "bodyaccelX_stdtime"
names(meansubset2)[7] <- "bodyaccelY_stdtime"
names(meansubset2)[8] <- "bodyaccelZ_stdtime"
names(meansubset2)[9] <- "gravityaccelX_meantime"
names(meansubset2)[10] <- "gravityaccelY_meantime"
names(meansubset2)[11] <- "gravityaccelZ_meantime"
names(meansubset2)[12] <- "gravityaccelX_stdtime"
names(meansubset2)[13] <- "gravityaccelY_stdtime"
names(meansubset2)[14] <- "gravityaccelZ_stdtime"
names(meansubset2)[15] <- "jerkaccelX_meantime"
names(meansubset2)[16] <- "jerkaccelY_meantime"
names(meansubset2)[17] <- "jerkaccelZ_meantime"
names(meansubset2)[18] <- "jerkaccelX_stdtime"
names(meansubset2)[19] <- "jerkaccelY_stdtime"
names(meansubset2)[20] <- "jerkaccelZ_stdtime"
names(meansubset2)[21] <- "bodygyroX_meantime"
names(meansubset2)[22] <- "bodygyroY_meantime"
names(meansubset2)[23] <- "bodygyroZ_meantime"
names(meansubset2)[24] <- "bodygyroX_stdtime"
names(meansubset2)[25] <- "bodygyroY_stdtime"
names(meansubset2)[26] <- "bodygyroZ_stdtime"
names(meansubset2)[27] <- "jerlgyroX_meantime"
names(meansubset2)[28] <- "jerlgyroY_meantime"
names(meansubset2)[29] <- "jerlgyroZ_meantime"
names(meansubset2)[30] <- "jerkgyroX_stdtime"
names(meansubset2)[31] <- "jerkgyroY_stdtime"
names(meansubset2)[32] <- "jerkgyroZ_stdtime"
names(meansubset2)[33] <- "bodyaccelmagnitude_meantime"
names(meansubset2)[34] <- "bodyaccelmagnitude_stdtime"
names(meansubset2)[35] <- "gravityaccelmagnitude_meantime"
names(meansubset2)[36] <- "gravityaccelmagnitude_stdtime"
names(meansubset2)[37] <- "jerkaccelmagnitude_meantime"
names(meansubset2)[38] <- "jerkaccelmagnitude_stdtime"
names(meansubset2)[39] <- "bodygyromagnitude_meantime"
names(meansubset2)[40] <- "bodygyromagnitude_stdtime"
names(meansubset2)[41] <- "jerkgyromagnitude_meantime"
names(meansubset2)[42] <- "jerkgyromagnitude_stdtime"
names(meansubset2)[43] <- "bodyaccelX_meanfourier"
names(meansubset2)[44] <- "bodyaccelY_meanfourier"
names(meansubset2)[45] <- "bodyaccelZ_meanfourier"
names(meansubset2)[46] <- "bodyaccelX_stdfourier"
names(meansubset2)[47] <- "bodyaccelY_stdfourier"
names(meansubset2)[48] <- "bodyaccelZ_stdfourier"
names(meansubset2)[49] <- "jerkaccelX_meanfourier"
names(meansubset2)[50] <- "jerkaccelY_meanfourier"
names(meansubset2)[51] <- "jerkaccelZ_meanfourier"
names(meansubset2)[52] <- "jerkaccelX_stdfourier"
names(meansubset2)[53] <- "jerkaccelY_stdfourier"
names(meansubset2)[54] <- "jerkaccelZ_stdfourier"
names(meansubset2)[55] <- "bodygyroX_meanfourier"
names(meansubset2)[56] <- "bodygyroY_meanfourier"
names(meansubset2)[57] <- "bodygyroZ_meanfourier"
names(meansubset2)[58] <- "bodygryoX_stdfourier"
names(meansubset2)[59] <- "bodygryoY_stdfourier"
names(meansubset2)[60] <- "bodygryoZ_stdfourier"
names(meansubset2)[61] <- "bodyaccelmagnitude_meanfourier"
names(meansubset2)[62] <- "bodyaccelmagnitude_stdfourier"
names(meansubset2)[63] <- "jerkaccelmagnitude_meanfourier"
names(meansubset2)[64] <- "jerkaccelmagnitude_stdfourier"
names(meansubset2)[65] <- "bodygyromagnitude_meanfourier"
names(meansubset2)[66] <- "bodygyromagnitude_stdfourier"
names(meansubset2)[67] <- "jerkgyromagnitude_meanfourier"
names(meansubset2)[68] <- "jerkgyromagnitude_stdfourier"

# Definitions can be found in the Code Book in this repo.

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(reshape2)       # Open reshape2 pacakge
tidyset<- melt(meansubset2, id=c("subjectID", "activityname"))  #Melt meansubset2 by subjectID and activityname to create tidyset
result <- dcast(tidyset, subjectID + activityname ~ ..., mean) # Average each column by activityname and subjectID

#Create text file "courseproject.txt" that has the results from #5. The file is saved to the working dir
write.table(result,file= "courseproject.txt", row.names=FALSE )

