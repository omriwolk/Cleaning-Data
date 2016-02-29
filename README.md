# Cleaning-Data
The purpose of this script is to process the raw data from the samsung experiment into a single tidy dataset that displays the mean  of each measurment of each activity for each person

step 1: Downloading the dataset##
The Directory was set


dir<-getwd()

The URL was copied from the Coursera Website

fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

The file was downloaded to the working directory and unzipped#

download.file(fileUrl ,destfile="UCI HAR Dataset.zip" ,method="curl")

unzip("UCI HAR Dataset.zip")



step 2: Loading to R##


 For every file in the UCI HAR Dataset folder, a path was cnstructed using the file.path function, and subsequently loaded into R using the read.table function
 the elements of the train and test file were joined by the cbind function in the order indicated by their ReadME files
 
file_path_features<-file.path(dir,"UCI HAR Dataset", "features.txt")

	features<-read.table(file_path_features)

file_path_activity_labels<-file.path(dir, "UCI HAR Dataset", "activity_labels.txt")

	activity_labels<-read.table(file_path_activity_labels)


file_path_subject_train<-file.path(dir, "UCI HAR Dataset", "train", "subject_train.txt")

	subject_train<-read.table(file_path_subject_train)
	
		names(subject_train)<-"subject"

file_path_y_train<-file.path(dir, "UCI HAR Dataset", "train", "y_train.txt")

	y_train<-read.table(file_path_y_train)

		names(y_train)<-"activity"

file_path_X_train<-file.path(dir, "UCI HAR Dataset", "train", "X_train.txt")

	X_train<-read.table(file_path_X_train)
	
		names(X_train)<-features[ ,2]


train<-cbind(subject_train, y_train, X_train)



file_path_subject_test<-file.path(dir, "UCI HAR Dataset", "test", "subject_test.txt")

	subject_test<-read.table(file_path_subject_test)

		names(subject_test)<-"subject"

file_path_y_test<-file.path(dir, "UCI HAR Dataset", "test", "y_test.txt")

	y_test<-read.table(file_path_y_test)

		names(y_test)<-"activity"
	

file_path_X_test<-file.path(dir, "UCI HAR Dataset", "test", "X_test.txt")

	X_test<-read.table(file_path_X_test)

		names(X_test)<-features[ ,2]

test<-cbind(subject_test, y_test, X_test)


step 3: Joining of the train and test sets by the rbind function##


mergeData<-rbind(train,test)


##step 4: Ordering the merged dataset by subject and activity


newData<-mergeData[order(mergeData$subject,mergeData$activity), ]


##step 5: Variable selection, ; only variables with -mean() or snntd() markings were selected, 


Means_STDs<-grep("subject|activity|.*?-mean[^Freq]().*?|.*?-std().*?",names(newData),value=TRUE)

newData_1<-newData[ ,c(Means_STDs)]

## step 6: renaming the variables with descriptive names: patters were replaced with more descriptive ones: abbreviations became full words, the "-" and "()" marks were removed, and repetitions were removed
	replacing patterns is a convinient alternative to manually changing each individual variable


	x<-c("^f", "^t", "BodyBody", "Acc","Gyro", "Mag", "mean", "std","\\-","\\(\\)")

	y<-c("Freq","Time", "Body", "Accelerator", "Gyroscope", "Magnitude", "Mean","Std","","")
	
	newNames<-Means_STDs
		
	for(i in 1:length(x)){newNames<-gsub(x[i],y[i],newNames)}

	names(newData_1)<-newNames

	##step 7: labeling the activities with descriptive terms, this was done by converting the activity variable column to a factor, and entering the activity lables from the raw  files as levels
	
	newData_1$activity<-as.factor(newData_1$activity)

	
levels(newData_1$activity)<-activity_labels[ ,2]



	
## step 8: calculating the means for the variables in each activity and each subject subsets, this was achived by splitting the dataframe by the subject and activity, and sapply the resulting subsets with the colMeans function


	Splitted_newData_1<-split(newData_1,list(newData_1$subject,newData_1$activity))


newData_2<-sapply(Splitted_newData_1,function(x){colMeans(x[, c(-1,-2)])})

##step 9: The output of the colMeans function is a matrix that fused the "subject" and "activity" names and transposed the data,and so the matrix was turned back to a datqaframe and reposed, and
the fused rownames were splitted to recrate the "subject" and "activity " variables



newData_3<-data.frame(t(newData_2))

	var<-rownames(newData_3)

	Splitted_var<-strsplit(var,"\\.")

	subject<-sapply(Splitted_var,function(x){x[1]})

	activity<-sapply(Splitted_var,function(x){x[2]})

newData_4<-cbind(subject,activity, newData_3)

Step 10: the dataset is ordered by subject and activity and written to a text file

	newData_4$subject<-factor(newData_4$subject, levels=1:30)

	rownames(newData_4)<-NULL

newData_5<-newData_4[order(newData_4$subject,newData_5$activity),]

	rownames(newData_5)<-NULL

	write.table(newData_5,file="course project.txt",row.names=FALSE)
