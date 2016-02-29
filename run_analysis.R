dir<-getwd()

fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

download.file(fileUrl ,destfile="UCI HAR Dataset.zip" ,method="curl")

unzip("UCI HAR Dataset.zip")

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


mergeData<-rbind(train,test)

newData<-mergeData[order(mergeData$subject,mergeData$activity), ]

Means_STDs<-grep("subject|activity|.*?-mean[^Freq]().*?|.*?-std().*?",names(newData),value=TRUE)

newData_1<-newData[ ,c(Means_STDs)]
	
	x<-c("^f", "^t", "BodyBody", "Acc","Gyro", "Mag", "mean", "std","\\-","\\(\\)")

	y<-c("Freq","Time", "Body", "Accelerometer", "Gyroscope", "Magnitude", "Mean","Std","","")
	
	newNames<-Means_STDs
		
	for(i in 1:length(x)){newNames<-gsub(x[i],y[i],newNames)}

	names(newData_1)<-newNames

	newData_1$activity<-as.factor(newData_1$activity)

	levels(newData_1$activity)<-activity_labels[ ,2]

	Splitted_newData_1<-split(newData_1,list(newData_1$subject,newData_1$activity))

newData_2<-sapply(Splitted_newData_1,function(x){colMeans(x[, c(-1,-2)])})

newData_3<-data.frame(t(newData_2))

	var<-rownames(newData_3)

	Splitted_var<-strsplit(var,"\\.")

	subject<-sapply(Splitted_var,function(x){x[1]})

	activity<-sapply(Splitted_var,function(x){x[2]})

newData_4<-cbind(subject,activity, newData_3)

	newData_4$subject<-factor(newData_4$subject, levels=1:30)

	rownames(newData_4)<-NULL

newData_5<-newData_4[order(newData_4$subject,newData_5$activity),]

	rownames(newData_5)<-NULL

	write.table(newData_5,file="course project.txt",row.names=FALSE)


















