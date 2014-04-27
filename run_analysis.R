run_analysis <- function() 
{
    ## Peer Assignment
    ## Coursera Getting and Cleaning Data
    ## J-Sieber 4/27/2014
    
    ## Read in Data
    X_test<-read.table("./UCI HAR Dataset/test/X_test.txt", quote="\"")
    y_test<-read.table("./UCI HAR Dataset/test/y_test.txt", quote="\"")
    X_train<-read.table("./UCI HAR Dataset/train/X_train.txt", quote="\"")
    y_train<-read.table("./UCI HAR Dataset/train/y_train.txt", quote="\"")
    Subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt"
                             , quote="\"")
    Subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt"
                              ,quote="\"")
    features<-read.table("./UCI HAR Dataset/features.txt",quote="\"")
    activity_labels<-read.table("./UCI HAR Dataset/activity_labels.txt"
                                ,quote="\"")
    
    ## Step 1
    ## Merge test and training 
    yNames<-c("Activity","Subject")
    y_test<-cbind(y_test,Subject_test)
    names(y_test)<-yNames
    y_train<-cbind(y_train,Subject_train)
    ## Step3
    ## Name the activities
    names(y_train)<-yNames
    y<-rbind(y_train,y_test)
    
    X<-rbind(X_train,X_test)
    ## Step 4
    ## Name the Columns
    names(X)<-features[,2]
    
    
    ## Step 2
    ## Extract mean and standard deviation
    meanSD_col<-(regexpr("mean\\(\\)",names(X))>0 | 
                     regexpr("std\\(\\)",names(X))>0)
    X<-X[meanSD_col]
    myData<-cbind(y,X)
    
    
    ## Step 5
    ## Create new tidy data set with Averages
    myData$ActSub<-interaction(myData$Activity,myData$Subject)
    s<-split(myData,myData$ActSub)
    SubMeans<-sapply(s,function(myData) colMeans(myData[,names(myData)[3:68]]))
}
