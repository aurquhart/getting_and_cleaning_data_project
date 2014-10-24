#Course project

#Going to merge 2 sets of testing and training files


getwd() #get working directory
install.packages("plyr")
library(plyr)
#Question 1

#help(read.table)

#I have a features file that is the headers

df_features <- read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/features.txt",header = FALSE)
#class(df_features)
#Filter on just field I need and convert 
df_features_filtered = df_features$V2

#I also have an activity file which is a description for the labels field
df_activities <- read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt",header = FALSE)

#Read in test data
df_test <- read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt",header = FALSE,quote = "\"'",dec = ".",col.names = df_features_filtered)
#str(df_test) # 561 variables and 2947 records
#colnames(df_test)

#Read in test labels
df_test_label <- read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt",header = FALSE,quote = "\"'",dec = ".")
df_test_label <- rename(df_test_label, c("V1"="Label"))

#str(df_test_label) # 1 variable and 2947 records
#head(df_test_label)

#Read in subjects
df_test_subject <- read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt",header = FALSE,quote = "\"'",dec = ".")
df_test_subject <- rename(df_test_subject, c("V1"="Subject"))

#summary(df_test_subject)
#str(df_test_subject) # 1 variable and 2947 records
#head(df_test_subject)


#as these 3 have the same records I should be able to join them
#As subject and label are simplest I'll join them first

#help(rbind)
test_merge <-cbind(df_test_subject,df_test_label,df_test)

#Add a flag to show whether its test or train
test_merge$test_or_train <- "test"

#head(test_merge)


#Now do the same process for the training files
df_train <- read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt",header = FALSE,quote = "\"'",dec = ".",col.names = df_features_filtered)
df_train_label <- read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt",header = FALSE,quote = "\"'",dec = ".")
df_train_label <- rename(df_train_label, c("V1"="Label"))
df_train_subject <- read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt",header = FALSE,quote = "\"'",dec = ".")
df_train_subject <- rename(df_train_subject, c("V1"="Subject"))
train_merge <-cbind(df_train_subject,df_train_label,df_train)
train_merge$test_or_train <- "train"
#head(train_merge)


#Now append the train and test sets onto each other.  

total_df <- rbind(train_merge,test_merge)

#Add in the correct activity labels
total_df2 <- merge(total_df, df_activities, by.x = "Label", by.y = "V1")


total_df2 <- rename(total_df2, c("V2"="Activity"))

head(total_df2)
summary(total_df2)

#Next set of analysis will only focus on columns containing mean or SD for each variable
#Will not include mean frequency which is a different field
df_mean_sd <- total_df2[,c("Subject","Activity", "tBodyAcc.mean...X", "tBodyAcc.mean...Y" ,                  
                        "tBodyAcc.mean...Z","tBodyAcc.std...X","tBodyAcc.std...Y" ,"tBodyAcc.std...Z" ,
                        "tBodyAcc.std...X","tBodyAcc.std...Y","tBodyAcc.std...Z",
                        "tGravityAcc.mean...X","tGravityAcc.mean...Y"  ,"tGravityAcc.mean...Z", "tGravityAcc.std...X" ,                
                        "tGravityAcc.std...Y", "tGravityAcc.std...Z",
                        "tBodyAccJerk.mean...X","tBodyAccJerk.mean...Y" ,              
                        "tBodyAccJerk.mean...Z","tBodyAccJerk.std...X"  ,              
                        "tBodyAccJerk.std...Y","tBodyAccJerk.std...Z",
                        "tBodyGyro.mean...X", "tBodyGyro.mean...Y",               
                        "tBodyGyro.mean...Z","tBodyGyro.std...X"  ,                 
                        "tBodyGyro.std...Y","tBodyGyro.std...Z",
                        "tBodyGyroJerk.mean...X", "tBodyGyroJerk.mean...Y",           
                        "tBodyGyroJerk.mean...Z","tBodyGyroJerk.std...X"  ,             
                        "tBodyGyroJerk.std...Y","tBodyGyroJerk.std...Z",
                        "tBodyAccMag.mean..",     "tBodyAccMag.std..",
                        "tGravityAccMag.mean.." ,"tGravityAccMag.std..",
                        "tBodyAccJerkMag.mean..","tBodyAccJerkMag.std..",
                        "tBodyGyroMag.mean..", "tBodyGyroMag.std..",
                        "tBodyGyroJerkMag.mean.." ,"tBodyGyroJerkMag.std..",
                        "fBodyAcc.mean...X",        
                        "fBodyAcc.mean...Y", "fBodyAcc.mean...Z"  ,                 
                        "fBodyAcc.std...X","fBodyAcc.std...Y"  ,"fBodyAcc.std...Z",
                        "fBodyAccJerk.mean...X","fBodyAccJerk.mean...Y" ,              
                        "fBodyAccJerk.mean...Z", "fBodyAccJerk.std...X" ,               
                        "fBodyAccJerk.std...Y","fBodyAccJerk.std...Z" ,
                        "fBodyGyro.mean...X"  ,"fBodyGyro.mean...Y" ,"fBodyGyro.mean...Z" ,                 
                        "fBodyGyro.std...X", "fBodyGyro.std...Y" ,"fBodyGyro.std...Z",
                        "fBodyAccMag.mean..","fBodyAccMag.std..","fBodyBodyAccJerkMag.mean.." ,         
                        "fBodyBodyAccJerkMag.std..","fBodyBodyGyroMag.mean..","fBodyBodyGyroMag.std..",
                        "fBodyBodyGyroJerkMag.mean.." , "fBodyBodyGyroJerkMag.std..",
                        "test_or_train"                       
)]

#head(df_mean_sd)
#colnames(total_df)
#colnames(df_mean_sd)

#colnames(df_test)

#Finally get the average for each variable by activity
tidydata <- ddply(df_mean_sd, c("Activity","Subject"), summarise,
                  AVG_tBodyAcc.mean...Z =mean(                        tBodyAcc.mean...Z),
                  AVG_tBodyAcc.std...X =mean(                        tBodyAcc.std...X),
                  AVG_tGravityAcc.mean...X =mean(                        tGravityAcc.mean...X),
                  AVG_tGravityAcc.std...Y =mean(                        tGravityAcc.std...Y),
                  AVG_tBodyAccJerk.mean...X =mean(                        tBodyAccJerk.mean...X),
                  AVG_tBodyAccJerk.mean...Z =mean(                        tBodyAccJerk.mean...Z),
                  AVG_tBodyAccJerk.std...Y =mean(                        tBodyAccJerk.std...Y),
                  AVG_tBodyGyro.mean...X =mean(                        tBodyGyro.mean...X),
                  AVG_tBodyGyro.mean...Z =mean(                        tBodyGyro.mean...Z),
                  AVG_tBodyGyro.std...Y =mean(                        tBodyGyro.std...Y),
                  AVG_tBodyGyroJerk.mean...X =mean(                        tBodyGyroJerk.mean...X),
                  AVG_tBodyGyroJerk.mean...Z =mean(                        tBodyGyroJerk.mean...Z),
                  AVG_tBodyGyroJerk.std...Y =mean(                        tBodyGyroJerk.std...Y),
                  AVG_tBodyAccMag.mean.. =mean(                        tBodyAccMag.mean..),
                  AVG_tGravityAccMag.mean.. =mean(                        tGravityAccMag.mean.. ),
                  AVG_tBodyAccJerkMag.mean.. =mean(                        tBodyAccJerkMag.mean..),
                  AVG_tBodyGyroMag.mean.. =mean(                        tBodyGyroMag.mean..),
                  AVG_tBodyGyroJerkMag.mean.. =mean(                        tBodyGyroJerkMag.mean.. ),
                  AVG_fBodyAcc.mean...X =mean(                        fBodyAcc.mean...X),
                  AVG_fBodyAcc.mean...Y =mean(                        fBodyAcc.mean...Y),
                  AVG_fBodyAcc.std...X =mean(                        fBodyAcc.std...X),
                  AVG_fBodyAccJerk.mean...X =mean(                        fBodyAccJerk.mean...X),
                  AVG_fBodyAccJerk.mean...Z =mean(                        fBodyAccJerk.mean...Z),
                  AVG_fBodyAccJerk.std...Y =mean(                        fBodyAccJerk.std...Y),
                  AVG_fBodyGyro.mean...X =mean(                        fBodyGyro.mean...X  ),
                  AVG_fBodyGyro.std...X =mean(                        fBodyGyro.std...X),
                  AVG_fBodyAccMag.mean.. =mean(                        fBodyAccMag.mean..),
                  AVG_fBodyBodyAccJerkMag.std.. =mean(                        fBodyBodyAccJerkMag.std..),
                  AVG_fBodyBodyGyroJerkMag.mean.. =mean(                        fBodyBodyGyroJerkMag.mean.. ),
                  AVG_tBodyAcc.mean...Y =mean( tBodyAcc.mean...Y ),
                  AVG_tBodyAcc.std...X =mean(tBodyAcc.std...X),
                  AVG_tBodyAcc.std...Y =mean(tBodyAcc.std...Y),
                  AVG_tGravityAcc.mean...Y =mean(tGravityAcc.mean...Y  ),
                  AVG_tGravityAcc.std...Z =mean( tGravityAcc.std...Z),
                  AVG_tBodyAccJerk.mean...Y =mean(tBodyAccJerk.mean...Y ),
                  AVG_tBodyAccJerk.std...X =mean(tBodyAccJerk.std...X  ),
                  AVG_tBodyAccJerk.std...Z =mean(tBodyAccJerk.std...Z),
                  AVG_tBodyGyro.mean...Y =mean( tBodyGyro.mean...Y),
                  AVG_tBodyGyro.std...X =mean(tBodyGyro.std...X  ),
                  AVG_tBodyGyro.std...Z =mean(tBodyGyro.std...Z),
                  AVG_tBodyGyroJerk.mean...Y =mean( tBodyGyroJerk.mean...Y),
                  AVG_tBodyGyroJerk.std...X =mean(tBodyGyroJerk.std...X  ),
                  AVG_tBodyGyroJerk.std...Z =mean(tBodyGyroJerk.std...Z),
                  AVG_tBodyAccMag.std.. =mean(     tBodyAccMag.std..),
                  AVG_tGravityAccMag.std.. =mean(tGravityAccMag.std..),
                  AVG_tBodyAccJerkMag.std.. =mean(tBodyAccJerkMag.std..),
                  AVG_tBodyGyroMag.std.. =mean( tBodyGyroMag.std..),
                  AVG_tBodyGyroJerkMag.std.. =mean(tBodyGyroJerkMag.std..),
                  AVG_fBodyAcc.std...Z =mean(fBodyAcc.std...Z),
                  AVG_fBodyAcc.mean...Z =mean( fBodyAcc.mean...Z  ),
                  AVG_fBodyAcc.std...Y =mean(fBodyAcc.std...Y  ),
                  AVG_fBodyAccJerk.mean...Y =mean(fBodyAccJerk.mean...Y ),
                  AVG_fBodyAccJerk.std...X =mean( fBodyAccJerk.std...X ),
                  AVG_fBodyAccJerk.std...Z =mean(fBodyAccJerk.std...Z ),
                  AVG_fBodyGyro.mean...Y =mean(fBodyGyro.mean...Y ),
                  AVG_fBodyGyro.std...Y =mean( fBodyGyro.std...Y ),
                  AVG_fBodyAccMag.std.. =mean(fBodyAccMag.std..),
                  AVG_fBodyBodyGyroMag.mean.. =mean(fBodyBodyGyroMag.mean..),
                  AVG_fBodyBodyGyroJerkMag.std.. =mean( fBodyBodyGyroJerkMag.std..),
                  AVG_fBodyGyro.mean...Z =mean(fBodyGyro.mean...Z ),
                  AVG_fBodyGyro.std...Z =mean(fBodyGyro.std...Z),
                  AVG_fBodyBodyAccJerkMag.mean.. =mean(fBodyBodyAccJerkMag.mean.. ),
                  AVG_fBodyBodyGyroMag.std.. =mean(fBodyBodyGyroMag.std..),
                  AVG_tBodyAcc.std...Y =mean(tBodyAcc.std...Y ),
                  AVG_tBodyAcc.std...Z =mean(tBodyAcc.std...Z),
                  AVG_tGravityAcc.mean...Z =mean(tGravityAcc.mean...Z),
                  AVG_tBodyAcc.std...Z =mean(tBodyAcc.std...Z ),
                  AVG_tGravityAcc.std...X =mean( tGravityAcc.std...X )
                                    
)

tidydata


tidydf <- by(df_mean_sd$"tBodyAcc.mean...X",df_mean_sd$Activity,mean)
tidydf
