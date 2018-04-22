#########################################################################################
#########################################################################################
#INSOFE - Batch 14 - Job Recommendation Engine
#########################################################################################
#########################################################################################
rm(list=ls())
setwd("/Users/Saatwik/Documents/INSOFE/Final Project/Job Recommendation Engine/Job recommendation engine")
files<-dir()
files

#########################################################################################
#Load the required libraries
require(DMwR)
require(reshape2)
require(recommenderlab)
require(ggplot2)
#########################################################################################
#Manually go thru all the files and look at the central statistics for each of the data
#do the basic steps like checking NA's, droping attributes
#########################################################################################
#List of files used after examining them
#1. main_info
#2. Education
#3. Positions of Interest
#4. Combined_Jobs_Final
#5. Main Job Views
#6. Training Data
#####################################################################################
#Applicant related Information
#####################################################################################
#1.Education.csv
edu<-read.csv("Education.csv",header=T,sep=",")
dim(edu)
str(edu)
head(edu)
summary(edu)
sum(is.na(edu))
length(manyNAs(edu)) #rows having less then 20% of columns filled
summary(edu$Degree)
#drop all the data where degree is "N/A","n/a","N/a"
edu<-edu[!is.na(edu$Degree),]
edu<-subset(edu,!(edu$Degree %in% c("N/A","n/a","N/a")))
app_edu_data <- subset(edu, 
                         select = c(Applicant.ID,Graduate.Year,School.Name,City,Degree)
)
#Ensure that main education info data is sorted on applicant ID
app_edu_data<-app_edu_data[order(app_edu_data$Applicant.ID),]
rm(edu)

#####################################################################################
#2.Positions_Of_Interest.csv
poi<-read.csv("Positions_Of_Interest.csv",header=T,sep=",")
dim(poi)
str(poi)
head(poi)
summary(poi)
sum(is.na(poi))
length(manyNAs(poi)) #rows having less then 20% of columns filled

app_poi_data <- subset(poi, 
                        select = c(Applicant.ID,Position.Of.Interest)
)
#Ensure that interests info data is sorted on applicant ID
app_poi_data<-app_poi_data[order(app_poi_data$Applicant.ID),]
rm(poi)

#####################################################################################
#3.Main_Info.csv
main_info<-read.csv("Main_Info.csv",header=T,sep=",")
dim(main_info)
str(main_info)
head(main_info)
summary(main_info)
sum(is.na(main_info))
length(manyNAs(main_info)) #rows having less then 20% of columns filled

app_main_data <- subset(main_info, 
                       select = -c(State.Name,Estimated.Age, Sign.Up.Date, Authentication.Type, 
                                   Last.Sign.In.Date, Sign.In.Count, No.Of.Applied.Jobs, 
                                   Created.At, Updated.At)
                                  )
str(app_main_data)
#Ensure that main info data is sorted on applicant ID
app_main_data<-app_main_data[order(app_main_data$Applicant.ID),]
rm(main_info)
#####################################################################################
#Job Related Information
#####################################################################################
#1.Combined_Jobs_Final.csv
jobs_final<-read.csv("Combined_Jobs_Final.csv",header=T,sep=",")
dim(jobs_final)
str(jobs_final)
head(jobs_final)
summary(jobs_final)
sum(is.na(jobs_final))
length(manyNAs(jobs_final)) #rows having less then 20% of columns filled
length(unique(jobs_final$Job.ID))

#Selecting the required attributes from this data
job_data <- subset(jobs_final, 
                   select = c(Job.ID,Title,Position,Company,Job.Description,
                              Latitude,Longitude,Employment.Type,Education.Required,Industry)
)
#Ensure that main job info data is sorted on applicant ID
job_data<-job_data[order(job_data$Job.ID),]
rm(jobs_final)

#2.MainJobViews.csv
main_view<-read.csv("MainJobViews.csv",header=T,sep=",")
dim(main_view)
str(main_view)
head(main_view)
summary(main_view)
sum(is.na(main_view))
length(manyNAs(main_view)) #rows having less then 20% of columns filled
length(unique(main_view$Job.ID))

#Selecting the required attributes from this data
job_main_view <- subset(main_view, 
                   select = -c(Event.ID,Job.URL,Visit.Date.and.Time)
)
jobs_applied<-subset(job_main_view,Job.Applied == "yes",
                     select=c(Applicant.ID,Job.ID,Job.Applied))
jobs_applied<-jobs_applied[sort(jobs_applied$Applicant.ID),]
jobs_applied<-jobs_applied[!is.na(jobs_applied$Applicant.ID),]
jobs_applied_Matrix<-dcast(jobs_applied,Applicant.ID~Job.ID,fill=NULL)
rm(main_view)
#####################################################################################
#Applicant/Job rating data
#####################################################################################
#1.TrainData.csv
data<-read.csv("TrainData.csv",header=T,sep=",")
dim(data)
str(data)
head(data)
summary(data)
sum(is.na(data))
table(data$Rating)
length(unique(data$ApplicantID))
length(unique(data$JobID))
sum(table(data$Rating))

#####################################################################################
#Pre-Process Ratings Data
#####################################################################################
#Convert the trainng data into matrix format, where the row ids are Applicant ID's and the
#column names are Job ID's. Each cell in matrix is rating value given for a job(s) by the applicant.
#The jobs which have not been rated will be filled with "NA". We have to build a recommendation
#engine based on this matrix to fill NA's which are predicted rating for jobs for each applicant

Applicant.Job.Matrix<-acast(data,ApplicantID~JobID,fill=NULL)
Applicant.Job.Matrix[1:10,1:25]

length(manyNAs(Applicant.Job.Matrix,0.999))
Rating.Matrix <- as(Applicant.Job.Matrix, "realRatingMatrix")
Rating.Matrix
summary(Rating.Matrix)

sparsity<-(100 - (nratings(Rating.Matrix)/(nrow(Rating.Matrix)*ncol(Rating.Matrix)) * 100))
cat("Rating Matrix Sparsity:",sparsity)

#Understand the rating matrix
user.counts<-colCounts(Rating.Matrix)
table(user.counts)
sum(table(user.counts))
#hist(col.counts,labels=T,main="Job Applied Frequency",
#     xlab="Number of Users Applied for Each Job")
qplot(user.counts)+stat_bin(binwidth=1)+ggtitle("Distribution of Applicants")

job.counts<-rowCounts(Rating.Matrix)
table(job.counts)
sum(table(job.counts))
#hist(row.counts,labels=T,main="Histogram of Number of jobs rated by Applicant",
#     xlab="Number of Jobs Applied by Each User")
qplot(job.counts)+stat_bin(binwidth=1)+ggtitle("Distribution of Jobs")



# Handling Sparsity
#There are many jobs which are applied by few users only. As many as 5442 jobs applied
#only 1 user. We can avoid these to decrease computation time. The minimum number of users
#that apply for job will be considered as 2

Rating.Matrix.Processed<-Rating.Matrix[,colCounts(Rating.Matrix) > 1]
Rating.Matrix.Processed
sum(rowCounts(Rating.Matrix.Processed) == 0)
Rating.Matrix.Processed<-Rating.Matrix.Processed[rowCounts(Rating.Matrix.Processed) > 1,]
Rating.Matrix.Processed
#Now Rating matrix has come down to 30X91 with 107 ratings, which turns to be 99.96 sparse


#Visulaing the rating matrix
image(Rating.Matrix.Processed[1:100,1:100], main = "Raw Ratings")
Rating.Matrix.Norm<-normalize(Rating.Matrix.Processed)
image(Rating.Matrix.Norm[1:100,1:100], main = "Normalized Ratings")
qplot(getRatings(Rating.Matrix.Norm),col="red")+
  stat_bin(binwidth=1)+ggtitle("Distribution of Ratings")

#####################################################################################
#Building Models
#####################################################################################
recommenderRegistry$get_entries(dataType = "realRatingMatrix")
#Split the data into test and train
set.seed(123)
index<-sample(1:nrow(Rating.Matrix.Processed),0.7*nrow(Rating.Matrix.Processed))
train<-Rating.Matrix.Processed[index,]
test<-Rating.Matrix.Processed[-index,]

rec.svd.model<-Recommender(data=train,method="SVD")

#Similarity Matrix for each model
svd.ratings<-as(rec.svd.model@model$dat@data,"matrix")
dim(svd.ratings)

#Now we predict the ratings using test data

#SVD Predictions
pred.ratings.svd<-predict(rec.svd.model,newdata=test)
str(pred.ratings.svd)
head(pred.ratings.svd@itemLabels)
recomm_matrix.svd<-sapply(pred.ratings.svd@items,
                           function(x){
                             recommended<-x
                             c(recommended,rep("",num.recomm - length(recommended)))
                           })
dim(recomm_matrix.svd)
recomm_matrix.svd[1:6,1:16]

#####################################################################################
#Now Build the recommender models using Evaluation Scheme and Evaluating Models
#####################################################################################
#Evaluation scheme - which does split the data in train/test and then evaluates the model
recommenderRegistry$get_entry("SVD")
eval.split <- evaluationScheme(Rating.Matrix.Norm, method="split", train=0.8,
                               given=1, goodRating=1)

eval.kfold <- evaluationScheme(Rating.Matrix.Norm, method="cross-validation", 
                               given=1, goodRating=1,k=10)

# Next, we create a recommender which generates recommendations based on various method
# recommendation based on evaluation schema 1
Recommend.SVD1 <- Recommender(getData(eval.split, "train"), "SVD",
                              parameter=list(categories=100))
Recommend.SVD1

# recommendation based on evaluation schema 2
Recommend.SVD2 <- Recommender(getData(eval.kfold, "train"), "SVD",
                              parameter=list(categories=100))
Recommend.SVD2

#List of Algorithms
algorithms<-list(
                SVD1 = list(name="SVD",param=list(alpha=0.1,categories=7)),
                SVD2 = list(name="SVD",param=list(alpha=0.2,categories=7)),
                SVD3 = list(name="SVD",param=list(alpha=0.3,categories=7)),
                SVD4 = list(name="SVD",param=list(alpha=0.4,categories=7)),
                SVD5 = list(name="SVD",param=list(alpha=0.5,categories=7)),
                SVD6 = list(name="SVD",param=list(alpha=0.6,categories=7)),
                SVD7 = list(name="SVD",param=list(alpha=0.7,categories=7)),
                SVD8 = list(name="SVD",param=list(alpha=0.8,categories=7)),
                SVD9 = list(name="SVD",param=list(alpha=0.9,categories=7))
                )
# Next, we compute predicted ratings for the known part

pred.SVD1 <- predict(Recommend.SVD1, getData(eval.split, "known"),
                      type="ratings")


pred.SVD2 <- predict(Recommend.SVD2, getData(eval.kfold, "known"),
                     type="ratings")

# Finally, we can calculate the error between the prediction and the unknown part of the test
# data.
#test.ratings.unknown<-as(getData(e, "unknown"),"data.frame")
error <- rbind(
  calcPredictionAccuracy(pred.SVD1, getData(eval.split, "unknown"),goodRating=0,given=1),
  calcPredictionAccuracy(pred.SVD2, getData(eval.kfold, "unknown"),goodRating=0,given=1)
)
rownames(error) <- c("SVD1","SVD2")
error

eval1<-evaluate(eval.split,algorithms,type="ratings")
eval2<-evaluate(eval.kfold,algorithms,type="ratings")

plot(eval1)
plot(eval2)


avg(eval1)
avg(eval2)
#########################################################################################
# We use SVD to predict ratings for entire dataset using kfold
#Enhance the rated jobs by prximity - used Latitude and Longitude
#We use the predictions from SVD based on kfold as base and we enhance further
#########################################################################################
index<-sample(1:nrow(Rating.Matrix.Norm),0.7*nrow(Rating.Matrix.Norm))
train<-Rating.Matrix.Norm[index,]
test<-Rating.Matrix.Norm[-index,]
Model.SVD<-Recommender(data=train,method="SVD",
                       parameter=list(categories=300,alpha=0.9))

#SVD Predictions - recommend 20 items
num.recomm<-20
svd.pred<-predict(Model.SVD,newdata=test,n=num.recomm)
str(svd.pred)
jobs.ord<-data.frame(Job.ID=as.integer(svd.pred@itemLabels))
jobs<-merge(jobs.ord,job_data,by="Job.ID",sort=F)
jobs<-subset(jobs,select=c(Job.ID,Position,
                           Latitude,Longitude,
                           Education.Required,Industry))
summary(jobs)
recomm_matrix.svd<-sapply(svd.pred@items,
                          function(x){
                            if (identical(x,seq(1:20))) {
                              x<-rep(" ",length(x))
                            }
                            recommended<-x
                            c(recommended,rep("",num.recomm - length(recommended)))
                          })
dim(recomm_matrix.svd)
recomm_matrix.svd.enh<-t(recomm_matrix.svd)
dim(recomm_matrix.svd.enh)
rownames(recomm_matrix.svd.enh)<-rownames(as(test,"matrix"))
recomm_matrix.svd.enh[1:6,]

#Now loop through all the recommended ratings above and enhance the recommendation
#using the profile factors. Enhancing just re-orders the recommended ratings and picks 
#top 10

# Proximity Calculation Using Latitude/Longitude
#distance in kilometers between two long/lat positions
earth.dist <- function (lat1,long1,lat2, long2) 
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

b<-data.frame()
job.sort<-data.frame()
for (i in (1:nrow(recomm_matrix.svd.enh))){
  App.id<-as.integer(rownames(recomm_matrix.svd.enh)[i])
  x<-data.frame()
  y<-data.frame()
  for (j in (1:ncol(recomm_matrix.svd.enh))){
    job.id<-recomm_matrix.svd.enh[i,j]
    
    if (App.id %in% app_main_data$Applicant.ID){
      App.lat<-app_main_data[app_main_data$Applicant.ID == App.id,c("Latitude")]
      App.long<-app_main_data[app_main_data$Applicant.ID == App.id,c("Longitude")]
    }
    if (job.id %in% jobs$Job.ID){
      job.lat<-jobs[job_data$Job.ID == job.id,c("Latitude")]
      job.long<-jobs[job_data$Job.ID == job.id,c("Longitude")]
    }
    if (App.id %in% app_main_data$Applicant.ID & job.id %in% jobs$Job.ID){
      dist<-earth.dist(App.lat,App.long,job.lat,job.long)
    } else {
      dist=999999
    }
    x<-data.frame(job.id=job.id,dist=dist)
    y<-rbind(y,x)
    b<-rbind(b,y)
  }
  y<-y[order(y$dist),]
  z<-as.integer(as.character(y$job.id))
  job.sort<-rbind(job.sort,z)
}

#job.sort<-job.sort[order(job.sort$app.id,job.sort$dist),]
#  jobs.enh<-job.sort$job.id
#  result<-data.frame(App.id,job.sort)
dim(job.sort)
head(job.sort)
rownames(job.sort)<-rownames(recomm_matrix.svd.enh)
colnames(job.sort)<-c(1:20)

#Check if recommended jobs have been applied by applicant
filtered_out<-data.frame()
job.top10<-data.frame()

for (i in (1:nrow(job.sort))){
  App.id<-as.integer(rownames(job.sort)[i])
  filter_in<-data.frame()
  for (j in (1:ncol(job.sort))){
    job.id<-job.sort[i,j]
    applied.list<-jobs_applied[jobs_applied$Applicant.ID==App.id,c("Job.ID")]
    if (job.id %in% applied.list){
      filtered_out<-rbind(filered_out,job.id)
      } else {
        x<-data.frame(job.id=job.id)
        filter_in<-rbind(filter_in,x)
      }
    }
  z<-as.integer(as.character(filter_in$job.id))
  job.top10<-rbind(job.top10,z[1:10])
}
rownames(job.top10)<-rownames(recomm_matrix.svd.enh)
colnames(job.top10)<-c(1:10)
test[1:6,]
recomm_matrix.svd.enh[1:6,1:20]
job.top10[1:6,]


#########################################################################
#Compare the Job's Position with Applicant's positions of interest
#Compare the job's education requirement with Applicants education qualification
#########################################################################




