#install.packages("devtools")
library(AnomalyDetection)
#devtools::install_github("twitter/AnomalyDetection")
#install.packages("Rcpp")
library(Rcpp)
# Detecting Anomaly through Function AnomalyDetectionTs()
# As the function AnomalyDetctionTs need a Time series as
# a two column data frame where the first column consists of
# the timestamps and the second column consists of the observations. 

#creating timestamp column

timeStamp<-as.POSIXct("2018-02-26 12:00:00 CST")
data_TimeStamp<-timeStamp
for(i in c(1:3600)){
data_TimeStamp[i]<-c(timeStamp<-timeStamp+60)
}
head(data_TimeStamp)

#Creating 2nd Column for observation  
count<-rnorm(3600)
head(count)
#Building Data Frame 
df<-data.frame(data_TimeStamp,count)
# Applyin the function for anomaly detection
dv<-AnomalyDetectionTs(df,max_anoms = 0.4,direction = 'both',alpha =2,only_last = 'hr',xlabel = "Time Series Granularity of 1 Minute",  plot = TRUE)
dv$plot
# By giving more disperse Data with mean vlaue 200, stander Deviation =105

count<-rnorm(3600, mean = 200, sd= 105)
head(count)
#Building Data Frame 
df<-data.frame(data_TimeStamp,count)
# Applyin the function for anomaly detection
dv<-AnomalyDetectionTs(df,max_anoms = 0.4,direction = 'both',alpha =5,only_last = 'hr',xlabel = "Time Series Granularity of 1 Minute",  plot = TRUE)
dv$plot


##########################################################################
# Anomaly Detection by function AnomalyDetectionVec(). which need
#Time series as a column data frame, list, or vector, where the column consists
#of the observations.
###########################################################################

# creating a vector of obserrvation for AnomalyDetectionVec function
# lets creat a uiform distribution value vector

v<-runif(2000)
dev<-AnomalyDetectionVec(v, max_anoms = 0.4,alpha = 10,direction = 'both', period = 5,plot = TRUE)
dev$plot
# The function AnomalyDetectionVec did not detectec any anomaly as the values are uniform

#  Lets make vector of normal distribution value with mean 50 and SD 15
v1<-rnorm(2000,mean=50, sd=10)
dev<-AnomalyDetectionVec(v1, max_anoms = 0.4,alpha = 10,direction = 'both', period = 5,plot = TRUE)
dev$plot
# Function has successfully detected the anomaly in the vector.


