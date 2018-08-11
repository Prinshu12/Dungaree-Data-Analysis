install.packages("readxl")
install.packages("graphics")
install.packages("Gmedian")

library(Hmisc) #Contents and Describe
library(leaps) #Variable selection
library(MASS)
library(NbClust)
library(Gmedian)



setwd("C:/Users/hitpr/Desktop/MSBA/1st semester/Business Analytics/Homework")

getwd()

dungaree_data <- read.csv("dungaree.csv", header=TRUE)

dungaree_data

set.seed(42)

b<- boxplot(dungaree_data$FASHION)
b$out
FASHION_OUT<-dungaree_data[dungaree_data$FASHION %in% b$out,]
FASHION_OUT

b<- boxplot(dungaree_data$LEISURE)
b$out
LEISURE_OUT<-dungaree_data[dungaree_data$LEISURE %in% b$out,]
LEISURE_OUT

b<- boxplot(dungaree_data$STRETCH)
b$out
STRETCH_OUT<-dungaree_data[dungaree_data$STRETCH %in% b$out,]
STRETCH_OUT

b<- boxplot(dungaree_data$ORIGINAL)
b$out
ORIGINAL_OUT<-dungaree_data[dungaree_data$ORIGINAL %in% b$out,]
ORIGINAL_OUT

#Collect outliers for each variable in a single dataframe
Outliers<-rbind(ORIGINAL_OUT,STRETCH_OUT,LEISURE_OUT,FASHION_OUT)

#Substract Outliers from the dataframe dungaree_data
dungaree_data<-dungaree_data[ !(dungaree_data$STOREID %in% Outliers$STOREID), ]
dim(dungaree_data)

hist(dungaree_data$ORIGINAL)
hist(dungaree_data$STRETCH)
hist(dungaree_data$LEISURE)
hist(dungaree_data$FASHION)

row.names(dungaree_data) <- dungaree_data[,1]
dungaree_data <- dungaree_data[, -c(1,6)]
dungaree_data.norm <- sapply(dungaree_data, scale)
dungaree_data.norm

devAskNewPage(ask=TRUE)

nc <- NbClust(dungaree_data.norm, min.nc=2, 
              max.nc=10, method="kmeans")

table(nc$Best.n[1,])

barplot(table(nc$Best.n[1,]), xlab="Number of Clusters", ylab="Number of criteria", main="Number of clusters chosen by criteria")

# Perform k-means cluster analysis
fit.km <- kmeans(dungaree_data.norm, 10, nstart=10)
fit.km

fit.km$size
fit.km$withinss
fit.km$centers


# calcualte cluster centroids using fit.km$centers
fit.km$centers

wssplot <- function(dungaree_data.norm, nc=10, seed=42) {
  wss <- (nrow(dungaree_data.norm)-1)*sum(apply(dungaree_data.norm, 2, var)) 
  for (i in 2:nc) {
    set.seed(42) 
    wss[i] <- sum(kmeans(dungaree_data.norm, centers=i)$withinss)
  } 
  plot(1:nc, wss, type="b", xlab="Number of clusters", ylab="Within groups sum of squares")
}
wssplot(dungaree_data.norm,nc=10)

# Perform k-means cluster analysis
fit.km <- kmeans(dungaree_data.norm, 6, nstart=10)
summary(fit.km)
fit.km$size
fit.km$centers

#Find the sum of squared distances within a cluster
fit.km$withinss

# calcualte cluster centroidsfit.km$centers
fit.km$centers

wssplot(dungaree_data.norm,nc=6)
#check if any cluster is astriking outlier
dist(fit.km$centers)
#Find the sum of squared distances with the entire dataset as a single cluster
fit.km1 <- kmeans(dungaree_data.norm, 1, nstart=10)
fit.km1$withinss

  #Evaluate the cluster validity. Find the ratio of sum of squared 
  #distances within clusters where k=6 and sum of squared distances
  #within cluster where k=1. The ratio should be closer to 0 and farther from 1.
  
  a<-sum(fit.km$withinss)/fit.km1$withinss
  a

#Profile plot of Centroids

# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l", ylim = c(min(fit.km$centers), max(fit.km$centers)), xlim = c(0, 8))
# label x-axes
axis(1, at = c(1:4), labels = names(dungaree_data))
# plot centroids

for (i in c(1:6))
  lines(fit.km$centers[i,], lty = i, lwd = 2, 
        col = ifelse(i %in% c(1, 3),"black",
                     (ifelse(i %in% c(5, 6),"blue",
                             "green"))))

# name clusters
text(x = 0.5, y = fit.km$centers[, 1], labels = paste("Cluster", c(1:6))) 

