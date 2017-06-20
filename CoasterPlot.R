coaster <- read.csv("C:/dev/RollerCoaster/coaster.tsv", header=T, sep='\t')
attach(coaster)
setwd('C:/dev/RollerCoaster/')

x<-rep(1:10, each=10)
y<-rep(1:10, 10)
z <- cbind(x,y)

f <- function(x){
  air <- mean(subset(coaster, airtime_rating == x[1])$overall)
  intense <- mean(subset(coaster, intensity_rating == x[2])$overall)
  ret <- if (is.na((air + intense)/2)) 0 else (air + intense)/2 
  return(ret)
}

xx <- apply(z, 1, FUN=f)

m <- matrix(xx, nrow=10)

plot.new()
jpeg("IntensityByAirtime.jpg")
plot(airtime_rating,intensity_rating,col=c("red","green","blue","orange","purple"),
     type="o", main="Henry's Intensity Rating by Air Time Rating",
     sub='as overplot',
     xlab="Henry Air Time Rating", ylab="Henry's Intensity Rating")
dev.off()

pdf("CoasterGraph.pdf")
x10 <- seq(1:10)
y10 <- x10
persp(x10,y10,m, theta=30, phi=40,xlab="Henry's Air Time Rating", 
      ylab="Henry's Intensity Rating",zlab="Henry's Overall Rating",
      col=c("orange","red","yellow","green","blue"), axes=T, box=T, 
      ticktype='detailed',nticks=4, main="Henry's Amazing Technicolor Dream Graph")
dev.off()


pdf("Contour.pdf")
contour(x10,y10,m,col=c("blue","green"))
dev.off()

pdf("HeatMap.pdf")
image(x10,y10,m,xlab="Henry's Air Time Rating", 
      ylab="Henry's Intensity Rating",
      col=c("yellow","green","blue","orange","red"))
dev.off()

summary(coaster)

pdf("Plot.pdf")
plot(type,overall,col=c("red","green","blue","orange","purple"), type="p")
dev.off()

head(coaster)
library(dplyr)
coast_nums <- select(coaster, height,inversions,length,speed,airtime_rating,
                     intensity_rating,smoothness_rating,theming_rating, overall)
wss <- (nrow(coast_nums)-1)*sum(apply(coast_nums,2,var))
for (i in 1:13) wss[i] <- sum(kmeans(coast_nums, 
                                     centers=i)$withinss)
plot(1:13, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")



# CLUSTERING #####

# Ward Hierarchical Clustering
d <- dist(coast_nums, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit) # display dendogram
groups <- cutree(fit, k=4) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=4, border="red")


# Ward Hierarchical Clustering with Bootstrapped p values
#install.packages("pvclust")
library(pvclust)
fit <- pvclust(coast_nums, method.hclust="ward",
               method.dist="euclidean")
plot(fit) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit, alpha=.95)

# Model Based Clustering
#install.packages("mclust")
library(mclust)
fit <- Mclust(coast_nums)
plot(fit) # plot results 
summary(fit) # display the best model


# K-Means Clustering with 5 clusters
fit <- kmeans(coast_nums, 4)
coast_nums$cluster <- fit$cluster
coast_nums$in.cluster.1 = as.numeric(coast_nums$cluster == 1)
coast_nums$in.cluster.2 = as.numeric(coast_nums$cluster == 2)
coast_nums$in.cluster.3 = as.numeric(coast_nums$cluster == 3)
# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
#install.packages("cluster")
library(cluster) 
clusplot(coast_nums, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
#install.packages("fpc")
library(fpc)
plotcluster(coast_nums, fit$cluster)

# Validating Cluster Solutions
library(fpc)
cluster.stats(d, fit1$cluster, fit2$cluster)

names(coaster)
coaster.g <- group_by(coaster, park)
summarise(coaster.g, height.mn = mean(height), height.md=median(height),
          inversions.mn = mean(inversions), inversion.md=median(inversions))
summarise(coaster.g, airtime.mn = mean(airtime_rating), airtime.md=median(airtime_rating),
          overall.mn = mean(overall), inversion.md=median(overall))

# LINEAR REGRESSION
names(coast_nums)
lmfit = lm( overall ~ ., coast_nums )
lmfit.smooth = lm(overall ~ smoothness_rating + in.cluster.1 + in.cluster.2 +
                    in.cluster.3, coast_nums)
lmfit.smooth
summary(lmfit.smooth, corretlation=T)
cor(coast_nums)
pairs(coast_nums)
library(lattice)
splom(coast_nums)

anova(lmfit.smooth)
plot(lmfit.smooth)
fitted(lmfit.smooth)
coef(lmfit.smooth)
residuals(lmfit.smooth)
names(lmfit)
lmfit$df.residual

hist(coast_nums$smoothness_rating, breaks = 100)


# convert duration to decimal
# STUB
minPerGame <- c("4:30","2:20","34:10")

sapply(strsplit(minPerGame,":"),
       function(x) {
         x <- as.numeric(x)
         x[1]+x[2]/60
       }
)