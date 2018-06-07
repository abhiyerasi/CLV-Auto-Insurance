rm(list = ls(all = T))

cereals <- read.csv('forall.csv', header = T, sep = ",")

str(cereals)
summary(cereals)

sum(is.na(cereals))

#library(dummies)
#cat=cereals[sapply(cereals,is.factor)]
#cat=dummy.data.frame(cat)
#target=cereals$Customer.Lifetime.Value
#num=cereals[sapply(cereals,is.numeric)]
#num$Customer.Lifetime.Value=NULL
#cereals=data.frame(cat,num,target)
#Hierrarchial Clustering
dist <- dist(cereals, method = "euclidean")
fit <- hclust(dist, method = "ward.D")
#plot(fit)
#fit$merge
#fit$height

groups <- cutree(fit, k = 4)
#groups

#rect.hclust(fit, k = 6, border = "brown")
DataWithHistCluster <- data.frame(cereals, groups)

ggplot(cereals, aes(x=Coverage, y=Monthly.Premium.Auto,color="rainbow")) + stat_summary(fun.y="mean", geom="bar")+geom_jitter()+ggtitle("Mean Premium Value for Coverages ")

ggplot(cereals, aes(x=Coverage, y=Customer.Lifetime.Value,color="rainbow")) + stat_summary(fun.y="mean", geom="bar")+geom_jitter()+ggtitle("Mean Customer Life Time Value for Coverages ")

ggplot(cereals, aes(x=Gender, y=Monthly.Premium.Auto,color="rainbow")) + stat_summary(fun.y="mean", geom="bar")+geom_jitter()+ggtitle("Mean Premium Value for Coverages based on genders ")+facet_wrap(~Coverage)

ggplot(cereals, aes(x=Gender,color="rainbow"))+geom_bar()+facet_wrap(~Coverage)

ggplot(cereals, aes(x=Marital.Status,color="rainbow"))+geom_bar()+facet_wrap(~Coverage)+
  ggtitle("Coverages Bought based on Marital Status")

ggplot(cereals, aes(x=Vehicle.Class, y=Customer.Lifetime.Value,color="rainbow")) + stat_summary(fun.y="mean", geom="bar")+geom_jitter()+ggtitle("Customer Life Time Value Based on Coverage and Vehicle Class ")+facet_wrap(~Coverage)+theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(cereals, aes(x=Vehicle.Class, y=Total.Claim.Amount,color="rainbow")) + stat_summary(fun.y="mean", geom="bar")+geom_jitter()+ggtitle("Claim Amount Based on Coverage and Vehicle Class ")+facet_wrap(~Coverage)+theme(axis.text.x = element_text(angle = 90, hjust = 1))


############## Pradeep Anna
ggplot(cereals, aes(x=Gender, y=Customer.Lifetime.Value,color="rainbow")) + stat_summary(fun.y="mean", geom="bar")+geom_jitter()+ggtitle("Gender, Coverages and CLTV")+facet_wrap(~Coverage)
ggplot(cereals, aes(x=Renew.Offer.Type, y=Customer.Lifetime.Value,color="rainbow")) + stat_summary(fun.y="mean", geom="bar")+geom_jitter()+ggtitle("Offer, Coverages and CLTV")+facet_wrap(~Coverage)
ggplot(cereals, aes(x=EmploymentStatus, y=Customer.Lifetime.Value,color="rainbow")) + stat_summary(fun.y="mean", geom="bar")+geom_jitter()+ggtitle("Employement status, Coverages and CLTV")+facet_wrap(~Coverage)+theme(axis.text.x = element_text(angle = 90, hjust = 1))



group4=DataWithHistCluster[DataWithHistCluster$groups==4,]
library(ggplot2)
ggplot(group4,aes(Coverage,Customer.Lifetime.Value,col="rainbow"))+stat_summary(fun.y="mean", geom="bar")+geom_jitter()+
  ggtitle("Customer Life Time Value Distribution Of Group4")


group3=DataWithHistCluster[DataWithHistCluster$groups==3,]
ggplot(group3,aes(Coverage,Customer.Lifetime.Value,col="rainbow"))+stat_summary(fun.y="mean", geom="bar")+geom_jitter()+
  ggtitle("Customer Life Time Value Distribution Of Group3")


group2=DataWithHistCluster[DataWithHistCluster$groups==2,]
ggplot(group2,aes(Coverage,Customer.Lifetime.Value,col="rainbow"))+stat_summary(fun.y="mean", geom="bar")+geom_jitter()+
  ggtitle("Customer Life Time Value Distribution Of Group2")

group1=DataWithHistCluster[DataWithHistCluster$groups==1,]
ggplot(group1,aes(Coverage,Customer.Lifetime.Value,col="rainbow"))+stat_summary(fun.y="mean", geom="bar")+geom_jitter()+
  ggtitle("Customer Life Time Value Distribution Of Group1")

#K Means Clustering

KMeanClusters <- kmeans(cereals, centers = 4, iter.max = 10)
KMeanClusters
KMeanClusters$withinss
KMeanClusters$betweenss

library(cluster)
clusplot(cereals, KMeanClusters$cluster, main = '2D representation of the Cluster solution',
         color = TRUE, shade = TRUE,
         labels = 2, lines = 0)

Scaled <- data.frame(cereals,KMeanClusters$cluster)


wss <- 0
for (i in 1:15) {
    wss[i] <- sum(kmeans(Scaled, centers = i)$withinss)
}

plot(1:15, wss,
     type = "b",
     xlab = "Number of Clusters",
     ylab = "Within groups sum of squares")

testData <- Scaled[sample(1:nrow(Scaled), 1),]
closest.cluster <- function(x) {
    cluster.dist <- apply(KMeanClusters$centers, 1, function(y) sqrt(sum((x - y) ^ 2)))
    print(cluster.dist)
    return(which.min(cluster.dist)[1])
}

closest.cluster(testData)

set.seed(12)
index <- (sample(nrow(Scaled), .90 * nrow(Scaled)))
dataTest <- Scaled[index,]
StabClus <- kmeans(dataTest, 5)
dataTest$clusters <- StabClus$cluster

ncol(Scaled)
group1 <- Scaled[index, 21]
group2 <- dataTest$clusters
group <- cbind(group1, group2)
write.csv(group, "clusgroup.csv")

#install.packages("fossil")
library(fossil)
stabilitycheck <- adj.rand.index(group1, group2)
stabilitycheck

#install.packages("clusteval")
library(clusteval)
Stabindex <- cluster_similarity(group1, group2, similarity = "jaccard", method = "independence")
Stabindex


