library(cluster)
library(factoextra)
library(NbClust)
library(psych)
library(smacof)
library(reshape)
library(flexclust)
library(png)
library(magick)
library(VIM)

#Description
#With this analysis I am trying to compare behaviour of SP500 sectors during the COVID-19 pandemic.
#The 1st period include 1 year behore WHO's officials report pandemic
#The second period covers time between the first period and first news about succesfull vaccine tests
#And the last period covers period betwenn 2nd and the end of December, 2020. 

#Loading data
data1 <- read.csv2("D:\\UW\\1st semester\\Microeconomics\\Assignment\\data\\data SP500 sec rates perc.csv", header = TRUE, sep = ",", dec = ".")
str(data1)

#Indexing date period
data1$Effective.date <- as.Date(data1$Effective.date, "%d/%m/%Y")
Seq <- seq.Date(as.Date("2019-03-11"),as.Date("2020-03-10"),by="day")
data_2019<- data1[data1$Effective.date %in% Seq,][,2:12]
data_2019_t <- as.data.frame(t(data_2019))

#NA checking
apply(data_2019, 2, function(x) any(is.na(x))) 
aggr(data_2019, combined = TRUE, numbers = TRUE)

#MDS
mds1<-cmdscale(dist(data_2019_t), k=2)
plot(mds1) # plot with labels
text(mds1, labels=rownames(data_2019_t), cex=0.6, adj=0.5)

#Quality evaluation
#Stress function is 0.11 - clustering quality is fair
mds0 <- mds(delta = dist(data_2019), ndim = 2, type = "ratio")
mds0

#An MDS solution may have high Stress simply as a consequence of high error in the data, and finding a precise representation for the data does not imply anything about its scientific value. (Borg and Groenen, 2005, p48 )
#The authors have a suggestion, normalized stress.

#Normalized stress valuse is .012 so goodness-of-fit is quite good
dhat_matrix = as.matrix(mds0$dhat)
d_matrix = as.matrix(mds0$confdist)
denominator = sum(dhat_matrix[upper.tri(dhat_matrix)]^2)
p_ij = dhat_matrix[upper.tri(dhat_matrix)]
d_ij = d_matrix[upper.tri(d_matrix)]
nominator = sum((p_ij - d_ij)^2) 
normalized_stress = nominator/denominator
normalized_stress


#clusterability checking
#Hopkins statistics <0.5 so you dataset is clusterable
mds1 <- as.data.frame(mds1)
res <- get_clust_tendency(mds1, 10, graph = TRUE)
1 - res$hopkins_stat
print(res$plot)

#number of cluster
fviz_nbclust(mds1, kmeans, method="wss")+theme_classic() #elbow
fviz_nbclust(mds1, FUNcluster=kmeans, method="silhouette")+theme_classic() #silhouette
fviz_gap_stat(clusGap(mds1, FUN = kmeans, nstart = 25, 
                      K.max = 10, B = 100) ) #gap stat
nbclust_out <- NbClust(
  data = mds1,
  distance = "euclidean",
  min.nc = 2, # minimum number of clusters
  max.nc = 9, # maximum number of clusters
  method = "kmean")

#The most frequent answer for the number of clusters is 3
#Barplot with answers on number of clusters (for some reasons it isn't displayed in knitr HTML and breaks the view, so I commented it out)
barplot(table(nbclust_out$Best.nc["Number_clusters",]))

#Kmeans clustering
set.seed(123)
km1 <- eclust(mds1, "kmeans", 3, graph=TRUE, hc_metric = "euclidean", nboot=50)

#checking clusters measurements 
km1$cluster
km1$centers
km1$size
km1$tot.withinss

#Silhouette value is 0.53, so clustering quality can we assumed as acceptable
fviz_silhouette(silhouette(km1$cluster, dist(mds1)))

#Shadow statistics for the third cluster is high, but it can be caused by low number of points in this cluster.
sh1 <- cclust(mds1,3,"manhattan")
shadow(sh1)
plot(shadow(sh1))


#Statistics by groups
data.c1 <-cbind(t(data_2019), km1$cluster)
class(data.c1)
data.c1 <- as.data.frame(data.c1)

colnames(data.c1)[length(data.c1)]<-"Cluster"
data.m1 <- melt(data.c1, id.var = "Cluster")
data.m1$Cluster <- as.character(data.m1$Cluster)
describeBy(data.m1$value, data.m1$Cluster)


#Plots
par(mfrow = c(1,1))
boxplot(data.m1$value~data.m1$Cluster, col=c("red", "green", "blue"))

violin_plot <- ggplot(data.m1, aes(x=value, y=Cluster, fill=Cluster)) + 
  geom_violin(trim=TRUE)+ 
  coord_flip()+ 
  stat_summary(fun.data="mean_sdl", mult=1, 
                             geom="crossbar", width=0.05)
violin_plot  

#PANDEMIC
###########################################################################
Seq <- seq.Date(as.Date("2020-03-11"),as.Date("2020-11-8"),by="day")
data_2020<- data1[data1$Effective.date %in% Seq,][,2:12]
str(data_2020)
data_2020_t <- as.data.frame(t(data_2020))

#MDS
mds1p<-cmdscale(dist(data_2020_t), k=2)
plot(mds1p) # plot with labels
text(mds1p, labels=rownames(data_2020_t), cex=0.6, adj=0.5)

#Quality evaluation
mds0p <- mds(delta = dist(data_2020), ndim = 2, type = "ratio")
mds0p

#Normalized stress valuse is .006 so goodness-of-fit is very good
dhat_matrix = as.matrix(mds0p$dhat)
d_matrix = as.matrix(mds0p$confdist)
denominator = sum(dhat_matrix[upper.tri(dhat_matrix)]^2)
p_ij = dhat_matrix[upper.tri(dhat_matrix)]
d_ij = d_matrix[upper.tri(d_matrix)]
nominator = sum((p_ij - d_ij)^2) 
normalized_stress = nominator/denominator
normalized_stress

#clusterability checking
#Hopkins statistics is less than 0,5, so clustering is possible
mds1p <- as.data.frame(mds1p)
res <- get_clust_tendency(mds1p, 10, graph = TRUE)
1 - res$hopkins_stat
print(res$plot)

#Kmeans clustering
set.seed(123)
km1p <- eclust(mds1p, "kmeans", 3, graph=TRUE, hc_metric = "manhattan", nboot=50)

#checking clusters measurements
km1p$cluster
km1p$centers
km1p$size
km1p$tot.withinss

#Clustering quality - silhouette value is 0.44, so clustering quality can we assumed as acceptable, because 1st and 2nd clusters have >0.5 silhouette index values, and 3rd cluster is less representative, because the number of observations is low
fviz_silhouette(silhouette(km1p$cluster, dist(mds1p)))

#Shadow statistics is safe far from 1, so the quality is acceptable.
sh2 <- cclust(mds1p,3,"manhattan")
shadow(sh2)
plot(shadow(sh2))


#Statistics
data.c1p <-cbind(t(data_2020), km1p$cluster)
class(data.c1p)
data.c1p <- as.data.frame(data.c1p)

colnames(data.c1p)[length(data.c1p)]<-"Cluster"
data.m1p <- melt(data.c1p, id.var = "Cluster")
data.m1p$Cluster <- as.character(data.m1p$Cluster)
describeBy(data.m1p$value, data.m1p$Cluster)

#Plots
par(mfrow = c(1,1))
boxplot(data.m1p$value~data.m1p$Cluster, col=c("red", "green", "blue"),range=4.5)

violin_plot <- ggplot(data.m1p, aes(x=value, y=Cluster, fill=Cluster)) + 
  geom_violin(trim=TRUE)+ 
  coord_flip()+ 
  stat_summary(fun.data="mean_sdl", mult=1, 
               geom="crossbar", width=0.05)
violin_plot  

#VACCINE
#############################################################################
Seq <- seq.Date(as.Date("2020-11-9"),as.Date("2020-12-21"),by="day")
data_2020v<- data1[data1$Effective.date %in% Seq,][,2:12]
str(data_2020v)
data_2020v_t <- as.data.frame(t(data_2020v))

#MDS
mds1v<-cmdscale(dist(data_2020v_t), k=2)
plot(mds1v) # plot with labels
text(mds1v, labels=rownames(data_2020v_t), cex=0.6, adj=0.5)

#quality evaluation
mds0v <- mds(delta = dist(data_2020v), ndim = 2, type = "ratio")
mds0v

#Normalized stress valuse is .008 so goodness-of-fit is very good
dhat_matrix = as.matrix(mds0v$dhat)
d_matrix = as.matrix(mds0v$confdist)
denominator = sum(dhat_matrix[upper.tri(dhat_matrix)]^2)
p_ij = dhat_matrix[upper.tri(dhat_matrix)]
d_ij = d_matrix[upper.tri(d_matrix)]
nominator = sum((p_ij - d_ij)^2) 
normalized_stress = nominator/denominator
normalized_stress

#clusterability checking
#Hopkins statistics is less than 0,5, so clustering is possible
mds1v <- as.data.frame(mds1v)
res <- get_clust_tendency(mds1v, 10, graph = TRUE)
1 - res$hopkins_stat
print(res$plot)

#Kmeans clustering
set.seed(123)
km1v <- eclust(mds1v, "kmeans", 3, graph=TRUE, hc_metric = "manhattan", nboot=50)


#checking clusters measurements
km1p$cluster
km1p$centers
km1p$size


#Clustering quality - silhouette value is 0.35, so clustering quality can we assumed as not good. But  1st cluster has >0.5 silhouette index values, and 3rd cluster is less representative, because the number of observations is low, while 2nd cluster consists only from single point
fviz_silhouette(silhouette(km1v$cluster, dist(mds1v)))

#Shadow statistics is safe far from 1, so the quality is acceptable.
sh3 <- cclust(mds1v,3,"manhattan")
shadow(sh3)
plot(shadow(sh3))

#Statistics by group
data.c1v <-cbind(t(data_2020v), km1p$cluster)
class(data.c1v)
data.c1v <- as.data.frame(data.c1v)

colnames(data.c1v)[length(data.c1v)]<-"Cluster"
data.m1v <- melt(data.c1v, id.var = "Cluster")
data.m1v$Cluster <- as.character(data.m1v$Cluster)
describeBy(data.m1v$value, data.m1v$Cluster)

#Plots
par(mfrow = c(1,1))
boxplot(data.m1v$value~data.m1v$Cluster, col=c("red", "green", "blue"))

violin_plot <- ggplot(data.m1v, aes(x=value, y=Cluster, fill=Cluster)) + 
  geom_violin(trim=TRUE)+ 
  coord_flip()+ 
  stat_summary(fun.data="mean_sdl", 
               geom="crossbar", width=0.05)
violin_plot 

#Clustering stability check
#Rand index shows that clusters isn't stable and a lot of points are migrating from the one cluster to another. So, changes between periods are significant. But rand index of second and third periods is quite high, which means, that between these periods difference not as high.
randIndex(km1$cluster, km1p$cluster)
randIndex(km1$cluster, km1v$cluster)
randIndex(km1p$cluster, km1v$cluster) 


#publishing
library(rmarkdown)
library(knitr)
library(readxl)

