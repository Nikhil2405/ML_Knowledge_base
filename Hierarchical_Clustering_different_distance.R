##### We Can try many different distance matrix and linkage method with this simple function to understand how cluster vary.
#### method1 specify distance matrix name
#### method2 specify linkage method
########## Different Distance ######
#Binary
#Eculidean
#Maximum
#Manhattan
#Canberra
#Minkowski
#Pearson
#Spearman
#Kendall

##### Linakge methods ####
#single
#ward.D2
#complete
#average
#mcquitty
#median
#centroid
###################### with default dist function #################
hirearchical_clustering <- function(orig_data,method1,method2,K)
{
  x <- dist(orig_data,method = method1)
  fit <- hclust(x,method = method2 )
  groups <- cutree(fit,k=K)
  clust_center<- aggregate(orig_data,list(groups),mean)
  print(clust_center)
  table(groups)
}
hirearchical_clustering(orig_data,"maximum","ward.D2",2)
####################################################################
############# get_distance fucntion ######################
library(factoextra)
hirearchical_clustering_fact <- function(orig_data,method1,method2,K)
{
  x <- get_dist(orig_data,method = method1)
  fit <- hclust(x,method = method2 )
  groups <- cutree(fit,k=K)
  clust_center<- aggregate(orig_data,list(groups),mean)
  print(clust_center)
  table(groups)
}
hirearchical_clustering_fact(orig_data,"kendall","average",2)

#################### Gower Distance ######

library(cluster)

hirearchical_clustering_daisy <- function(orig_data,method1,method2,K)
{
  
  x <- daisy(orig_data,metric = method1)
  fit <- hclust(x,method = method2 )
  groups <- cutree(fit,k=K)
  clust_center<- aggregate(orig_data,list(groups),mean)
  print(clust_center)
  table(groups)
}
hirearchical_clustering_daisy(orig_data,"gower","mcquitty",2)

##### Divisive Hirearchical Clustering #######

hirearchical_clustering_divise <- function(orig_data,method2,K)
{
  
  x <- get_dist(orig_data,method  = method2)
  fit <- diana(x)
  groups <- cutree(fit,k=K)
  clust_center<- aggregate(orig_data,list(groups),mean)
  print(clust_center)
  table(groups)
}
hirearchical_clustering_divise(orig_data,"euclidean",2)
