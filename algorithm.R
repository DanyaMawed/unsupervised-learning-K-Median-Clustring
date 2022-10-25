library('tidyverse')

set.seed(123)

kmedians<-function(X,K){
  if (!(is.data.frame(X)) | (K %% 1 != 0 ) ){
    print('x must be a data frame, k must be an integer')
  }
  else if (K==1){ #if we want to have just one cluster 
    final<- X
    final$Cluster<-1
  }
  else{
    #Randomaly assign each observition to a cluster
    Cluster<-sample(1:K,nrow(X), replace = TRUE)
    clustring_df<-X %>% mutate(Cluster)
    #return(Cluster) for test
    i<-1
    Median<-list()
    while (i <= K) {
      #caluclate the median of each cluster
      #the first cluster
      clust<-clustring_df%>% filter(Cluster==i)
      x_median_clust<-median(clust[[1]])
      y_median_clust<-median(clust[[2]])
      Median[[i]]<-c(x_median_clust,y_median_clust)
      i<-i+1
      }# while brackets
    #print(Median)
    Median<-as.data.frame(Median)
    Median<-t(Median)
    row.names(Median)<-1:nrow(Median)
    #print(Median)
    
    
    #A function for Euclidean distance
    
    l2_dist <- function(x,y){
      sq_dif<-(x-y)^2
      euc<-sum(sq_dif)
      euc<-sqrt(euc)
      return(euc)
    }
    
    final<-clustring_df # can't modify the original df since it is used in the loop, to save the final clustering
    #calculate the distance between each observation and the median of each cluster
    j<-1
    while (j<=K) {
      clust_values<-clustring_df %>% filter(Cluster==j)
      distance<-matrix(nrow = nrow(clust_values),ncol = K)
      for (d in 1:K){ 
        distance[,d]<-apply(clust_values[,-3],1,l2_dist,y=Median[d,])
      }
      #print(distance)
      distance<-as.data.frame(distance)
      #compare the distance
      for (col in 1:K){
        if (col<K){
         # first column with all column if False 0 then the first cluster
          less_than <- as.matrix(distance[,col]<distance[,(col+1):K])
          Rsum<-rowSums(less_than)
          distance[,K+col]<-Rsum < (K-col)
          }
        }
      #print(distance)
      distance$Cluster<-NA
      for (num in 1:K){
        if (num < K){
           for (obs in 1:nrow(distance)){
              if (distance[obs,K+num]=='FALSE' & is.na(distance[obs,'Cluster']) ){
                distance[obs,'Cluster']<-num  #if False 0 then the same cluster
              }
           }
        }
        else if (num==K ){
          for (obs in 1:nrow(distance)){
            if (distance[obs,(K+num-1)]=='TRUE'& is.na(distance[obs,'Cluster'])){
                distance[obs,'Cluster']<-K      #if TRUE then the Last cluster number
            }
          }
        }
      }
      #print(distance)
      #print('end loop ')
      final[final[[1]] %in% clust_values[[1]],'Cluster']<-distance$Cluster
      j=j+1}
  }# else brackets
  #print(final)
  #final%>% ggplot(mapping = aes(x=final[[1]],y=final[[2]],col=as.character(Cluster)))+
    #geom_point()
  return(final)
  
}# the function brackets




