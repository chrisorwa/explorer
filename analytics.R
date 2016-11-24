
#load required libraries
library(ape)
library(bnlearn)

#================================ Perform PCA analysis ==========================================
pca_analysis <-function(dtm)
{
  #perform PCA
  c <-prcomp(dtm)
  d <-as.data.frame(c$rotation)
  e <-d[c('PC1','PC2')]
  return(e)
}

#=============================== Perform Mantel's test =========================================
mantel <- function(dtm)
{
  #define euclidean function
  euclidean <-function(x2,y2,x1,y1){
    a <-(x2-x1)*(x2-x1)
    b <-(y2-y1)*(y2-y1)
    return(sqrt(a+b))
  }
  
  #initialize loop variables
  k = 500
  v = nrow(dtm)
  record = c()
  
  while(v-k > 0 )
  {
    c <-prcomp(dtm[1:k,])
    d <-as.data.frame(c$rotation)
    e <-d[c('PC1','PC2')]
    
    #create empty matrix
    n <-matrix(nrow = nrow(e),ncol = nrow(e))
    colnames(n) <-rownames(e)
    rownames(n) <-rownames(e)
    
    #populate matrix
    for (i in 1:nrow(e)){
      for (j in 1:nrow(e)){
        n[i,j] <-round(euclidean(e$PC2[i],e$PC1[i],e$PC2[j],e$PC1[j]),digits = 4)
      }
    }
    
    c <-prcomp(dtm[1:v,])
    d <-as.data.frame(c$rotation)
    e <-d[c('PC1','PC2')]
    
    #create empty matrix
    m <-matrix(nrow = nrow(e),ncol = nrow(e))
    colnames(m) <-rownames(e)
    rownames(m) <-rownames(e)
    
    #populate matrix
    for (i in 1:nrow(e)){
      for (j in 1:nrow(e)){
        m[i,j] <-round(euclidean(e$PC2[i],e$PC1[i],e$PC2[j],e$PC1[j]),digits = 4)
      }
    }
    
    
    #mantel's permutation test
    mantle <-mantel.test(m,n,graph = FALSE)
    
    rec = c(k,v,mantle$p)
    record = rbind(record,rec)
    
    #increment counter
    k = k+500
    v = v-500
  } 
  
  #give column names
  colnames(record) <-c("k","j","p-value")
  record <-as.data.frame(record)
  
  #create new variable
  record$diff <-record$j - record$k
  
  #return value
  return(record)
}


#===================================== Generate Dendrogram =====================================

#dendrogram
dendrogram <-function(dtm)
{
  return(hclust(dist(t(dtm)),"ave"))
}

#==================================== Bayesian Network =======================================

#bayesian network
bayesian_network <- function(dtm)
{
  for (i in ncol(dtm))
  {
    dtm[,c(i)] = as.factor(dtm[,c(i)])
  }
  bn = hc(dtm)
  return(bn)
}
