
#load required libraries
library(ggplot2)
library(ggthemes)

#laod pre-defined scripts
source('analytics.R')

explore <- function(dtm)
{
  t = Sys.time()
  #pca analysis
  p = pca_analysis(dtm)
  
  #hc-pca
  h = hc_pca(dtm)
  
  #mantels test
  mt = mantel(dtm)
  
  #dendrogam
  d = dendrogram(dtm)
  
  #bayesian network
  bayes = bayesian_network(dtm)
  
  #plotting
  pdf("analytics.pdf",width=7,height=5)
  q = qplot(PC1,PC2,data = p,main = 'Principal Component Analysis\n',label=rownames(p),geom='text')
  plot(q)
  
  #dendrogram
  plot(d)
  
  #hc-pca
  plot(x = h, axes = c(1,2), choice = "3D.map", 
       draw.tree = TRUE, ind.names = TRUE, title = NULL,
       tree.barplot = TRUE, centers.plot = FALSE)
  
  #plotting
  #q = qplot(PC1,PC2,mt = p,main = 'Principal Component Analysis\n',label=rownames(p),geom='text')
  g <- ggplot(mt, aes(x=mt$diff,y=mt$`p-value`)) + 
    xlab("Difference")+
    ylab("P-value")+
    theme_economist(base_size = 10,horizontal = TRUE)+
    geom_line(aes(x = mt$diff,y = mt$`p-value`,color='red',size=1)) + 
    theme(text=element_text(size=10))+
    ggtitle("Mantels Test \'n")
   plot(g) 
  
  #bayesian
  plot(bayes)
  
  dev.off()
  #return values
  
  t1 = Sys.time() - t
  print(t1)
}
