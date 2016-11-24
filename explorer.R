
#load required libraries
library(ggplot2)
library(ggthemes)
library(ape)

#laod pre-defined scripts
source('analytics.R')

explore <- function(dtm)
{
  t = Sys.time()
  #pca analysis
  p = pca_analysis(dtm)
  
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
  
  #plotting
  g <- ggplot(mt, aes(x=mt$diff,y=mt$`p-value`)) + 
    xlab("Difference")+
    ylab("P-value")+
    theme_economist(base_size = 10,horizontal = TRUE)+
    geom_line(aes(x = mt$diff,y = mt$`p-value`,color='red',size=1)) + 
    theme(text=element_text(size=10))+
    ggtitle("Mantels Test \'n")
  plot(g)
  
  #dendrogram
  plot(d)

  #bayesian
  plot(bayes)
  
  dev.off()
  #return values
  
  t1 = Sys.time() - t
  print(t1)
}
