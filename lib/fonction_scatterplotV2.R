run.scatterplot <- function(scatterfile, filename='scatterplot_pre_postCT2.pdf'){
  scatterfile <- read.table("alltaxons.txt", header = T)
  before <- asin(sqrt(scatterfile[1:15,]))
  after <- asin(sqrt(scatterfile[16:30,]))
  attach(scatterfile)
  attributes(scatterfile)
  tax <- names(scatterfile)
  if(!is.null(filename)) pdf(filename,width=4,height=4)
  for (i in tax[1:30]){
    title.text = i
    if ((max(before[,i])<0.1) & (max(after[,i])< 0.1)){
      plot(before[,i],after[,i],xlim=c(0,0.1),ylim=c(0,0.1), col=c("firebrick2"), pch= 20 , xlab='before chemotherapy',ylab='afterchemotherapy',main= title.text)
      abline(0, 1)
      } else if ((max(before[,i])<0.3) & (max(after[,i])< 0.3)){
        plot(before[,i],after[,i],xlim=c(0,0.4),ylim=c(0,0.4), col=c("firebrick2"), pch= 20 , xlab='before chemotherapy',ylab='afterchemotherapy',main= title.text)
        abline(0, 1)
      } else {
        plot(before[,i],after[,i],xlim=c(0,0.6),ylim=c(0,0.9), col=c("firebrick2"), pch= 20 , xlab='before chemotherapy',ylab='afterchemotherapy',main= title.text)
        abline(0, 1)
      }
    
  }
  if(!is.null(filename)) dev.off()
}  

run.scatterplot()
