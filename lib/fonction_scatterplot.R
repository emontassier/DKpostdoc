#do scatterplot of bugs

#choisir l environnement
setwd("C:/Users/Utilisateur/src/data")
getwd()


####scatterplot avec transformation arcsin####
run.scatterplot <- function(scatterfile, filename='scatterplot_pre_postCT14.pdf'){
  scatterfile <- read.table("alltaxons.txt", header = T)
  before <- asin(sqrt(scatterfile[1:15,]))
  after <- asin(sqrt(scatterfile[16:30,]))
  attach(scatterfile)
  attributes(scatterfile)
  tax <- names(scatterfile)
  if(!is.null(filename)) pdf(filename,width=4,height=4)
  for (i in tax[1:30]){
    title.text = i
    plot(before[,i],after[,i],xlim=c(0,0.6),ylim=c(0,0.9), col=c("firebrick2"), pch= 20 , xlab='before chemotherapy',ylab='afterchemotherapy',main= title.text)
    abline(0, 1)
  }
  if(!is.null(filename)) dev.off()
}  

run.scatterplot(tax)
