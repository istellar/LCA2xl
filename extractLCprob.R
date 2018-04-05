#' for Average Probability Latent Class Membership
#' @param file is the name of the Mplus .out file 
#' @return section of model with results in probability scale
#' @author Stella Min
#' @export
extract_LCprob<-function(rawtext, classes=classes) {
  textlist<-readLines(rawtext)
  start_cn<- grep("^\\s*(Class Counts and Proportions)\\s*$", textlist, ignore.case=TRUE, perl=TRUE) + 5
  end_cn<- start_cn+1

  #Average Latent Class Probabilities for Most Likely Latent Class Membership (Row)
  #by Latent Class (Column) section
  start_avprob<- end_cn + classes + 6
  end_avprob<- start_avprob + classes
  
  #trim White Space
  trimSpace <- function(string) {
    stringTrim <- sapply(string, function(x) {
      x <- sub("^\\s*", "", x, perl=TRUE)
      x <- sub("\\s*$","", x, perl=TRUE)
      return(x)
    }, USE.NAMES=FALSE)
    return(stringTrim)
  }
  fileText<-strsplit(trimSpace(textlist), "\\s+", perl=TRUE)
  
  avLCprob<-data.frame(matrix(unlist(fileText[start_avprob:end_avprob]), nrow=classes, byrow=1))
  avLCprob$X1<-lapply(avLCprob$X1, function(x) paste("Latent Class",x, sep=" "))
  
  LCprob<-list()
  for (i in 1:classes) {
    LCprob[[i]]<-as.numeric(as.vector(avLCprob[[(1+i)]][i]))
    names(LCprob[[i]])<-paste("LCprob",i,sep="")
  }
  
  LCprob<-data.frame(unlist(LCprob))
  names(LCprob)<-"LCprob"
  return(LCprob)
}

