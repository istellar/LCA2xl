#' Optional Function To Parse UseVariable List from Mplus Model
#' @param file is the name of the Mplus .out file 
#' @return number the variables used in LCA model
#' @author Stella Min
#' @export

getUsevars<- function(file,recursive=TRUE) {
  text<-readLines(file)
  
  #trim White Space
  trimSpace <- function(string) {
    stringTrim <- sapply(string, function(x) {
      x <- sub("^\\s*", "", x, perl=TRUE)
      x <- sub("\\s*$","", x, perl=TRUE)
      return(x)
    }, USE.NAMES=FALSE)
    return(stringTrim)
  }
  fileText<-strsplit(trimSpace(text), "\\s+", perl=TRUE)
  
  if (! grep("USEVARIABLES", fileText, ignore.case=TRUE, perl=TRUE)==0) {
    start_usevars<-grep("USEVARIABLES", fileText, ignore.case=TRUE, perl=TRUE)
    uselist<-fileText[start_usevars:length(fileText)] #search new text starting from usevariables
    end_usevars<-grep(";", uselist)
    
    usevars<- uselist[1L:end_usevars[1L]]
    usevars<-as.character(as.vector(unlist(lapply(usevars[2:length(usevars)], function(x) sub(";","",x,perl=TRUE)))))
    usevars<-toupper(usevars) #capitalize
  } else { warning("Could not find usevariable list") }
  
  #capitalize strings function
  usevarList<-lapply(usevars, function(v) {
    if (is.character(v)) return(toupper(v))
    else return(v)
  })
  usevarList<-unlist(usevarList)
  return(usevarList)
}