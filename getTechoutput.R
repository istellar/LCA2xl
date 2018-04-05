#' Extracts Technical Output from Mplus LCA Model (.out)
#'
#' Extracts Technical 11 Output from one Mplus output file, 
#'
#' @param outfile is the name of the Mplus output file (.out)
#' @param classes is 1 by default
#' @return A list with one mplus model. Each mplus.model object is composed of
#'   elements containing major output sections.
#' @author Stella Min
#' @export
getTech11<-function(outfile, classes=1, recursive=TRUE) {
  outfiletext<-readLines(outfile)

  tech11.text<-list()
  tech11<-list()
  VLMR<-list()
  LMR<-list()
  if (classes==1) {
    stop("Technical output not available for 1 class models")
  } else {
    stech11 <- grep("^\\s*(TECHNICAL 11 OUTPUT)\\s*$", outfiletext, ignore.case=TRUE, perl=TRUE)
    if (length(stech11) == 0L) {
      warning("Could not find beginning of technical 11 output")
      attr(tech11, "start.line") <- attr(tech11s, "end.line") <- -1L
      return(tech11)
    }
    etech11 <- grep("^\\s*(TECHNICAL 14 OUTPUT|^\\s*SAVEDATA INFORMATION|^\\s*MUTHEN & MUTHEN)\\s*$", outfiletext, ignore.case=TRUE, perl=TRUE) -3L
    if (length(etech11) > 1 & etech11[1] > 0) {
      etech11<-etech11[1]
    }
    if (etech11[1] < 0) {
      etech11<-etech11[2]
    } else {
      warning("Could not find end of technical 11 output")
      attr(tech11, "start.line") <- attr(tech11, "end.line") <- -1
      return(tech11)
    }
    
    tech11.text<-outfiletext[stech11:etech11]
    h11<-grep("(VUONG-LO-MENDELL-RUBIN.* | LO-MENDELL-RUBIN.*)", tech11.text, ignore.case=TRUE, perl=TRUE) 
    
    # trim White Space
    trimSpace <- function(string) {
      stringTrim <- sapply(string, function(x) {
        x <- sub("^\\s*", "", x, perl=TRUE)
        x <- sub("\\s*$","", x, perl=TRUE)
        return(x)
      }, USE.NAMES=FALSE)
      return(stringTrim)
    }
    
    tech11.text<-strsplit(trimSpace(tech11.text), "\\s+", perl=TRUE)
    VLMR$H0<-tech11.text[[h11[1]+2]][4]
    VLMR$LLD<-tech11.text[[h11[1]+3]][6]
    VLMR$DPN<-tech11.text[[h11[1]+4]][7]
    VLMR$Mean<-tech11.text[[h11[1]+5]][2]
    VLMR$SD<-tech11.text[[h11[1]+6]][3]
    VLMR$Pval<-tech11.text[[h11[1]+7]][2]
    
    LMR$LRT<-tech11.text[[h11[2]+2]][2]
    LMR$Pval<-tech11.text[[h11[2]+3]][2]
    
    tech11<-list(VLMR=c(VLMR), LMR=c(LMR))
  }
  return(tech11)
}

#' Extracts Technical Output from Mplus LCA Model (.out)
#'
#' Extracts Technical 11 Output from one Mplus output file, 
#'
#' @param modeloutput is the name of the Mplus output file (.out)
#' @param classes is 1 by default
#' @return A list with one mplus model. Each mplus.model object is composed of
#'    elements containing major output sections.
#' @author Stella Min
#' @export
getTech14<-function(modeloutput, classes=1, recursive=TRUE) {
  lcatext<-readLines(modeloutput)
  
  
  tech14.text<-list()
  tech14<-list()
  BLRT<-list()
  
  if (classes==1) {
    stop("Technical output not available for 1 class models")
  } else {
    stech14 <- grep("^\\s*(TECHNICAL 14 OUTPUT)\\s*$", lcatext, ignore.case=TRUE, perl=TRUE)
    if (length(stech14) == 0L) {
      warning("Could not find beginning of technical 14 output")
      attr(tech14, "start.line") <- attr(tech14, "end.line") <- -1L
      return(tech14)
    }
    
    etech14 <- grep("^\\s*(SAVEDATA INFORMATION|^\\s*MUTHEN & MUTHEN)\\s*$", lcatext, ignore.case=TRUE, perl=TRUE) -3L
    if (length(etech14) > 1 & etech14[1] > 0) {
      etech14<-etech14[1]
    } else{
      (etech14[1] < 0) 
      etech14<-etech14[2]
    } 
    
    tech14.text<-lcatext[stech14:etech14]
    h14<-grep("(PARAMETRIC BOOTSTRAPPED.*)", tech14.text, ignore.case=TRUE, perl=TRUE) 
    
    #trim White Space
    trimSpace <- function(string) {
      stringTrim <- sapply(string, function(x) {
        x <- sub("^\\s*", "", x, perl=TRUE)
        x <- sub("\\s*$","", x, perl=TRUE)
        return(x)
      }, USE.NAMES=FALSE)
      return(stringTrim)
    }
    
    tech14.text<-strsplit(trimSpace(tech14.text), "\\s+", perl=TRUE)
    BLRT$H0<-tech14.text[[h14[1]+2]][4]
    BLRT$LLD<-tech14.text[[h14[1]+3]][6]
    BLRT$DPN<-tech14.text[[h14[1]+4]][7]
    BLRT$Pval<-tech14.text[[h14[1]+5]][3]
    BLRT$SBD<-tech14.text[[h14[1]+6]][4]
    
    tech14<-c(BLRT)
  } 
  return(tech14) 
}
