#' Extract Threshold Section of LCA Model Output
#' @param filename is the name of the Mplus .out file 
#' @param classes is 1 by default
#' Extracts Threshold Results from Mplus Model
#' @param filename is the name the Mplus lca model (.out)
#' @param classes is 1 by default
#' @param usevariableList is the an object containing the list of variables 
#' @return Model result thresholds
#' @author Stella Min
#' @keywords internal
#' @example
#' make me
#' @export
getThreshold<-function(filename, classes=1, usevariableList=usevars) {
  
  rawtext<-readLines(filename)
  #trim White Space
  trimSpace <- function(string) {
    stringTrim <- sapply(string, function(x) {
      x <- sub("^\\s*", "", x, perl=TRUE)
      x <- sub("\\s*$","", x, perl=TRUE)
      return(x)
    }, USE.NAMES=FALSE)
    return(stringTrim)
  }
  fileText<-strsplit(trimSpace(rawtext), "\\s+", perl=TRUE)
  
  tablenames<-c(unlist(fileText[grep("Two-Tailed", fileText, ignore.case=TRUE, perl=TRUE)+1]))
  tablenames[1]<-"Est."
  tablenames[4]<-"PVAL"
  
  #pull threshholds
  start_thresh<-grep("^\\s*(MODEL RESULTS)\\s*$", rawtext, ignore.case=TRUE, perl=TRUE) + 5
  end_thresh<- ((length(usevars) * classes) + (4 * classes))+start_thresh-2
  thresh_text<-rawtext[start_thresh:end_thresh]
  
  matches <- gregexpr("^\\s*((Thresholds)|([\\w_\\d+\\.#\\&]+\\s+))\\s*$", thresh_text, perl=TRUE)
  convertMatches <- ldply(matches, function(row) data.frame(start=row, end=row+attr(row, "match.length")-1))
  convertMatches$startline <- 1:nrow(convertMatches)
  convertMatches <- convertMatches[which(convertMatches$start > 0),]
  
  if (classes > 1) { 
    convertMatches$endline<-convertMatches$startline[2:length(convertMatches)] -4 
    if (convertMatches[classes,classes]) {
      convertMatches$endline[classes]<-length(thresh_text)}
  } else { convertMatches$endline<-length(thresh_text) }
  
  convertMatches$start<-NULL
  convertMatches$end<-NULL
  
  rownames<-c(paste("LC", 1:classes,sep=" "))
  row.names(convertMatches)<-rownames
  
  #thresh_text<- strsplit(trimSpace(thresh_text), "\\s+", perl=TRUE)
  df1<-list()
  df2<-list()
  df3<-list()
  threshmodels<-list() #list, row, column
  
  if (length(usevariableList) >0) {
    numrows<-length(usevariableList)
    usevars<-usevariableList

    for(i in 1:classes) {
      df1[[i]]<-c(thresh_text[(convertMatches$startline[i]+1):convertMatches$endline[i]])
      df2<-strsplit(trimSpace(df1[i]), "\\s+", perl=TRUE)
      df3<-lapply(df2, function(x) t(x))
      threshmodels[[i]]<-data.frame(matrix(unlist(df3), nrow=numrows, byrow=5))
      names(threshmodels[[i]])<-c("Variables",tablenames)
      #replace variables with usevars
      threshmodels[[i]]['Variables']<-unlist(usevars)
    
      #create indicator
      threshmodels[[i]]['Indicator']<-stri_extract_all_regex(threshmodels[[i]]['Variables'][[1]],
                                                           "[0-9]+") %>%
        unlist %>%
        matrix(ncol=1, byrow=T) %>%
        data.frame %>%
        setNames(c("Indicator")) %>%
        bind_cols(threshmodels["Indicator"], .)
    
      #remove indicator from variables
      threshmodels[[i]]['Variables']<-stri_extract_all_regex(threshmodels[[i]]['Variables'][[1]],
                                                           "[A-Z]+") %>%
        unlist %>%
        matrix(ncol=1, byrow=T) %>%
        data.frame %>%
        setNames(c("Var_Name")) %>%
        bind_cols(threshmodels["Var_Name"], .)
    
    
      #adding a variable to indicate class number
      lvec<-length(unlist(threshmodels[[1]]['Variables']))
      threshmodels[[i]]['LC']<-as.character(as.vector(paste("LCA", rep(i,lvec), sep="")))
    
      #reorder columns and drop Est./S.E.
      threshmodels[[i]] <- threshmodels[[i]] %>%
        select(Variables, Indicator, LC, Est., S.E., PVAL)
    }
    threshmodels<-ldply(threshmodels, data.frame)
  
    #add stars and remove pvalue
    threshmodels$PVAL<-as.numeric(as.vector(unlist(threshmodels$PVAL)))
    threshmodels$Est.<-as.numeric(as.vector(unlist(threshmodels$Est.)))
    threshmodels$S.E.<-as.numeric(as.vector(unlist(threshmodels$S.E.)))
  
    threshmodels$Est.<-ifelse((threshmodels$PVAL < 0.001), paste(threshmodels$Est.,"***",sep=""),
                            ifelse((threshmodels$PVAL < 0.01), paste(threshmodels$PVAL,"**",sep=""),
                                   ifelse((threshmodels$PVAL < 0.05), paste(threshmodels$PVAL, "*", sep=""), 
                                          paste(threshmodels$PVAL ,sep=""))))
    threshmodels$PVAL<-NULL
  
    threshmodels<-threshmodels %>%
      gather(var, val, (`Est.`:`S.E.`)) %>%
      unite(temp, `LC`, var) %>%
      spread(temp, val)
  
    #sort by Indicator
    threshmodels<-arrange(threshmodels,(as.numeric(as.vector(unlist(threshmodels$Indicator)))))
  } else {
    warning("Indicator may be incorrect because variables were truncated!")
    
    for(i in 1:classes) {
      df1[[i]]<-c(thresh_text[(convertMatches$startline[i]+1):convertMatches$endline[i]])
      df2<-strsplit(trimSpace(df1[i]), "\\s+", perl=TRUE)
      df3<-lapply(df2, function(x) t(x))
      threshmodels[[i]]<-data.frame(matrix(unlist(df3), nrow=numrows, byrow=5))
      names(threshmodels[[i]])<-c("Variables",tablenames)
      
      #create indicator
      threshmodels[[i]]['Indicator']<-stri_extract_all_regex(threshmodels[[i]]['Variables'][[1]],
                                                             "[0-9]+") %>%
        unlist %>%
        matrix(ncol=1, byrow=T) %>%
        data.frame %>%
        setNames(c("Indicator")) %>%
        bind_cols(threshmodels["Indicator"], .)
      
      #remove indicator from variables
      threshmodels[[i]]['Variables']<-stri_extract_all_regex(threshmodels[[i]]['Variables'][[1]],
                                                             "[A-Z]+") %>%
        unlist %>%
        matrix(ncol=1, byrow=T) %>%
        data.frame %>%
        setNames(c("Var_Name")) %>%
        bind_cols(threshmodels["Var_Name"], .)
      
      
      #adding a variable to indicate class number
      lvec<-length(unlist(threshmodels[[1]]['Variables']))
      threshmodels[[i]]['LC']<-as.character(as.vector(paste("LCA", rep(i,lvec), sep="")))
      
      #reorder columns and drop Est./S.E.
      threshmodels[[i]] <- threshmodels[[i]] %>%
        select(Variables, Indicator, LC, Est., S.E., PVAL)
    }
    threshmodels<-ldply(threshmodels, data.frame)
    
    #add stars and remove pvalue
    threshmodels$PVAL<-as.numeric(as.vector(unlist(threshmodels$PVAL)))
    threshmodels$Est.<-as.numeric(as.vector(unlist(threshmodels$Est.)))
    threshmodels$S.E.<-as.numeric(as.vector(unlist(threshmodels$S.E.)))
    
    threshmodels$Est.<-ifelse((threshmodels$PVAL < 0.001), paste(threshmodels$Est.,"***",sep=""),
                              ifelse((threshmodels$PVAL < 0.01), paste(threshmodels$PVAL,"**",sep=""),
                                     ifelse((threshmodels$PVAL < 0.05), paste(threshmodels$PVAL, "*", sep=""), 
                                            paste(threshmodels$PVAL ,sep=""))))
    threshmodels$PVAL<-NULL
    
    threshmodels<-threshmodels %>%
      gather(var, val, (`Est.`:`S.E.`)) %>%
      unite(temp, `LC`, var) %>%
      spread(temp, val)
    
    #sort by Indicator
    threshmodels<-arrange(threshmodels,(as.numeric(as.vector(unlist(threshmodels$Indicator)))))
  }
    return(threshmodels)
}