#' Extract Results in Probability Scale from Mplus LCA Model
#' @param file is the name of the Mplus .out file
#' @param classes is 1 by default
#' @param recursive TRUE 
#' @param classes is variable with number of classes
#' @return separates each latent class model in probability scale
#' @export

getProbResults <- function(textfile ,classes=1, usevariableList=usevars, recursive=TRUE) {
  
  textlist<-readLines(textfile)
  #extract and parse mplus input syntax from the output file.
  presult <- list()

  startpresult <- grep("^\\s*(RESULTS IN PROBABILITY SCALE)\\s*$",textlist, ignore.case=TRUE, perl=TRUE)
  if (length(startpresult) == 0L) {
    warning("Could not find beginning of Results in Probability Scale")
    return(presult)
  }
  
  if (classes==1) {
    endpresult <- grep("^\\s*(QUALITY OF NUMERICAL RESULTS)\\s*$", textlist, ignore.case=TRUE, perl=TRUE)
    if (length(endpresult) == 0L) {
      warning("Could not find end of Results in Probability Scale")
      return(presult)
    } else { endpresult <- endpresult[1L] - 3L } #one line before first warning or end of instructions
    
  }else {
    endpresult <- grep("^\\s*(LATENT CLASS ODDS RATIO RESULTS)\\s*$", textlist, ignore.case=TRUE, perl=TRUE)
    if (length(endpresult) == 0L) {
      warning("Could not find end of Results in Probability Scale")
      return(presult)
    } else { endpresult <- endpresult[1L] - 3L } #one line before first warning or end of instructions
  }
  
  presult.text <- textlist[startpresult[1L]:endpresult[1L]] #explicit first element because there could be both warnings and errors.
  
  #parse sectitons out by model
  if (classes==1) {
    ptable<-presult.text
    return(ptable)
  } else {
    ptable<-list()
    for (i in 1:classes) {
      
      #define text to look for
      stext<-paste("Latent Class ",i,sep="") #starting text to search for
      classplus<-i+1
      etext<-paste("Latent Class ",classplus,sep="") #ending text to search for
      
      #look for starting text line in raw output
      stable <- grep(stext, presult.text, ignore.case=TRUE, perl=TRUE)
      
      #as long as it is not the last class
      if (i!=classes) {
        #look for ending text line in raw output
        etable <- grep(etext, presult.text, ignore.case=TRUE, perl=TRUE)
        if (length(etable) == 0L) {
          warning("Could not find end of class table")
          attr(ptable, "start.line") <- attr(ptable, "end.line") <- -1
          return(ptable)
        } else { etable <- etable[1L] - 2L } #one line before first warning or end of instructions
      }
      #if it is the last class--the last line is the length of the file
      else { etable <- (length(presult.text)) }
      
      #output text
      ptable[[i]] <- presult.text[stable[1L]:etable[1L]] #explicit first element because there could be both warnings and errors.
      
    }
  }
 
  targetText<-list()
  
  # trim White Space
  trimSpace <- function(string) {
    stringTrim <- sapply(string, function(x) {
      x <- sub("^\\s*", "", x, perl=TRUE)
      x <- sub("\\s*$","", x, perl=TRUE)
      return(x)
    }, USE.NAMES=FALSE)
    return(stringTrim)
  }
  if (classes > 1) {
    for (i in 1:classes) {
      #split line by line
      targetText[[i]]<-strsplit(trimSpace(ptable[i]), "\\s+", perl=TRUE)
      #id headers
      hlines <- lapply(ptable, function(x) grep("^\\s*([\\w_\\d+\\.#\\&]+)\\s*$", x, perl=TRUE))
      hline<-hlines[[1]]
      cat2 <-hline + 2
    }
  }else {
    targetText<-strsplit(trimSpace(ptable), "\\s+", perl=TRUE)
    #id headers
    cat2 <- grep("Category 2.*", ptable, ignore.case = TRUE, perl=TRUE)
  }
  #category 2 subheader lines
  
  df2<-c()
  prob_results<-list()
  
  if (length(usevariableList)>0) {
    usevars<-usevariableList 
    
    if (classes > 1) {
      for (i in 1:classes) {
        tempdf<-targetText[i]
        #No category 1 for now
      
        #category 2 only
        df2<-lapply(tempdf,"[",cat2,drop=FALSE)
        df2<-lapply(df2, function(x) str_replace_all(x, "Category", paste(usevars,"Cat",sep="_")))
        df2<-lapply(df2, function(x) gsub('["c(\"]', '',x, ignore.case = FALSE))
        df2<-lapply(df2, function(x) gsub('[")"]', '',x))
        prob_results[[i]]<-c(df2)
        }
      } else {
        df2<-targetText[cat2]
        df2<-str_replace_all(df2, "Category", paste(usevars,"Cat",sep="_"))
        df2<-gsub('["c(\"]', '',df2, ignore.case = FALSE)
        df2<-gsub('[")"]', '',df2, ignore.case=FALSE)
        prob_results<-c(df2)
      }
  } else { warning("Indicators may be truncated because usevariable list not provided")
    if (classes > 1) {
      for (i in 1:classes) {
        tempdf<-targetText[i]
        #No category 1 for now
        
        #category 2 only
        df2<-lapply(tempdf,"[",cat2,drop=FALSE)
        df2<-lapply(df2, function(x) str_replace_all(x, "Category", paste("Cat",sep="_")))
        df2<-lapply(df2, function(x) gsub('["c(\"]', '',x, ignore.case = FALSE))
        df2<-lapply(df2, function(x) gsub('[")"]', '',x))
        prob_results[[i]]<-c(df2)
      }
    } else {
      df2<-targetText[cat2]
      df2<-str_replace_all(df2, "Category", paste("Cat",sep="_"))
      df2<-gsub('["c(\"]', '',df2, ignore.case = FALSE)
      df2<-gsub('[")"]', '',df2, ignore.case=FALSE)
      prob_results<-c(df2)
    }
  }
  prob_results<-lapply(prob_results, function(x) trimSpace(x))
  
  
  tempdf<-list()
  outdf<-list()
  resultsdf<-list() #list, row, column
  
  if (classes > 1) {
    for (i in 1:classes) {
      tempdf<-strsplit(trimSpace(prob_results[i]), ",", perl=TRUE)
      resultsdf<-lapply(tempdf, function(x) t(x))
      numrows<-length(prob_results[[1]])
      outdf[[i]]<-data.frame(matrix(unlist(resultsdf), nrow=numrows, byrow=6))
      newnames<-c("Variables", "Category", "Prob_Est", "Prob_SE", "P_EST_SE", "PVAL")
      outdf<-lapply(outdf,setNames, newnames)
    }
  } else {
    tempdf<-strsplit(trimSpace(prob_results), ",", perl=TRUE)
    resultsdf<-lapply(tempdf, function(x) t(x))
    numrows<-length(prob_results)
    outdf<-data.frame(matrix(unlist(resultsdf), nrow=numrows, byrow=6))
    newnames<-c("Variables", "Category", "Prob_Est", "Prob_SE", "P_EST_SE", "PVAL")
    names(outdf)<-newnames
  }
  #removing Category
  if (classes == 1) {
    lvec<-length(unlist(outdf$Variables))
    outdf$Category<-NULL
    outdf$P_EST_SE<-NULL
    
    #creating indicator
    outdf$Indicator<-as.numeric(unlist(stri_extract_all_regex(outdf$Variables,"[0-9]+")))
    
    #removing Cat and number from Variable name
    outdf$Variables<-as.factor(unlist(stri_extract_all_regex(substr(outdf$Variables,
                                                                    1,nchar(as.character(outdf$Variables))-4),
                                                             "[A-Z]+")))
    
    #reorder columns
    outdf <- outdf %>%
      select(Variables, Indicator, Prob_Est, Prob_SE, PVAL)
    
    #adding a variable to indicate class number
    outdf$LCAnum<-as.character(as.vector(paste("LCA",rep(1,lvec),sep="")))
    probscale_results<- data.frame(outdf)
    
    names(probscale_results)<-c("Measures", "Indicator", "Est.", "S.E.", "Pval", "Latent Class")
    
    #add stars to tablefor significance value
    probscale_results$Pval<-as.numeric(as.vector(unlist(probscale_results$Pval)))
    probscale_results$Est.<-ifelse((probscale_results$Pval < 0.001), paste(probscale_results$Est.,"***",sep=""),
                                   ifelse((probscale_results$Pval < 0.01), paste(probscale_results$Est.,"**",sep=""),
                                          ifelse((probscale_results$Pval < 0.05), paste(probscale_results$Est., "*", sep=""), paste(probscale_results$Est.,sep=""))))
    probscale_results$Pval<-NULL
    probscale_results<-arrange(probscale_results,(as.numeric(as.vector(unlist(probscale_results$Indicator)))))
    return(probscale_results)
  }
  else{
    lvec<-length(unlist(outdf[[1]]['Variables']))
    
    for (i in 1:classes) {
      outdf[[i]]['Category']<-NULL
      outdf[[i]]['P_EST_SE']<-NULL
      
      #creating indicator
      outdf[[i]]['Indicator']<-stri_extract_all_regex(outdf[[i]]['Variables'][[1]],
                                                      "[0-9]+") %>%
        unlist %>%
        matrix(ncol=1, byrow=T) %>%
        data.frame %>%
        setNames(c("Indicator")) %>%
        bind_cols(outdf["Indicator"], .)
      
      #removing Cat and number from Variable name
      outdf[[i]]['Variables']<-stri_extract_all_regex((substr(outdf[[i]]['Variables'][[1]],
                                                              1,nchar(as.character(outdf[[i]]['Variables'][[1]]))-4)),
                                                      "[A-Z]+") %>%
        unlist %>%
        matrix(ncol=1, byrow=T) %>%
        data.frame %>%
        setNames(c("Var_Name")) %>%
        bind_cols(outdf["Var_Name"], .)
      
      #reorder columns
      outdf[[i]] <- outdf[[i]] %>%
        select(Variables, Indicator, Prob_Est, Prob_SE, PVAL)
      
      #adding a variable to indicate class number
      outdf[[i]]['LCAnum']<-as.character(as.vector(paste("LCA",rep(i,lvec),sep="")))
    }
    probscale_results<- ldply(outdf, data.frame)
    
    names(probscale_results)<-c("Measures", "Indicator", "Est.", "S.E.", "Pval", "Latent Class")
    
    #add stars to tablefor significance value
    probscale_results$Pval<-as.numeric(as.vector(unlist(probscale_results$Pval)))
    probscale_results$Est.<-ifelse((probscale_results$Pval < 0.001), paste(probscale_results$Est.,"***",sep=""),
                                   ifelse((probscale_results$Pval < 0.01), paste(probscale_results$Est.,"**",sep=""),
                                          ifelse((probscale_results$Pval < 0.05), paste(probscale_results$Est., "*", sep=""), paste(probscale_results$Est.,sep=""))))
    probscale_results$Pval<-NULL
    
    #spread results over Latent Class
    probscale_results<-probscale_results %>%
      gather(var, val, (`Est.`:`S.E.`)) %>%
      unite(temp, `Latent Class`, var) %>%
      spread(temp, val)
    #sort by Indicator
    probscale_results<-arrange(probscale_results,(as.numeric(as.vector(unlist(probscale_results$Indicator)))))
  }
  return(probscale_results)
}
