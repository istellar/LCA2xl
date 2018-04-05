#' Square Root of the Results in Probability Scale
#'
#' Provides the user with the option of taking the square root of 
#' the results in probability scale, in case they would like to 
#' graph these results.
#'
#' Takes the Square Root of the Results in Probability Scale
#' @param file is the name of the Mplus output file (.out)
#' @return A list with one mplus model. Each mplus.model object is composed of
#'   elements containing major output sections.
#' @author Stella Min
#' @export
sqprobLCA<-function(textfile, classes=1, usevariableList=usevars) {

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
      presult<-ptable
    }
    
  targetText<-list()
  
  trimSpace <- function(string) {
    stringTrim <- sapply(string, function(x) {
      x <- sub("^\\s*", "", x, perl=TRUE)
      x <- sub("\\s*$","", x, perl=TRUE)
      return(x)
    }, USE.NAMES=FALSE)
    return(stringTrim)
  }
  
  if (length(usevariableList)>0) {
    usevarList<-usevariableList
    if (classes > 1) {
      for (i in 1:classes) {
        #split line by line
        targetText[[i]]<-strsplit(trimSpace(presult[i]), "\\s+", perl=TRUE)
        #id headers
        hlines <- lapply(presult, function(x) grep("^\\s*([\\w_\\d+\\.#\\&]+)\\s*$", x, perl=TRUE))
        hline<-hlines[[1]]
        cat2 <-hline + 2
      }
    }else {
      targetText<-strsplit(trimSpace(presult), "\\s+", perl=TRUE)
      #id headers
      cat2 <- grep("Category 2.*", presult, ignore.case = TRUE, perl=TRUE)
    }
  }
    df2<-c()
    prob_results<-list()
  
    if (length(usevariableList)>0) {
      usevarList<-usevariableList
      
      if (classes >1 ) {
        for (i in 1:classes) {
        tempdf<-targetText[i]
        #No category 1 for now
      
        #category 2 only
        df2<-lapply(tempdf,"[",cat2,drop=FALSE)
        df2<-lapply(df2, function(x) str_replace_all(x, "Category", paste(usevarList,"Cat",sep="_")))
        df2<-lapply(df2, function(x) gsub('["c(\"]', '',x, ignore.case = FALSE))
        df2<-lapply(df2, function(x) gsub('[")"]', '',x))
        prob_results[[i]]<-c(df2)
        }
      } else {
        df2<-targetText[cat2]
        df2<-str_replace_all(df2, "Category", paste(usevarList,"Cat",sep="_"))
        df2<-gsub('["c(\"]', '',df2, ignore.case = FALSE)
        df2<-gsub('[")"]', '',df2, ignore.case=FALSE)
        prob_results<-c(df2)
        }
      }else { warning("Indicator may be incorrect because Usevarlist not provided")}
  
    prob_results<-lapply(prob_results, function(x) trimSpace(x))
  
  tempdf<-list()
  outdf<-list()
  resultsdf<-list() #list, row, column
  
  if (classes >1 ) {
    for (i in 1:classes) {
      numrows<-length(prob_results[[1]])
      tempdf<-strsplit(trimSpace(prob_results[i]), ",", perl=TRUE)
      resultsdf<-lapply(tempdf, function(x) t(x))
      outdf[[i]]<-data.frame(matrix(unlist(resultsdf), nrow=numrows, byrow=6))
      newnames<-c("Variables", "Category", "Prob_Est", "Prob_SE", "P_EST_SE", "PVAL")
      outdf<-lapply(outdf,setNames, newnames)
    }
  }else {
    tempdf<-strsplit(trimSpace(prob_results), ",", perl=TRUE)
    resultsdf<-lapply(tempdf, function(x) t(x))
    numrows<-length(prob_results)
    outdf<-data.frame(matrix(unlist(resultsdf), nrow=numrows, byrow=6))
    newnames<-c("Variables", "Category", "Prob_Est", "Prob_SE", "P_EST_SE", "PVAL")
    names(outdf)<-newnames
  }
  #removing Category
  if (classes > 1) {
    lvec<-length(unlist(outdf[[1]]['Variables']))
    
    #removing Category
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
      
      #create single data frame
      onedf<- ldply(outdf, data.frame)
    } 
  } else {
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
    
    #make 1 data frame
    onedf<- data.frame(outdf)
  }
  
  #square root of estimate
  onedf$Prob_Est<-as.numeric(as.vector(unlist(onedf$Prob_Est)))
  sqLCAmodel<-
    onedf%>%
    mutate(sq_Prob_Est= sqrt(Prob_Est)) %>%
    select(Variables, Indicator, LCAnum, sq_Prob_Est)
  
  #format to 3 decimal points
  sqLCAmodel$sq_Prob_Est<-as.numeric(as.vector(unlist(sqLCAmodel$sq_Prob_Est)))
  sqLCAmodel$sq_Prob_Est<-format(round(sqLCAmodel$sq_Prob_Est,3), nsmall=3)
  #numeric format
  sqLCAmodel$sq_Prob_Est<-as.numeric(as.vector(unlist(sqLCAmodel$sq_Prob_Est)))
  names(sqLCAmodel)<-c("Variables", "Indicator", "LCnum", "P_Est")
  
  if (classes > 1) {
    #spread results over Latent Class
    sqLCAmodel<-sqLCAmodel %>%
      gather(var, val, (`P_Est`)) %>%
      unite(temp, `LCnum`, var) %>%
      spread(temp, val)
    #sort by Indicator
    sqLCAmodel<-arrange(sqLCAmodel,(as.numeric(as.vector(unlist(sqLCAmodel$Indicator)))))
  }
  return(sqLCAmodel)
}
