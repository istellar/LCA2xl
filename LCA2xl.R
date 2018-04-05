#' Export Mplus LCA Results to Excel and Save
#'
#' Exports use-specified Mplus LCA results to and Excel woorkbook
#' and saves the results. 
#'
#' @param models is a list of model generated with getLCA or the optional sqprobLCA function
#' @param returnfile is the name of the file the user would like to save
#' @return An excel workbook (.xlsx) file containing models, separated by sheet
#' @author Stella Min
#' @export


LCA2xl<- function(modelList=list(), 
                  returnfile="Results.xlsx") {
  
  if (length(modelList) == 1) {
    x<-xl.workbook.add()
    #sheet 1
    xl.sheet.add("sheet 1",before=1)
    xlrc[a3]=list
    xl.list = xl.connect.table("a3", row.names=FALSE, col.names=TRUE)

    
  }
  else {
    x<-xl.workbook.add()
    for (i in 1:length(modelList)) {
      xl.sheet.add(xl.sheet.name =paste("sheet",i,sep=" "),before=i)
      xlrc[a3]=models[[i]]
      xl.models=xl.connect.table("a3", row.names=TRUE, col.names=TRUE)
      xl.models[,1]<-NULL
    }
  }
  returnfile<-xl.workbook.save(returnfile)
  return(returnfile)
}