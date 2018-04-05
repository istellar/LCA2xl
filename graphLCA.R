#' Line Graph of Mplus LCA Results
#'
#' Creates a line graph of the LCA Results provided by the user.
#'
#' @param model is the LCA model generated with getLCA or the optional sqprobLCA function
#' @param variable is the variable the user would like to use in the line graph
#' @param x is the variable for the x-axis
#' @param y is the variable for the y-axis
#' @param classes is the variable containing the class indicator--best if factor variable.
#' @param title is what the user would like to title the line graph
#' @param xtitle is the title for the x-axis
#' @param ytitle is the title for the y-axis
#' @return A line graph of one mplus LCA model. 
#' @author Stella Min
#' @export


graphLCA<-function( model=model , variable=variables, x=x , y=y, classes=1, 
                   title="Title",
                   xtitle="xTitle", ytitle="yTitle") {

  if (classes==1) {
    head(model)
    #dev.off() #to view results
    r_graph <- ggplot(data = model,
                      aes(x = x, y =y,
                          color=Variables), environment=environment()) +
      geom_line() +
      geom_point() +
      labs(colour = "Measures",
           fill= "Variables",
           title = title,
           x = xtitle,
           y = ytitle) +
      scale_y_continuous(name=ytitle, limits=c(0, 1.01),
                         breaks =c(.10, .20, .30, .40,.50, .60, .70, .80, .90, 1.00)) +
      scale_x_discrete(name=xtitle, limits=c(1:max(x)),
                       breaks =c(1:max(x))) +
      #theme_minimal()
      scale_linetype_discrete ("Latent Class") +
      theme(axis.title.x= element_text(size = rel(.8)),
            axis.title.y= element_text(size = rel(.8)),
            axis.text.y= element_text(size=7),
            axis.text.x= element_text(size=7),
            plot.caption = element_text(size = rel(.7)),
            panel.grid.major= element_blank(),
            legend.key= element_rect(fill="white"),
            #panel.border = element_rect(linetype="line"),
            panel.background =element_rect(fill = "white", color="grey50"))
  }else {
    head(model)
    #dev.off() #to view results
    r_graph <- ggplot(data = model,
                      aes(x = x, y =y,
                          group=interaction(Variables, classes),
                          color=Variables), environment=environment()) +
      geom_line(aes(linetype=factor(classes))) +
      geom_point() +
      labs(colour = "Measures",
           fill= "Variables",
           title = title,
           x = xtitle,
           y = ytitle) +
      scale_y_continuous(name=ytitle, limits=c(0, 1.01),
                         breaks =c(.10, .20, .30, .40,.50, .60, .70, .80, .90, 1.00)) +
      scale_x_discrete(name=xtitle, limits=c(1:max(x)),
                       breaks =c(1:max(x))) +
      #theme_minimal()
      scale_linetype_discrete ("Latent Class") +
      theme(axis.title.x= element_text(size = rel(.8)),
            axis.title.y= element_text(size = rel(.8)),
            axis.text.y= element_text(size=7),
            axis.text.x= element_text(size=7),
            plot.caption = element_text(size = rel(.7)),
            panel.grid.major= element_blank(),
            legend.key= element_rect(fill="white"),
            #panel.border = element_rect(linetype="line"),
            panel.background =element_rect(fill = "white", color="grey50"))
    
    if(numclass > 3 ) {
      warning("The appearance of the graph may be crowded due to the number
              of classes. Consider graphing by a specific measure and/or
              indicator across classes.")
    }
    }
  return(r_graph)
}

