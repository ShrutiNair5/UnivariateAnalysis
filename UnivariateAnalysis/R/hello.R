# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
#' @title Univariate Analysis
#'
#' @description To plot graph using univariate analysis
#'
#' @param data
#'
#' @return NULL
#'
#' @examples Graphs('cars')
#'
#' @export Graphs


Graphs <- function(data)
{
  for(i in 1:ncol(data))
  {
    if(is.factor(data[,i])|is.character(data[,i]))
    {
      png(paste(names(data)[i], ".png", sep="")) #NOTE this step
      par(mfrow=c(2,1))
      barplot(prop.table(table(data[,i])),col=c('gold','magenta','maroon'))
      #barplot(table(data[,i]))
      percent=100*table(data[,i])/length(data[,i])
      pie(x=percent, label=paste(percent, "%"),  col=rainbow(length(names)), main=paste("Percentage of", names(data)[i]) )





    }
    else
    {
      png(paste(names(data)[i], ".png", sep="")) #NOTE this step

      par(mfrow=c(2,1))
      boxplot(data[,i], main = paste("Boxplot of", names(data)[i]),
              ylab = names(data)[i], col = "maroon", border = "grey5",
              horizontal = T)

      hist(data[,i], main = paste("Histogram of", names(data)[i]),
           xlab = names(data)[i], ylab = "No. of Houses", col = "lightgreen", border=F)


      dev.off()  #NOTE this step
    }

  }

}
