# Q-Q Plot.
#
# Creates a Q-Q plot on the residuals from a model.
#
# @param resid Residuals from a model.
# @param pred Fitted values from a model.
# @return A Q-Q plot of the residuals.
# creating this Q-Q plot follows that used by SAS:
#
# \url{http://support.sas.com/documentation/cdl/en/sgug/59902/HTML/default/viewer.htm#fit_sect51.htm#sg_fit_fitresidualnormalquantiles}
#
# After sorting the residuals in ascending order, for each of the ith ordered residuals,
# the following quantile is computed:
#
# \deqn{\Phi^{-1} * ((i - 0.375) / (n + 0.25))}
#
# Each pair of points is then plotted to create the Q-Q plot. The line drawn on the
# plot has an intercept equal to the mean of the residuals and a slope equal to the
# standard deviation of the residuals. Data that is approximately normal will fall
# very close to or on the line.
#
# @examples
# model <- lm(Volume ~ Girth, data = trees)
# resid_qq(resid(model), fitted(model))

resid_sqq <- function(resid, pred, theme, axis.text.size, title.text.size, title.opt, qqline, qqbands){

  ## Creation of Values to Plot -----------------------------------------------------
  ## Creation of Labels -------------------------------------------------------------
  ## Creation of Plot ---------------------------------------------------------------

  r <- data.frame(r=resid)

  if(qqbands==TRUE){
    plot <- ggplot(data = r, mapping = aes(sample = r)) +
      stat_qq_band()+
      stat_qq_point() +labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
  }else{
    plot <- ggplot(data = r, mapping = aes(sample = r)) +
      stat_qq_point() +labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
  }



  if(qqline==TRUE){
    plot <- plot+stat_qq_line(color="blue",size=.5)
  }




  # Add theme to plot
  if (theme == "bw"){
    plot <- plot + theme_bw()
  } else if (theme == "classic"){
    plot <- plot + theme_classic()
  } else if (theme == "gray" | theme == "grey"){
    plot <- plot + theme_grey()
  }


  # Set text size of title and axis lables, determine whether to include a title, and return plot
  if(title.opt == TRUE){
    plot +
      labs(title = "Q-Q Plot of Residuals") +
      theme(plot.title = element_text(size = title.text.size, face = "bold"),
            axis.title = element_text(size = axis.text.size))
  } else if (title.opt == FALSE){
    plot + theme(axis.title = element_text(size = axis.text.size))
  }

}

