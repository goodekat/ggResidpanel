# Histogram of Residuals.
#
# Creates a histogram of the residuals from a model.
#
# @param resid Residuals from a model.
# @param pred Fitted values from a model.
# @return A histogram of the residuals.
# @examples
# model <- lm(Volume ~ Girth, data = trees)
# resid_shist(resid(model), pred(model))


resid_shist <- function(resid, pred, bins, theme, axis.text.size, title.text.size, title.opt){

  #If bins=NA, use default
  if(is.na(bins)){
    bins <- 30
  }

    model_values <- data.frame(resid=resid)
  #Step to make sure are not cutting out any huge outliers
  if (min(model_values$resid) < -4*sd(model_values$resid)){
    min_x <- NA
  }else{
    min_x <- -4*sd(model_values$resid)
  }

  if (max(model_values$resid) > 4*sd(model_values$resid)){
    max_x <- NA
  }else{
    max_x <- 4*sd(model_values$resid)
  }

  #do not want xlim if data outside 4*sd
  if (is.na(min_x)&is.na(max_x)){
    # Create the histogram of residuals
    plot <- ggplot(model_values, aes(x = resid)) +
      geom_histogram(aes(y = ..density.., fill = ..count..),
                     color = "black", fill = "grey82", bins = bins) +
      stat_function(fun = dnorm, color = "blue",
                    args = list(mean = 0,
                                sd = sd(model_values$resid))) +
      labs(x = "Residuals", y = "Density") +
      theme(plot.title = element_text(size = 12, face = "bold"),
            axis.title = element_text(size = 10))
  }else{
  # Create the histogram of residuals
  plot <- ggplot(model_values, aes(x = resid)) +
    geom_histogram(aes(y = ..density.., fill = ..count..),
                   color = "black", fill = "grey82", bins = bins) +
    stat_function(fun = dnorm, color = "blue",
                  args = list(mean = 0,
                              sd = sd(model_values$resid))) +
    labs(x = "Residuals", y = "Density") +
      xlim(c(min_x, max_x))+
    theme(plot.title = element_text(size = 12, face = "bold"),
          axis.title = element_text(size = 10))
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
      labs(title =  "Histogram of Residuals") +
      theme(plot.title = element_text(size = title.text.size, face = "bold"),
            axis.title = element_text(size = axis.text.size))
  } else if (title.opt == FALSE){
    plot + theme(axis.title = element_text(size = axis.text.size))
  }

}
