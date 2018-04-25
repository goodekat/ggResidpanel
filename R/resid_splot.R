# Residual Plot.
#
# Creates a residual plot with residuals versus predicted values from a model.
#
# @param resid Residuals from a model.
# @param pred Fitted values from a model.
# @return A histogram of the residuals.
# @examples
# model <- lm(Volume ~ Girth, data = trees)
# resid_splot(resid(model), pred(model))


resid_splot <- function(resid, pred,smoother, theme, axis.text.size, title.text.size, title){



  # Create a data frame with the residuals

  model_values <- data.frame(Residual=resid, Predicted=pred)

  # Create the residual plot
  plot <- ggplot(data=model_values, aes(x = Predicted, y = Residual)) +
    geom_point() +
    geom_abline(slope = 0, intercept = 0, color = "blue") +
    labs(x = "Predicted Values", y = "Residuals")


  # If smoother is set to true, add it to the plot
  if (smoother == TRUE){
   plot <- plot +
     geom_smooth(colour = "red", se = FALSE, method = "loess", size = 0.5)
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
  if(title == TRUE){
    plot +
      labs(title = "Residuals Plot") +
      theme(plot.title = element_text(size = title.text.size, face = "bold"),
            axis.title = element_text(size = axis.text.size))
  } else if (title == FALSE){
    plot + theme(axis.title = element_text(size = axis.text.size))
  }

}
