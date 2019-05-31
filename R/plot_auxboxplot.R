# Boxplot of Residuals.

# Creates a boxplot from input residuals and predicted values
plot_auxboxplot <- function(resid, theme, axis.text.size, title.text.size, title.opt){

  ## Creation of Values to Plot -----------------------------------------------------

  # Create a data frame with the residuals
  model_values <- data.frame(Residual = resid)

  # Add an observation variable
  model_values$Observation <- 1:nrow(model_values)

  ## Creation of Plot ---------------------------------------------------------------

  # Create the boxplot of residuals
  plot <- ggplot(model_values, aes_string(x = " ", y = "Residual")) +
    geom_boxplot(width = .5) +
    geom_point(alpha = 0) +
    labs(x = " ", y = "Residuals")

  # Add theme to plot
  if (theme == "bw"){
    plot <- plot + theme_bw()
  } else if (theme == "classic"){
    plot <- plot + theme_classic()
  } else if (theme == "gray" | theme == "grey"){
    plot <- plot + theme_grey()
  }

  # Set text size of title and axis lables, determine whether to include a title,
  # and return plot
  if (title.opt == TRUE){
    plot +
      labs(title = "Boxplot") +
      theme(plot.title = element_text(size = title.text.size, face = "bold"),
                 axis.title = element_text(size = axis.text.size))
  } else if (title.opt == FALSE){
    plot + theme(axis.title = element_text(size = axis.text.size))
  }

}
