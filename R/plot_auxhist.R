# Histogram of Residuals.

# Creates a histogram of the input residuals
plot_auxhist <- function(resid, bins, theme, axis.text.size, title.text.size, title.opt){

  ## Creation of Values to Plot -----------------------------------------------------

  # If bins = NA, use default
  if(is.na(bins)){
    bins <- 30
  }

  # Create a data frame withthe residuals
  model_values <- data.frame(Residual = resid)

  # Steps to make sure any huge outliers are not cut off
  if (min(model_values$Residual) < -4 * sd(model_values$Residual)){
    min_x <- NA
  } else{
    min_x <- -4 * sd(model_values$Residual)
  }
  if (max(model_values$Residual) > 4 * sd(model_values$Residual)){
    max_x <- NA
  } else{
    max_x <- 4*sd(model_values$Residual)
  }

  ## Creation of Plot ---------------------------------------------------------------

  # Create the histogram of residuals
  if (is.na(min_x) & is.na(max_x)){

    # Data is outside of 4*sd, so xlim is not used
    plot <- ggplot(model_values, aes_string(x = "Residual")) +
      geom_histogram(aes_string(y = "after_stat(density)", fill = "after_stat(count)"),
                     color = "black", fill = "grey82", bins = bins) +
      stat_function(fun = dnorm, color = "blue",
                    args = list(mean = 0, sd = sd(model_values$Residual))) +
      labs(x = "Residuals", y = "Density")

  } else{

    # Data is not outside of 4*sd, so xlim is used
    plot <- ggplot(model_values, aes_string(x = "Residual")) +
      geom_histogram(aes_string(y = "after_stat(density)", fill = "after_stat(count)"),
                     color = "black", fill = "grey82", bins = bins) +
      stat_function(fun = dnorm, color = "blue",
                    args = list(mean = 0, sd = sd(model_values$Residual))) +
      labs(x = "Residuals", y = "Density") +
        xlim(c(min_x, max_x))

  }

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
  if(title.opt == TRUE){
    plot +
      labs(title =  "Histogram") +
      theme(plot.title = element_text(size = title.text.size, face = "bold"),
            axis.title = element_text(size = axis.text.size))
  } else if (title.opt == FALSE){
    plot + theme(axis.title = element_text(size = axis.text.size))
  }

}
