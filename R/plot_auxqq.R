# Q-Q Plot.

# Creates a Q-Q plot from the input residuals
plot_auxqq <- function(resid, theme, axis.text.size, title.text.size, title.opt, qqline, qqbands, alpha){

  ## Creation of Values to Plot -----------------------------------------------------

  # Create a data frame with the residuals
  model_values <- data.frame(Residual = resid)

  ## Creation of Plot ---------------------------------------------------------------

  # Create the qq plot
  if(qqbands == TRUE){

    # Add bands if requested
    plot <- 
      ggplot(
        data = model_values, 
        mapping = aes(sample = {Residual})
      ) +
      stat_qq_band() +
      stat_qq_point(alpha = alpha) +
      labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

  } else{

    # Don't add bands
    plot <- 
      ggplot(
        data = model_values, 
        mapping = aes(sample = {Residual})
      ) +
      stat_qq_point(alpha = alpha) +
      labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

  }

  # Add a line if requested
  if(qqline == TRUE){
    plot <- plot + stat_qq_line(color = "blue", linewidth = .5)
  }

  # Add theme to plot
  if (theme == "bw"){
    plot <- plot + theme_bw()
  } else if (theme == "classic"){
    plot <- plot + theme_classic()
  } else if (theme == "gray" | theme == "grey"){
    plot <- plot + theme_grey()
  }

  # Set text size of title and axis labels, determine whether to include a title,
  # and return plot
  if(title.opt == TRUE){
    plot +
      labs(title = "Q-Q Plot") +
      theme(plot.title = element_text(size = title.text.size, face = "bold"),
            axis.title = element_text(size = axis.text.size))
  } else if (title.opt == FALSE){
    plot + theme(axis.title = element_text(size = axis.text.size))
  }

}

