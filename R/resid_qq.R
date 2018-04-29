# Q-Q Plot.

# Creates a Q-Q plot on the residuals from a model
resid_qq <- function(model, type, theme, axis.text.size, title.text.size, title.opt,
                     qqline, qqbands){

  ## Creation of Values to Plot -----------------------------------------------------

  # Compute the residuals and put in a dataframe
  if(is.na(type)){
    model_values <- data.frame(Residual = resid_resid(type = NA, model = model))
  } else{
    model_values <- data.frame(Residual = resid_resid(type = type, model = model))
  }

  ## Creation of Labels -------------------------------------------------------------

  # Call function to return appropriate residual label
  r_label <- resid_label(type, model)

  # Create a title for the plot based on r_label
  title <- paste("Q-Q Plot of", r_label)

  # Create labels for plotly
  Data <- resid_plotly_label(model)
  model_values$Data <- Data

  ## Creation of Plot ---------------------------------------------------------------

  # Create the qq plot
  plot <- ggplot(data = model_values, mapping = aes(sample = Residual, label = Data)) +
    stat_qq_point() +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

  # Add a confidence band if requested
  if(qqbands == TRUE){
    plot <- plot + stat_qq_band()
  }

  # Add a line if requested
  if(qqline == TRUE){
    plot <- plot + stat_qq_line(color = "blue", size = .5)
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
      labs(title = title) +
      theme(plot.title = element_text(size = title.text.size, face = "bold"),
            axis.title = element_text(size = axis.text.size))
  } else if (title.opt == FALSE){
    plot + theme(axis.title = element_text(size = axis.text.size))
  }

}

