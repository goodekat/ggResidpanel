# Residual Plot.

# Creates a residual plot with residuals versus predicted values from a model
resid_plot <- function(model, type, smoother, theme, axis.text.size, title.text.size, title.opt){

  ## Creation of Values to Plot -----------------------------------------------------

  # Create a data frame with the residuals
  if(is.na(type)){
    model_values <- data.frame(Residual = resid_resid(type = NA, model = model),
                               Prediction = fitted(model))
  }else{
    model_values <- data.frame(Residual = resid_resid(type = type, model = model),
                               Prediction = fitted(model))
  }

  # Compute the values for the lowess curve
  model_values$Lowess.x <- lowess(x = model_values$Prediction, y = model_values$Residual)$x
  model_values$Lowess.y <- lowess(x = model_values$Prediction, y = model_values$Residual)$y

  ## Creation of Labels -------------------------------------------------------------

  # Call function to return appropriate residual label
  r_label <- resid_label(type, model)

  # Create a title for the plot based on r_label
  title <- paste(r_label, "Plot")

  # Create labels for plotly
  Data <- resid_plotly_label(model)

  ## Creation of Plot ---------------------------------------------------------------

  # Create the residual plot
  plot <- ggplot(data = model_values, aes(x = Prediction, y = Residual, label = Data)) +
    geom_point() +
    geom_abline(slope = 0, intercept = 0, color = "blue") +
    labs(x = "Predicted Values", y = r_label)

  # If smoother is set to true, add it to the plot
  if (smoother == TRUE){
   plot <- plot +
     geom_line(aes(Lowess.x, Lowess.y), colour = "red", size = 0.5)
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
