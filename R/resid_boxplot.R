# Boxplot of Residuals.

# Function for creating a boxplot of the residuals
resid_boxplot <- function(model, type, theme, axis.text.size, title.text.size, title.opt){

  ## Creation of Values to Plot -----------------------------------------------------

  # Create a data frame with the residuals
  if(is.na(type)){
    model_values <- data.frame(Residual = helper_resid(type = NA, model = model))
  } else{
    model_values <- data.frame(Residual = helper_resid(type = type, model = model))
  }

  # Add an observation variable
  model_values$Observation <- 1:nrow(model_values)

  ## Creation of Labels -------------------------------------------------------------

  # Call function to return appropriate residual label
  r_label <- helper_label(type = type, model = model)

  # Create a title for the plot based on r_label
  #title <- paste("Boxplot of", r_label)

  # Create labels for plotly
  Data <- helper_plotly_label(model)

  ## Creation of Plot ---------------------------------------------------------------

  # Create the boxplot of residuals
  plot <- ggplot(model_values, aes(x = " ", y = Residual, label = Data)) +
    geom_boxplot(width = .5) +
    geom_point(alpha = 0) +
    labs(x = " ", y = r_label)

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
