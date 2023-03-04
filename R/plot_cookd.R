# Cook's D Plot.

# Creates a plot with the Cook's D values versus the observation number
plot_cookd <- function(model, theme, axis.text.size, title.text.size, title.opt, alpha){

  ## Creation of Values to Plot -----------------------------------------------------

  # Create a data frame with the Cook's D values and the observation numbers
  model_values <- data.frame(CooksD = round(cooks.distance(model), 3),
                             Obs = 1:length(resid(model)))

  # Create the cutoff SAS uses with Cook's D
  cutoff <- 4 / length(resid(model))

  # Obtain the leverage values
  Leverage <- hatvalues(model)
  
  # Determine if any of the leverage values are equal to 1
  cutoff <- 0.999999999
  one_lev <- sum(Leverage >= cutoff) > 0
  
  #IDs for observations over cutoff:
  
  IDs <- which(Leverage >= cutoff)
  
  
  
  # Alternative cutoff that could be used with Cook's D
  # k <- length(model$coefficients)-1
  # cutoff <- qf(.2, k+1, length(model$residuals)-k-1)

  ## Creation of Labels -------------------------------------------------------------

  # Create labels for plotly
  Data <- helper_plotly_label(model)

  ## Creation of Plot ---------------------------------------------------------------

  # Create the Cook's D plot
  # Return a warning if any of the leverage values are equal to 1 and add lines to plot
  if(one_lev){
    warning("Observation(s) with a leverage value of 1 are present. Point(s) have
            infinite Cook's D.")
    plot <- ggplot(model_values, aes_string(x = "Obs", y = "CooksD", label = "Data")) +
      geom_point(alpha = alpha) +
      geom_segment(aes_string(xend = "Obs", yend = 0), color = "blue") +
      labs(x = "Observation Number", y = "Cook's D") +
      geom_hline(yintercept = cutoff, colour = "blue", linetype = 5) +
      geom_hline(yintercept = 0, colour = "black") +
      geom_vline(xintercept = IDs, colour = "red", linetype = 2)
  } else{
  plot <- ggplot(model_values, aes_string(x = "Obs", y = "CooksD", label = "Data")) +
    geom_point(alpha = alpha) +
    geom_segment(aes_string(xend = "Obs", yend = 0), color = "blue") +
    labs(x = "Observation Number", y = "Cook's D") +
    geom_hline(yintercept = cutoff, colour = "blue", linetype = 5) +
    geom_hline(yintercept = 0, colour = "black")
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
      labs(title = "Cook's D versus Index Plot") +
      theme(plot.title = element_text(size = title.text.size, face = "bold"),
                 axis.title = element_text(size = axis.text.size))
  } else if (title.opt == FALSE){
    plot + theme(axis.title = element_text(size = axis.text.size))
  }

}
