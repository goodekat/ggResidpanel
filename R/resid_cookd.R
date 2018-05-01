# Cook's D Plot.

# Creates a plot with the Cook's D values versus the observation number
resid_cookd <- function(model, theme, axis.text.size, title.text.size, title.opt){

  ## Creation of Values to Plot -----------------------------------------------------

  # Create a data frame with the Cook's D values and the observation numbers
  model_values <- data.frame(CooksD = round(cooks.distance(model), 3),
                             Obs = 1:length(resid(model)))

  # Create the cutoff SAS uses with Cook's D
  cutoff <- 4 / length(resid(model))

  # Alternative cutoff that could be used with Cook's D
  # k <- length(model$coefficients)-1
  # cutoff <- qf(.2, k+1, length(model$residuals)-k-1)

  ## Creation of Labels -------------------------------------------------------------

  # Create labels for plotly
  Data <- helper_plotly_label(model)

  ## Creation of Plot ---------------------------------------------------------------

  # Create the Cook's D plot
  plot <- ggplot(model_values, aes(x = Obs, y = CooksD, label = Data)) +
    geom_point() +
    geom_segment(aes(xend = Obs, yend = 0), color = "blue") +
    labs(x = "Observation", y = "COOK's D") +
    geom_hline(yintercept = cutoff, colour = "blue", linetype = 5) +
    geom_hline(yintercept = 0, colour = "black")

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
      labs(title = "COOK's D Plot") +
      theme(plot.title = element_text(size = title.text.size, face = "bold"),
                 axis.title = element_text(size = axis.text.size))
  } else if (title.opt == FALSE){
    plot + theme(axis.title = element_text(size = axis.text.size))
  }

}
