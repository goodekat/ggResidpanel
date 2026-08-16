# Residual vs Index Plot.

# Creates a residual plot with the input residuals and predicted values
plot_auxindex <- function(
    resid,
    smoother,
    theme,
    axis.text.size,
    title.text.size,
    title.opt,
    alpha
  ) {
  
  ## Creation of Values to Plot -----------------------------------------------------
  
  # Create a data frame with the residuals
  model_values <- data.frame(Residual = resid)
  
  # Create a variable for observation number
  model_values$Observation <- 1:length(model_values$Residual)
  
  ## Creation of Plot ---------------------------------------------------------------
  
  # Create the residual plot
  plot <-
    ggplot(
      data = model_values,
      mapping = aes(
        x = .data$Observation, 
        y = .data$Residual
      )
    ) +
    geom_point(alpha = alpha) +
    geom_abline(
      slope = 0,
      intercept = 0,
      color = "blue"
    ) +
    labs(
      x = "Observation Number",
      y = "Residuals"
    )
  
  # If smoother is set to true, add it to the plot
  if (smoother == TRUE) {
    plot <- plot +
      geom_smooth(
        method = "loess",
        se = FALSE,
        color = "red",
        linewidth = 0.5,
        formula = 'y ~ x'
      )
  }
  
  # Add theme to plot
  if (theme == "bw") {
    plot <- plot + theme_bw()
  } else if (theme == "classic") {
    plot <- plot + theme_classic()
  } else if (theme == "gray" | theme == "grey") {
    plot <- plot + theme_grey()
  }
  
  # Set text size of title and axis labels, determine whether to include a title,
  # and return plot
  if (title.opt == TRUE) {
    plot +
      labs(title = "Index Plot") +
      theme(
        plot.title = element_text(size = title.text.size, face = "bold"),
        axis.title = element_text(size = axis.text.size)
      )
  } else if (title.opt == FALSE) {
    plot + theme(axis.title = element_text(size = axis.text.size))
  }
  
}
