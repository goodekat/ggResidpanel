# Cook's D Plot.

# Creates a plot with the Cook's D values versus the observation number
plot_cookd <- function(
    model,
    theme,
    axis.text.size,
    title.text.size,
    title.opt,
    alpha) {
  
  ## Creation of Values to Plot -----------------------------------------------
  
  # Create a data frame with the Cook's D values and the observation numbers
  model_values <- 
    data.frame(
      CooksD = round(cooks.distance(model), 3),
      Obs = 1:length(resid(model))
    )
  
  # Create the cutoff SAS uses with Cook's D
  cutoff_cookd <- 4 / length(resid(model))
  
  # Obtain the leverage values
  Leverage <- hatvalues(model)
  
  # Determine if any of the leverage values are equal to 1
  cutoff <- 0.999999999
  one_lev <- sum(Leverage >= cutoff) > 0
  
  # IDs for observations over cutoff:
  IDs <- which(Leverage >= cutoff)
  
  # Alternative cutoff that could be used with Cook's D
  # k <- length(model$coefficients)-1
  # cutoff <- qf(.2, k+1, length(model$residuals)-k-1)
  
  ## Creation of Labels -------------------------------------------------------
  
  # Create labels for plotly
  Data <- helper_plotly_label(model)
  
  ## Creation of Plot ---------------------------------------------------------
  
  # Create the Cook's D plot
  # Return a warning if any of the leverage values are equal to 1 and add
  # lines to plot
  if (one_lev) {
    warning(
      "Observation(s) with a leverage value of 1 are present. Point(s)
            have infinite Cook's D."
    )
    plot <-
      ggplot(
        data = model_values,
        mapping = aes(
          x = .data$Obs,
          y = .data$CooksD,
          color = .data$CooksD,
          label = Data
        )
      ) +
      geom_point(alpha = min(alpha * 2, 1)) +
      scale_color_gradient2(
        low = "#F0E442",
        mid = "#56B4E9",
        high = "#D55E00",
        na.value = "#D55E00",
        limits = c(0, 1),
        midpoint = cutoff_cookd,
        breaks = sort(c(0, cutoff_cookd, 0.5, 1)),
        labels = c(0, "4/n", 0.5, "\u2265 1")
      ) + #Wrong label for rare case of n < 8
      geom_segment(
        mapping = aes(xend = .data$Obs, yend = 0), 
        color = "blue"
      ) +
      labs(
        x = "Observation Number", 
        y = "Cook's D", 
        colour = "Cook's D"
      ) +
      geom_hline(
        yintercept = cutoff_cookd,
        colour = "blue",
        linetype = 5
      ) +
      geom_hline(
        yintercept = 0, 
        colour = "black"
      ) +
      geom_vline(
        xintercept = IDs,
        colour = "#D55E00",
        linetype = 2
      )
  } else{
    plot <-
      ggplot(
        data = model_values,
        mapping = aes(
          x = .data$Obs,
          y = .data$CooksD,
          color = .data$CooksD,
          label = Data
        )
      ) +
      geom_point(alpha = min(alpha * 2, 1)) +
      scale_color_gradient2(
        low = "#F0E442",
        mid = "#56B4E9",
        high = "#D55E00",
        na.value = "#D55E00",
        limits = c(0, 1),
        midpoint = cutoff_cookd,
        breaks = sort(c(0, cutoff_cookd, 0.5, 1)),
        labels = c(0, "4/n", 0.5, "\u2265 1")
      ) + # Wrong label for rare case of n < 8
      geom_segment(
        mapping = aes(xend = .data$Obs, yend = 0), 
        color = "blue"
      ) +
      labs(
        x = "Observation Number", 
        y = "Cook's D"
      ) +
      geom_hline(
        yintercept = cutoff_cookd,
        colour = "blue",
        linetype = 5
      ) +
      geom_hline(
        yintercept = 0, 
        colour = "black"
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
  
  # Set text size of title and axis labels, determine whether to include a
  # title, and return plot without Cook's D color legend (not needed since
  # plotting Cook's D)
  if (title.opt == TRUE) {
    plot +
      labs(title = "Cook's D versus Index Plot") +
      theme(
        plot.title = element_text(size = title.text.size, face = "bold"),
        axis.title = element_text(size = axis.text.size),
        legend.pos = "none"
      )
  } else if (title.opt == FALSE) {
    plot +
      theme(
        axis.title = element_text(size = axis.text.size),
        legend.pos = "none"
      )
  }
  
}
