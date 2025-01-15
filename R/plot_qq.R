# Q-Q Plot.

# Creates a Q-Q plot on the residuals from a model
plot_qq <- function(model, type, theme, axis.text.size, title.text.size, title.opt,
                     qqline, qqbands, alpha){

  ## Creation of Values to Plot -----------------------------------------------------

  # Compute the residuals and put in a dataframe
  if(is.na(type)){
    model_values <- data.frame(Residual = helper_resid(type = NA, model = model))
  } else{
    model_values <- data.frame(Residual = helper_resid(type = type, model = model))
  }

  ## Creation of Labels -------------------------------------------------------------

  # Call function to return appropriate residual label
  r_label <- helper_label(type, model)

  # Create a title for the plot based on r_label
  #title <- paste("Q-Q Plot of", r_label)

  # Create labels for plotly
  data_add <- helper_plotly_label(model)
  model_values <- cbind(model_values, data_add)
  names(model_values)[which(names(model_values)=="data_add")] <- "Data"

  ## Creation of Plot ---------------------------------------------------------------
  model_values <- model_values[order(model_values$Residual),]
  plot <- 
    ggplot(
      data = model_values,
      mapping = aes(sample = {Residual}, label = {Data})
    ) +
    stat_qq_point(alpha = alpha ) +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

  plot_data <- ggplot_build(plot)
  model_values$Theoretical <- plot_data[[1]][[1]]$theoretical

  #Residual for using for qq: 'Residual' is for label
  model_values$Residual_Plot <- model_values$Residual
  # Create the qq plot
  if(qqbands == TRUE){

    # Add bands if requested
    plot <- 
      ggplot(
        data = model_values,
        mapping = aes(sample = {Residual_Plot}, label = {Data})
      ) +
      stat_qq_band() +
      stat_qq_point(alpha = alpha) +
      labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
      coord_fixed()

  } else {

    # Don't add bands
    plot <- 
      ggplot(
        data = model_values,
        mapping = aes(sample = {Residual_Plot}, label = {Data})
      ) +
      stat_qq_point(alpha = alpha)+
      geom_point(
        mapping = aes(x = {Theoretical}, y = {Residual}), 
        alpha = alpha
      ) +
      labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
      coord_fixed()

  }

  # Add a line if requested
  if (qqline == TRUE) {
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
  if (title.opt == TRUE) {
    plot +
      labs(title = "Q-Q Plot") +
      theme(plot.title = element_text(size = title.text.size, face = "bold"),
            axis.title = element_text(size = axis.text.size))
  } else if (title.opt == FALSE){
    plot + theme(axis.title = element_text(size = axis.text.size))
  }

}

