# Response versus predicted plot.

# Creates a plot of the response variable versus the predicted values
plot_yvp <- function(model, theme, axis.text.size, title.text.size, title.opt, alpha){

  ## Creation of Values to Plot -----------------------------------------------------

  # Create a data frame with the predicted values and response variable
  # based on the model type
  if(class(model)[1] == "glm"){
    if(model$family[[1]] == "binomial"){
      model_values <- data.frame(Predicted = fitted(model),
                                 Response = helper_glm_actual(model))
      names(model_values) <- c("Predicted", "Response")
      y_label <- "Actual Proportions"
    } else{
      model_values <- data.frame(Predicted = fitted(model),
                                 Response = model.frame(model)[[1]])
      y_label <- names(model.frame(model)[1])
    }
  } else if (class(model)[1] == "glmerMod"){
    if(model@resp$family[[1]] == "binomial"){
      model_values <- data.frame(Predicted = fitted(model),
                                 Response = helper_glm_actual(model))
      names(model_values) <- c("Predicted", "Response")
      y_label <- "Actual Proportions"

    } else{
      model_values <- data.frame(Predicted = fitted(model),
                                 Response = model.frame(model)[[1]])
      y_label <- names(model.frame(model)[1])
    }
  } else if (class(model)[1] == "lme") {
    model_values <- data.frame(Predicted = fitted(model),
                               Response = model$data[[1]])
    y_label <- names(model$data[1])
  } else{
    model_values <- data.frame(Predicted = fitted(model),
                               Response = model.frame(model)[[1]])
    y_label <- names(model.frame(model)[1])
  }

  ## Creation of Labels -------------------------------------------------------------

  # Create labels for plotly
  Data <- helper_plotly_label(model)

  ## Creation of Plot ---------------------------------------------------------------


  # Create the plot of response variable versus predicted values
  plot <- ggplot(data = model_values,
                 mapping = aes_string(x = "Predicted", y = "Response", label = "Data")) +
    geom_point(alpha = alpha) +
    geom_abline(slope = 1, intercept = 0, color = "blue") +
    labs(x = "Predicted Values", y = y_label)

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
      labs(title = "Response vs Predicted") +
      theme(plot.title = element_text(size = title.text.size, face = "bold"),
            axis.title = element_text(size = axis.text.size))
  } else if (title.opt == FALSE){
    plot + theme(axis.title = element_text(size = axis.text.size))
  }

}
