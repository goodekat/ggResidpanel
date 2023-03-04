# Location-Scale Plot.

# Creates a location-scale plot with the square root of the standardized residuals
# versus predicted values from a model
plot_ls <- function(model, type, smoother, theme, axis.text.size, title.text.size, title.opt, alpha){

  ## Creation of Values to Plot -----------------------------------------------------

  # Create a data frame with the square root of the standardized residuals and
  # predicted values based on the model type
  if(class(model)[1] == "lm"){
    model_values <- data.frame(Sqrt_Std_Res = sqrt(abs(helper_resid(type = "standardized",
                                                                   model = model))),
                               Prediction = fitted(model))
  } else if (class(model)[1] == "glm"){
    if(is.na(type) | type == "response" | type == "deviance" | type == "stand.deviance"){
      model_values <- data.frame(Sqrt_Std_Res = sqrt(abs(helper_resid(type = "stand.deviance",
                                                                     model = model))),
                                 Prediction = fitted(model))

    } else if (type == "pearson" | type == "stand.pearson"){
      model_values <- data.frame(Sqrt_Std_Res = sqrt(abs(helper_resid(type = "stand.pearson",
                                                                     model = model))),
                                 Prediction = fitted(model))
    }
  }

  # Compute the values for the lowess curve: old version used lowess, new version using loess
  # model_values$Lowess.x <- lowess(x = model_values$Prediction, y = model_values$Sqrt_Std_Res)$x
  # model_values$Lowess.y <- lowess(x = model_values$Prediction, y = model_values$Sqrt_Std_Res)$y

  ## Creation of Labels -------------------------------------------------------------

  # Create labels for plotly
  Data <- helper_plotly_label(model)

  ## Creation of Plot ---------------------------------------------------------------

  # Create the location-scale plot - labels are adjusted based on the model type
  if (class(model)[1] == "lm"){

    # Location-scale plot for lm model
    plot <- ggplot(data = model_values,
                   mapping = aes_string(x = "Prediction", y = "Sqrt_Std_Res", label = "Data")) +
      geom_point(alpha = alpha) +
      labs(x = "Predicted Values", y = expression(sqrt(abs(" Standardized Residuals ")))) +
      expand_limits(y = 0)

  } else if (class(model)[1] == "glm"){

    # Location-scale plot for glm model with deviance residuals
    if(is.na(type) | type == "response" | type == "deviance" | type == "stand.deviance"){
      plot <- ggplot(data = model_values,
                     mapping = aes_string(x = "Prediction", y = "Sqrt_Std_Res", label = "Data")) +
        geom_point(alpha = alpha) +
        labs(x = "Predicted Values",
             y = expression(sqrt(abs(" Standardized Deviance Residuals ")))) +
        expand_limits(y = 0)

    # Location-scale plot for glm model with Pearson residuals
    } else if(type == "pearson" | type == "stand.pearson"){
      plot <- ggplot(data = model_values,
                     mapping = aes_string(x = "Prediction", y = "Sqrt_Std_Res", label = "Data")) +
        geom_point(alpha = alpha) +
        labs(x = "Predicted Values",
             y = expression(sqrt(abs(" Standardized Pearson Residuals ")))) +
        expand_limits(y = 0)

    }

  }

  # If smoother is set to true, add it to the plot
  if (smoother == TRUE){
    plot <- plot +
      geom_smooth(method = "loess", se = FALSE, color = "red", size = 0.5)
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
      labs(title = "Location-Scale Plot") +
      theme(plot.title = element_text(size = title.text.size, face = "bold"),
            axis.title = element_text(size = axis.text.size))
  } else if (title.opt == FALSE){
    plot + theme(axis.title = element_text(size = axis.text.size))
  }

}

