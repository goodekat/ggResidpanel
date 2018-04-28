# Scale-Location Plot.
#
# Creates a scale-location plot with the square root of the standardized residuals versus predicted values from a model.
#
# @param model Model fit using either lm or glm.
# @return A plot of the square roote of the standardized residuals versus predicted values from the \code{model}
#  with smooth curve fit using "loess".
# @examples
# model <- lm(Volume ~ Girth, data = trees)
# resid_ls(model)

resid_ls <- function(model, type,theme, axis.text.size, title.text.size, title.opt){



  # Create a data frame with the square root of the standardized residuals and predicted values
  if(class(model)[1]=="lm"){
    model_values <- data.frame(Sqrt_Std_Res = sqrt(abs(resid_resid(type="standardized", model=model))),
                               Prediction = fitted(model))

  }else if (class(model)[1]=="glm"){
    if(is.na(type)|type=="deviance"|type=="stand.deviance"){
      model_values <- data.frame(Sqrt_Std_Res = sqrt(abs(resid_resid(type="stand.deviance", model=model))),
                                 Prediction = fitted(model))

    }else if (type=="pearson"|type=="stand.pearson"){
      model_values <- data.frame(Sqrt_Std_Res = sqrt(abs(resid_resid(type="stand.pearson", model=model))),
                                 Prediction = fitted(model))
     }
  }
  model_values$Lowess.x <- lowess(x=model_values$Prediction, y=model_values$Sqrt_Std_Res)$x
  model_values$Lowess.y <- lowess(x=model_values$Prediction, y=model_values$Sqrt_Std_Res)$y


  Data <- resid_plotly_label(model)

  if (class(model)[1]=="lm"){
    plot <- ggplot(model_values, aes(x = Prediction, y = Sqrt_Std_Res,label=Data)) +
      geom_point() +
      labs(x = "Predicted Values", y = expression(sqrt(abs(" Standardized Residuals  ")))) +
      expand_limits(y = 0) +
      geom_line(aes(Lowess.x, Lowess.y),colour = "red", size = 0.5)

  }else if (class(model)[1]=="glm"){
    if(is.na(type)|type=="deviance"|type=="stand.deviance"){
    plot <- ggplot(model_values, aes(x = Prediction, y = Sqrt_Std_Res,label=Data)) +
      geom_point() +
      labs(x = "Predicted Values", y = expression(sqrt(abs(" Standardized Deviance Residuals  ")))) +
      expand_limits(y = 0) +
      geom_line(aes(Lowess.x, Lowess.y),colour = "red", size = 0.5)
    }else if(type=="pearson"|type=="stand.pearson"){
      plot <- ggplot(model_values, aes(x = Prediction, y = Sqrt_Std_Res,label=Data)) +
        geom_point() +
        labs(x = "Predicted Values", y = expression(sqrt(abs(" Standardized Pearson Residuals  ")))) +
        expand_limits(y = 0) +
        geom_line(aes(Lowess.x, Lowess.y),colour = "red", size = 0.5)
    }
  }

    # Create the location-scale plot


  # Add theme to plot
  if (theme == "bw"){
    plot <- plot + theme_bw()
  } else if (theme == "classic"){
    plot <- plot + theme_classic()
  } else if (theme == "gray" | theme == "grey"){
    plot <- plot + theme_grey()
  }

  # Set text size of title and axis lables, determine whether to include a title, and return plot
  if(title.opt == TRUE){
    plot +
      labs(title = "Scale-Location") +
      theme(plot.title = element_text(size = title.text.size, face = "bold"),
            axis.title = element_text(size = axis.text.size))
  } else if (title.opt == FALSE){
    plot + theme(axis.title = element_text(size = axis.text.size))
  }

}

