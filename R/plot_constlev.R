# Residual-Leverage plot.

# Creates a plot of the residuals versus leverage from a model
plot_constlev <- function(model, type, theme, axis.text.size, title.text.size, title.opt,
                          alpha = alpha){

  ## Creation of Values to Plot -----------------------------------------------------

  # Create a data frame with the factor values and standardized residuals based
  # on the type of model

  model_summary <- summary(model)

  n_factor_variables <- length(names(model$xlevels))

  #Add first factor variable
  all_factors <- model$model[[2]]
  #if there are more than 1 factor variables, add them
  if(n_factor_variables>1){
    for(i in 3:(n_factor_variables+1))
    all_factors <- paste(all_factors, model$model[[i]], sep = ":")
  }

  # Create a data frame with the all_factors variable
  model_values <- data.frame(Variables = all_factors)

  # Add the standardized residuals to the plot
  if(class(model)[1] == "lm"){
    if (sum(hatvalues(model) == 1) > 0) {
      model_values$Std_Res = suppressWarnings(helper_resid(model, type = "standardized"))
    } else {
      model_values$Std_Res = helper_resid(model, type = "standardized")
    }
  } else if (class(model)[1] == "glm"){
    if(is.na(type) | type == "response" | type == "deviance" | type == "stand.deviance"){
      if (sum(hatvalues(model) == 1) > 0) {
        model_values$Std_Res = suppressWarnings(helper_resid(model, type = "stand.deviance"))
      } else {
        model_values$Std_Res = helper_resid(model, type = "stand.deviance")
      }
    } else if (type == "pearson" | type == "stand.pearson"){
      if (sum(hatvalues(model) == 1) > 0) {
        model_values$Std_Res = suppressWarnings(helper_resid(model, type = "stand.pearson"))
      } else {
        model_values$Std_Res =  helper_resid(model, type = "stand.pearson")
      }
    }
  }

  # Compute the values for the lowess curve #Removed 
  #model_values$Lowess.x <- lowess(x = model_values$Variables, y = model_values$Std_Res)$x
  #model_values$Lowess.y <- lowess(x = model_values$Variables, y = model_values$Std_Res)$y

  ## Creation of Labels -------------------------------------------------------------

  # Call function to return appropriate residual label based on model type
  if(class(model)[1] == "lm"){
    r_label <- helper_label(type = "standardized", model)
  } else if (class(model)[1] == "glm"){
    if(is.na(type) | type == "response" | type == "deviance" | type == "stand.deviance"){
      r_label <- helper_label(type = "stand.deviance", model)
    } else if (type == "pearson" | type == "stand.pearson"){
      r_label <- helper_label(type = "stand.pearson", model)
    }
  }

  # Create labels for plotly
  Data <- helper_plotly_label(model)
  model_values$Data <- Data

  ## Creation of Plot ---------------------------------------------------------------
  # Create the constant leverage plot

  plot <- 
    ggplot(
      data = model_values, 
      mapping = aes(x = {Variables}, y = {Std_Res}), 
      na.rm = TRUE
    ) +
    geom_point(aes(group = {Data}), alpha = alpha) +
    #geom_line(aes(x = {Lowess.x}, y = {Lowess.y}), color = "red", linewidth = 0.5) +
    geom_abline(slope = 0, intercept = 0, color = "blue", linewidth = 0.5) +
    xlab("Factor Level Combinations") +
    ylab("Standardized Residuals")

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
  if(title.opt == TRUE){
    plot +
      labs(title = "Constant Leverage Plot") +
      theme(plot.title = element_text(size = title.text.size, face = "bold"),
            axis.title = element_text(size = axis.text.size))
  } else if (title.opt == FALSE){
    plot + theme(axis.title = element_text(size = axis.text.size))
  }

}
