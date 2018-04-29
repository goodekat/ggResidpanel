# Residual-Leverage plot.

# Creates a plot of the residuals versus leverage from a model
resid_lev <- function(model, type, theme, axis.text.size, title.text.size, title.opt){

  ## Creation of Values to Plot -----------------------------------------------------

  # Create a data frame with the leverage values and standardized residuals based
  # on the type of model
  if(class(model)[1] == "lm"){
      model_values <- data.frame(Leverage = hatvalues(model),
                                 Std_Res = resid_resid(model, type = "standardized"))
  } else if (class(model)[1] == "glm"){
    if(is.na(type) | type == "deviance" | type == "stand.deviance"){
      model_values <- data.frame(Leverage = hatvalues(model),
                                 Std_Res = resid_resid(model, type = "stand.deviance"))
    } else if (type == "pearson" | type == "stand.pearson"){
      model_values <- data.frame(Leverage = hatvalues(model),
                                 Std_Res = resid_resid(model, type = "stand.pearson"))
    }
  }

  # Compute the values for the lowess curve
  model_values$Lowess.x <- lowess(x = model_values$Leverage, y = model_values$Std_Res)$x
  model_values$Lowess.y <- lowess(x = model_values$Leverage, y = model_values$Std_Res)$y

  # Compute the hat matrix values
  hii <- (infl <- influence(model, do.coef = FALSE))$hat

  # Determine the range of the hat matrix values
  r.hat <- range(hii, na.rm = TRUE)

  # Determine the rank of the model
  p <- model$rank

  # Create a sequence of hat values
  usr <- par("usr")
  hh <- seq.int(min(r.hat[1L], r.hat[2L]/100), usr[2L], length.out = 101)

  # Compute Cook's D values based on the sequence of hat values for two levels
  cl_h1 <- data.frame(hh = hh,
                      pos = sqrt(0.5 * p * (1 - hh) / hh),
                      neg = -sqrt(0.5 * p * (1 - hh) / hh))
  cl_h2 <- data.frame(hh = hh,
                      pos = sqrt(1 * p * (1 - hh) / hh),
                      neg = -sqrt(1 * p * (1 - hh) / hh))

  ## Creation of Labels -------------------------------------------------------------

  # Call function to return appropriate residual label based on model type
  if(class(model)[1] == "lm"){
    r_label <- resid_label(type = "standardized", model)
  } else if (class(model)[1] == "glm"){
    if(is.na(type) | type == "deviance" | type == "stand.deviance"){
      r_label <- resid_label(type = "stand.deviance", model)
    } else if (type == "pearson" | type == "stand.pearson"){
      r_label <- resid_label(type = "stand.pearson", model)
    }
  }

  # Create a title for the plot based on r_label
  title <- paste(r_label, "vs Leverage")

  # Create labels for plotly
  Data <- resid_plotly_label(model)
  model_values$Data <- Data

  ## Creation of Plot ---------------------------------------------------------------

  # Create the residual vs. leverage plot
  plot <- ggplot(data = model_values, aes(x = Leverage, y = Std_Res), na.rm = TRUE) +
    geom_point(aes(group = Data)) +
    labs(x = "Leverage", y = r_label) +
    expand_limits(x = 0) +
    geom_line(aes(Lowess.x, Lowess.y),color = "red", size = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_x_continuous(limits = c(0, max(model_values$Leverage, na.rm = TRUE))) +
    scale_y_continuous(limits = extendrange(range(model_values$Std_Res, na.rm = TRUE), f = 0.08))+
    geom_line(data = data.frame(cl_h1), aes(x = hh, y = pos), linetype = "dashed", color = "red", na.rm = TRUE) +
    geom_line(data = data.frame(cl_h1), aes(x = hh, y = neg), linetype = "dashed", color = "red", na.rm = TRUE) +
    geom_line(data = data.frame(cl_h2), aes(x = hh, y = pos), linetype = "dashed", color = "red", na.rm = TRUE) +
    geom_line(data = data.frame(cl_h2), aes(x = hh, y = neg), linetype = "dashed", color = "red", na.rm = TRUE)

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
      labs(title = title) +
      theme(plot.title = element_text(size = title.text.size, face = "bold"),
            axis.title = element_text(size = axis.text.size))
  } else if (title.opt == FALSE){
    plot + theme(axis.title = element_text(size = axis.text.size))
  }

}
