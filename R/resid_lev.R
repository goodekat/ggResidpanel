# Residual-Leverage plot.
#
# Creates a plot of the residuals versus leverage from a model.
#
# @param model Model fit using lm.
# @return A plot of residuals versus leverage values from the \code{model}.
# @examples
# model <- lm(Volume ~ Girth, data = trees)
# residlev_plot(model)

resid_lev <- function(model, theme="bw", axis.text.size=12, title.text.size=12, title=TRUE){



  # Create a data frame with the leverage values and standardized residuals
  model_values <- data.frame(leverage = hatvalues(model),
                             std_res = stdres(model))
  #stdres: linear model
  #resid(model)/(summary(model)$sigma*sqrt(1-hatvalues(model)))


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

  # Create the residual vs. leverage plot
  plot <- ggplot(model_values, aes(x = leverage, y = std_res)) +
    geom_point() +
    labs(x = "Leverage", y = "Standardized Residuals") +
    expand_limits(x = 0) +
    geom_smooth(color = "red", se = FALSE, method = 'loess', size = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_x_continuous(limits = c(0, max(model_values$leverage, na.rm = TRUE))) +
    scale_y_continuous(limits = extendrange(range(model_values$std_res, na.rm = TRUE), f = 0.08)) +
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

  # Set text size of title and axis lables, determine whether to include a title, and return plot
  if(title == TRUE){
    plot +
      labs(title = "Residuals vs Leverage") +
      theme(plot.title = element_text(size = title.text.size, face = "bold"),
            axis.title = element_text(size = axis.text.size))
  } else if (title == FALSE){
    plot + theme(axis.title = element_text(size = axis.text.size))
  }

}
