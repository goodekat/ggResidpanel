# Residual-Leverage plot.

# Creates a plot of the residuals versus leverage from a model
plot_lev <- function(model, type, theme, axis.text.size, title.text.size, title.opt){

  ## Creation of Values to Plot -----------------------------------------------------

  # Obtain the leverage values
  Leverage = hatvalues(model)

  # Determine if any of the leverage values are equal to 1
  cutoff = 0.999999999
  one_lev = sum(Leverage >= cutoff) > 0

  # Check if constant leverage
  range_lev <- range(subset(Leverage, Leverage < cutoff), na.rm = TRUE)
  const_lev <- all(range_lev == 0) || diff(range_lev) < 1e-10 * mean(subset(Leverage, Leverage < cutoff), na.rm = TRUE)

  if(const_lev){

    # Create the constant leverage plot
    plot_constlev(model = model,
                  type = type,
                  theme = theme,
                  axis.text.size = axis.text.size,
                  title.text.size = title.text.size,
                  title.opt = title.opt)

  } else {

    # Return a warning if any of the leverage values are equal to 1
    if(one_lev){
      warning("Observations with a leverage value of 1 are not included
              in the residuals versus leverage plot.")
    }

    # Create a dataframe with the leverage values
    model_values <- data.frame(Leverage = hatvalues(model))

    # Create a data frame with the leverage values and standardized residuals based
    # on the type of model
    if (class(model)[1] == "lm") {
      if (one_lev){
        model_values$Std_Res = suppressWarnings(helper_resid(model, type = "standardized"))
      } else {
        model_values$Std_Res = helper_resid(model, type = "standardized")
      }
    } else if (class(model)[1] == "glm"){
      if(is.na(type) | type == "deviance" | type == "stand.deviance"){
        if (one_lev){
          model_values$Std_Res = suppressWarnings(helper_resid(model, type = "stand.deviance"))
        } else {
          model_values$Std_Res = helper_resid(model, type = "stand.deviance")
        }
      } else if (type == "pearson" | type == "stand.pearson"){
        if (one_lev){
          model_values$Std_Res = suppressWarnings(helper_resid(model, type = "stand.pearson"))
        } else {
          model_values$Std_Res = helper_resid(model, type = "stand.pearson")
        }
      }
    }

    # Determine the rank of the model
    p <- model$rank

    # Create a sequence of hat values
    hat_seq <- seq.int(min(range_lev[1], range_lev[2]/100),
                       range_lev[2],
                       length.out = 100)

    # Create the limits for the plot
    xlimits <- c(0, max(subset(model_values$Leverage, model_values$Leverage < cutoff), na.rm = TRUE))
    ylimits <- extendrange(range(subset(model_values$Std_Res, model_values$Leverage < cutoff), na.rm = TRUE), f = 0.1)

    # Compute standardized residual locations based on a Cook's D value
    # and the sequence of leverage values
    cooksd_contours <- data.frame(case = rep(c("pos_0.5", "neg_0.5", "pos_1", "neg_1"), each = length(hat_seq)),
                                  hat_seq = rep(hat_seq, 4),
                                  stdres = c(sqrt(0.5 * p * (1 - hat_seq) / hat_seq),
                                             -sqrt(0.5 * p * (1 - hat_seq) / hat_seq),
                                             sqrt(1 * p * (1 - hat_seq) / hat_seq),
                                             -sqrt(1 * p * (1 - hat_seq) / hat_seq)))

    cooksd_contours <- subset(cooksd_contours, cooksd_contours$hat_seq <= xlimits[2] &
                                cooksd_contours$stdres <= ylimits[2] &
                                cooksd_contours$stdres >= ylimits[1])

    ## Creation of Labels -------------------------------------------------------------

    # Call function to return appropriate residual label based on model type
    if(class(model)[1] == "lm"){
      r_label <- helper_label(type = "standardized", model)
    } else if (class(model)[1] == "glm"){
      if(is.na(type) | type == "deviance" | type == "stand.deviance"){
        r_label <- helper_label(type = "stand.deviance", model)
      } else if (type == "pearson" | type == "stand.pearson"){
        r_label <- helper_label(type = "stand.pearson", model)
      }
    }

    # Create labels for plotly
    Data <- helper_plotly_label(model)
    model_values$Data <- Data

    ## Creation of Plot ---------------------------------------------------------------

    # Remove data points with leverage values equal to 1
    model_values <- subset(model_values, model_values$Leverage < cutoff)

    # Create the residual vs. leverage plot
    plot <- ggplot(data = model_values, aes(x = Leverage, y = Std_Res), na.rm = TRUE) +
      labs(x = "Leverage", y = r_label) +
      expand_limits(x = 0) +
      geom_smooth(method = "loess", se = FALSE, color = "red", size = 0.5) +
      geom_point(aes(group = Data)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_vline(xintercept = 0, linetype = "dashed") +
      scale_x_continuous(limits = xlimits) +
      scale_y_continuous(limits = ylimits) +
      geom_text(aes(x = 2.25 * min(model_values$Leverage, na.rm = TRUE),
                    y = 1.1 * min(model_values$Std_Res, na.rm = TRUE)),
                label = "- - - Cook's distance contours", color = "red", size = 3)

    # Add Cook's D lines if they are inside the limits of the plot
    if (dim(cooksd_contours)[1] > 0) {
      plot <- plot +
        geom_line(data = cooksd_contours, aes(x = hat_seq, y = stdres, group = case),
                  color = "red", linetype = "dashed", na.rm = TRUE)
    }

    # Add Cook's D contour line labels if within the range of graph limits
    xlable <- max(model_values$Leverage, na.rm = TRUE)
    ylable_pos_0.5 <- 1.05 * sqrt(0.5 * p * (1 - max(model_values$Leverage, na.rm = TRUE)) /
                                 max(model_values$Leverage, na.rm = TRUE))
    ylable_neg_0.5 <- 1.05 * -sqrt(0.5 * p * (1 - max(model_values$Leverage, na.rm = TRUE)) /
                                max(model_values$Leverage, na.rm = TRUE))
    ylable_pos_1 <- 1.05 * sqrt(1 * p * (1 - max(model_values$Leverage, na.rm = TRUE)) /
                              max(model_values$Leverage, na.rm = TRUE))
    ylable_neg_1 <- 1.05 * -sqrt(1 * p * (1 - max(model_values$Leverage, na.rm = TRUE)) /
                              max(model_values$Leverage, na.rm = TRUE))
    if (ylable_pos_0.5 <= ylimits[2]){
      plot <- plot + geom_text(aes(x = xlable, y = ylable_pos_0.5), label = "0.5", color = "red", size = 3)
    }
    if (ylable_neg_0.5 >= ylimits[1]){
      plot <- plot + geom_text(aes(x = xlable, y = ylable_neg_0.5), label = "0.5", color = "red", size = 3)
    }
    if (ylable_pos_1 <= ylimits[2]){
      plot <- plot + geom_text(aes(x = xlable, y = ylable_pos_1), label = "1", color = "red", size = 3)
    }
    if (ylable_neg_1 >= ylimits[1]){
      plot <- plot + geom_text(aes(x = xlable, y = ylable_neg_1), label = "1", color = "red", size = 3)
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
        labs(title = "Residual vs Leverage") +
        theme(plot.title = element_text(size = title.text.size, face = "bold"),
              axis.title = element_text(size = axis.text.size))
    } else if (title.opt == FALSE){
      plot + theme(axis.title = element_text(size = axis.text.size))
    }

  }

}
