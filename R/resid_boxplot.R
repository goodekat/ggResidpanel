# Boxplot of Residuals.
#
# Creates a boxplot on the residuals from a model.
#
# @param model Model fit using either lm, glm, lmer, or glmer.
# @param type The user may specify a type of residuals to use, otherwise the default residual type for each model is used.
# @return A boxplot of the residuals from the \code{model}.
# @examples
# model <- lm(Volume ~ Girth, data = trees)
# resid_boxplot(model)

# Boxplot of Residuals.
#
# Creates a boxplot on the residuals from a model.

resid_boxplot <- function(model, theme="bw", axis.text.size=12, title.text.size=12, title=TRUE,type=NA){


  #call function to return appropriate residual label
  r_label <- resid_label(type, model)
  # Create a data frame with the residuals
  if(is.na(type)){
    model_values <- data.frame(resid = resid(model))
  }else{
    model_values <- data.frame(resid = resid(model), type=type)
  }
  model_values$Observation <- 1:nrow(model_values)

  Default_Title <- paste("Boxplot of", r_label)
  # Create the boxplot of residuals
  plot <- ggplot(model_values, aes(x = " ", y = resid)) +
    geom_boxplot() +
    labs(x = " ", y = r_label) +
    theme(plot.title = element_text(size = 12, face = "bold"),
          axis.title = element_text(size = 10))
    labs(x = " ", y = "Residuals")

  # Add theme to plot

    if (theme == "bw"){
    plot <- plot + theme_bw()
  } else if (theme == "classic"){
    plot <- plot + theme_classic()
  } else if (theme == "gray" | theme == "grey"){
    plot <- plot + theme_grey()
  }

  # Set text size of title and axis lables, determine whether to include a title, and return plot
  if (title == TRUE){
    plot +
      labs(title = Default_Title) +
      theme(plot.title = element_text(size = title.text.size, face = "bold"),
                 axis.title = element_text(size = axis.text.size))
  } else if (title == FALSE){
    plot + theme(axis.title = element_text(size = axis.text.size))
  }


}
