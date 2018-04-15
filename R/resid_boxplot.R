# Boxplot of Residuals.
#
# Creates a boxplot on the residuals from a model.
#
# @param model Model fit using either lm, glm, lmer, or glmer.
# @return A boxplot of the residuals from the \code{model}.
# @examples
# model <- lm(Volume ~ Girth, data = trees)
# resid_boxplot(model)

resid_boxplot <- function(model, theme, axis.text.size, title.text.size){

  # Return an error if a model is not entered in the function
  if(typeof(model) == "double")
    stop("The updated version of ggResidpanel requires a model to be input to the functions.
         Accepted models currently are lm and glm.")

  # Create a data frame with the residuals
  model_values <- data.frame(resid = resid(model))

  # Create the boxplot of residuals
  plot <- ggplot(model_values, aes(x = " ", y = resid)) +
    geom_boxplot() +
    labs(x = " ", y = "Residuals", title = "Boxplot of Residuals")

  # Add theme to plot
  if (theme == "bw"){
    plot <- plot + theme_bw()
  } else if (theme == "classic"){
    plot <- plot + theme_classic()
  } else if (theme == "gray" | theme == "grey"){
    plot <- plot + theme_grey()
  }

  # Set text size of title and axis lables and return plot
  plot + theme(plot.title = element_text(size = title.text.size, face = "bold"),
               axis.title = element_text(size = axis.text.size))

}
