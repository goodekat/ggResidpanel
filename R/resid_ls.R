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

resid_ls <- function(model, theme, axis.text.size, title.text.size){

  # Return an error if a model is not entered in the function
  if(typeof(model) == "double")
    stop("The updated version of ggResidpanel requires a model to be input to the functions.
         Accepted models currently are lm and glm.")

  if(!(class(model)[1] %in% c("lm", "glm")))
    stop("Accepted models currently are lm and glm.")

  # Create a data frame with the square root of the standardized residuals and predicted values
  model_values <- data.frame(sqr_stand_resid = sqrt(abs(rstandard(model))),
                             pred = fitted(model))

  # Create the location-scale plot
  plot <- ggplot(model_values, aes(x = pred, y = sqr_stand_resid)) +
    geom_point() +
    labs(x = "Predicted Values", y = expression(sqrt(abs("Standardized Residuals"))),
         title = "Scale-Location") +
    expand_limits(y = 0) +
    geom_smooth(colour = "red", se = FALSE, method = "loess", size = 0.5)

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

