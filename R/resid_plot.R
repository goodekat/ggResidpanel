# Residual Plot.
#
# Creates a residual plot with residuals versus predicted values from a model.
#
# @param model Model fit using either lm, glm, lmer, or glmer.
# @param smoother Indicates whether or not to include a smoother on the residual plot.
# Specify TRUE or FALSE. Default is set to FALSE.
# @return A plot of the residuals versus predicted values from the \code{model}
#  with a horizontal line through 0.
# @examples
# model <- lm(Volume ~ Girth, data = trees)
# resid_plot(model)

resid_plot <- function(model, smoother = FALSE){

  # Return an error if a model is not entered in the function
  if(typeof(model) == "double")
    stop("The updated version of ggResidpanel requires a model to be input to the functions.
         Accepted models currently are lm and glm.")

  # Create a data frame with the residuals and predicted values
  model_values <- data.frame(resid = resid(model),
                             pred = fitted(model))

  # Create the residual plot
  residplot <- ggplot(model_values, aes(x = pred, y = resid)) +
    geom_point() +
    geom_abline(slope = 0, intercept = 0, color = "blue") +
    labs(x = "Predicted Values", y = "Residuals", title = "Residual Plot") +
    theme_bw() +
    theme(plot.title = element_text(size = 12, face = "bold"),
          axis.title = element_text(size = 10))

  # If smoother is set to true, add it to the plot
  if (smoother == TRUE){
   residplot <- residplot +
     geom_smooth(colour = "red", se = FALSE, method = "loess", size = 0.5)
  }

  # Return the plot
  residplot

}
