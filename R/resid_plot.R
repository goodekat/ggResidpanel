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

resid_plot <- function(model, type=NA, smoother = FALSE, theme="bw", axis.text.size=12, title.text.size=12, title=TRUE){

  # Return an error if a model is not entered in the function
  if(typeof(model) == "double")
    stop("The updated version of ggResidpanel requires a model to be input to the functions.
         Accepted models currently are lm,glm, lmer, and glmer.")

  r_label <- resid_label(type, model)
  Default_Title <- paste(r_label,"Plot")
  # Create a data frame with the residuals
  if(is.na(type)){
    model_values <- data.frame(resid = resid(model),
                               pred = fitted(model))
  }else{
    model_values <- data.frame(resid = resid(model,type=type),
                               pred = fitted(model))
  }

  # Create a data frame with the residuals and predicted values


  # Create the residual plot
  plot <- ggplot(model_values, aes(x = pred, y = resid)) +
    geom_point() +
    geom_abline(slope = 0, intercept = 0, color = "blue") +
    labs(x = "Predicted Values", y = r_label)

  # If smoother is set to true, add it to the plot
  if (smoother == TRUE){
   plot <- plot +
     geom_smooth(colour = "red", se = FALSE, method = "loess", size = 0.5)
  }

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
      labs(title = Default_Title) +
      theme(plot.title = element_text(size = title.text.size, face = "bold"),
            axis.title = element_text(size = axis.text.size))
  } else if (title == FALSE){
    plot + theme(axis.title = element_text(size = axis.text.size))
  }

}
