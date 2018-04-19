# Cook's D Plot.
#
# Creates a Cook's D plot with the Cook's D values versus the observation number.
#
# @param model Model fit using either lm or glm.
# @return A plot of the Cook's D values versus observation numbers from the \code{model}.
# The Horizontal line represents a cut-off to identify highly influential points. The horizontal line is
# placed at 4/n where n is the number of data points used in the \code{model}.
# @examples
# model <- lm(Volume ~ Girth, data = trees)
# resid_cookd(model)

resid_cookd <- function(model, theme="bw", axis.text.size=12, title.text.size=12, title=TRUE){

  # Return an error if a model is not entered in the function
  if(typeof(model) == "double")
    stop("The updated version of ggResidpanel requires a model to be input to the functions.
         Accepted models currently are lm, glm, lmere, and glmer.")

  if(!(class(model)[1] %in% c("lm", "glm")))
    stop("Accepted models currently are lm and glm.")

  # Create a data frame with the cook's d values and the observation numbers
  model_values <- data.frame(cooksd = cooks.distance(model),
                             obs = 1:length(resid(model)))

  #Alternative
  #k <- length(model$coefficients)-1
  #cutoff <- qf(.2, k+1, length(model$residuals)-k-1)
  #SAS
  cutoff <- 4/length(resid(model))

  # Create the Cook's D plot
  plot <- ggplot(model_values, aes(x = obs, y = cooksd)) +
    geom_point() +
    geom_segment(aes(xend = obs, yend = 0), color = "blue") +
    labs(x = "Observation", y = "COOK's D") +
    geom_hline(yintercept = cutoff, colour = "blue", linetype = 5) +
    geom_hline(yintercept = 0, colour = "black")


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
      labs(title = "COOK's D") +
      theme(plot.title = element_text(size = title.text.size, face = "bold"),
                 axis.title = element_text(size = axis.text.size))
  } else if (title == FALSE){
    plot + theme(axis.title = element_text(size = axis.text.size))
  }

}
