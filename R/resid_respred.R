# Response versus predicted plot.
#
# Creates a plot of the response variable versus the predicted values.

resid_respred <- function(model, theme, axis.text.size, title.text.size){

  # Create a data frame with the predicted values and response variable
  model_values <- data.frame(pred = fitted(model),
                             response = model.frame(model)[[1]])

  # Create the plot of response variable versus predicted values
  plot <- ggplot(model_values, aes(x = pred, y = response)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, color = "blue") +
    labs(x = "Predicted Values", y = names(model.frame(model)[1]),
         title = "Response vs Predicted")

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
