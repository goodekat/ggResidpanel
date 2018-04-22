# Response versus predicted plot.
#
# Creates a plot of the response variable versus the predicted values.

resid_respred <- function(model,theme="bw", axis.text.size=12, title.text.size=12, title=TRUE){


  # Create a data frame with the predicted values and response variable
  model_values <- data.frame(pred = fitted(model),
                             response = model.frame(model)[[1]])
  #model_values$line <- model_values$pred
  # Create the plot of response variable versus predicted values
  plot <- ggplot(model_values, aes(x = pred, y = response)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, color = "blue") +
    labs(x = "Predicted Values", y = names(model.frame(model)[1]))


  # plot <- ggplot(model_values, aes(x = pred, y = response)) +
  #   geom_point() +
  #   geom_line(aes(pred, line), color = "blue") +
  #   labs(x = "Predicted Values", y = names(model.frame(model)[1]))
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
      labs(title = "Response vs Predicted") +
      theme(plot.title = element_text(size = title.text.size, face = "bold"),
            axis.title = element_text(size = axis.text.size))
  } else if (title == FALSE){
    plot + theme(axis.title = element_text(size = axis.text.size))
  }

}
