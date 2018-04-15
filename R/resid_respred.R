# Response versus predicted plot.
#
# Creates a plot of the response variable versus the predicted values.

resid_respred <- function(model){

  # Create a data frame with the predicted values and response variable
  model_values <- data.frame(pred = fitted(model),
                             response = model.frame(model)[[1]])

  # Create the plot of response variable versus predicted values
    ggplot(model_values, aes(x = pred, y = response)) +
      geom_point() +
      geom_abline(slope = 1, intercept = 0, color = "blue") +
      labs(x = "Predicted Values", y = names(model.frame(model)[1]),
           title = "Response vs Predicted") +
      theme_bw() +
      theme(plot.title = element_text(size = 12, face = "bold"),
            axis.title = element_text(size = 10))

}
