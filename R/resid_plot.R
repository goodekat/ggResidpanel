#' Residual Plot.
#'
#' Creates a residual plot with residuals versus predicted values from a model.
#'
#' @param resid Residuals from a model.
#' @param pred Predicted values from a model.
#' @export
#' @return A residual plot of \code{resid} versus \code{pred} with a horizontal line
#' with an intercept of 0.
#' @examples
#' model <- lm(Volume ~ Girth, data = trees)
#' resid_plot(model$residuals, model$fitted.values)

resid_plot <- function(resid, pred){

  # Create a data frame with the residuals and predicted values
  model.values <- data.frame(resid = resid, pred = pred)

  # Create a residual plot
  ggplot(model.values, aes(x = pred, y = resid)) +
    geom_point() +
    geom_abline(slope = 0, intercept = 0) +
    labs(x = "Predicted Values", y = "Residuals", title = "Residual Plot") +
    theme_bw(base_size = 10)
}
