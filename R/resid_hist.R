#' Histogram of Residuals.
#'
#' Creates a histogram of the residuals from a model.
#'
#' @param model Model fit using either lm, glm, lmer, or glmer.
#' @export
#' @return A histogram of the residuals from the \code{model} with a normal
#' density curve overlaid with mean equal to the mean of the residuals and
#' standard deviation equal to the standard deviation of the residuals.
#' @examples
#' model <- lm(Volume ~ Girth, data = trees)
#' resid_hist(model)

resid_hist <- function(model, bins = NA){

  # If bins is not specified, set to 30 and return a warning message
  # letting the user know this.
  if(is.na(bins)){
    bins = 30
    warning("By default, bins = 30 in the histogram of residuals. If needed, specify an appropriate number of bins.")
  }

  # Create a data frame with the residuals
  model_values <- data.frame(resid = resid(model))

  # Create the histogram of residuals
  ggplot(model_values, aes(x = resid)) +
    geom_histogram(aes(y = ..density.., fill = ..count..),
                   color = "black", fill = "grey82", bins = bins) +
    theme_bw() +
    stat_function(fun = dnorm, color = "blue",
                  args = list(mean = 0,
                              sd = sd(model_values$resid))) +
    xlim(c(-4 * sd(model_values$resid), 4 * sd(model_values$resid))) +
    labs(x = "Residuals", y = "Density", title = "Histogram of Residuals") +
    theme(plot.title = element_text(size = 12, face = "bold"),
          axis.title = element_text(size = 10))

}
