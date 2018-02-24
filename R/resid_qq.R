#' Q-Q Plot.
#'
#' Creates a Q-Q plot on the residuals from a model.
#'
#' @param model Model fit using either lm, glm, lmer, or glmer.
#' @export
#' @return A Q-Q Plot of the residuals from the \code{model}. The method for
#' creating this Q-Q plot follows that used by SAS:
#'
#' \url{http://support.sas.com/documentation/cdl/en/sgug/59902/HTML/default/viewer.htm#fit_sect51.htm#sg_fit_fitresidualnormalquantiles}
#'
#' After sorting the residuals in ascending order, for each of the ith ordered residuals,
#' the following quantile is computed:
#'
#' \deqn{\Phi^{-1} * ((i - 0.375) / (n + 0.25))}
#'
#' Each pair of points is then plotted to create the Q-Q plot. The line drawn on the
#' plot has an intercept equal to the mean of the residuals and a slope equal to the
#' standard deviation of the residuals. Data that is approximately normal will fall
#' very close to or on the line.
#'
#' @examples
#' model <- lm(Volume ~ Girth, data = trees)
#' resid_qq(model)

resid_qq <- function(model){

  # Sort the residuals from the model
  actual_quantiles <- sort(resid(model))

  # Create indexes of residuals
  i <- 1:length(actual_quantiles)

  # Calculate theoretical normal quantiles
  normal_quantiles <- qnorm((i - 0.375) / (length(actual_quantiles) + 0.25))

  # Enter into data frame
  quantiles <- data.frame(actual_quantiles, normal_quantiles)

  # Create the normal quantile plot
  ggplot(quantiles, aes(x = normal_quantiles, y = actual_quantiles))+
    geom_point()+
    theme_bw()+
    labs(x = "Quantiles", y = "Residuals", title = "Q-Q Plot")+
    geom_abline(intercept = mean(actual_quantiles), slope = sd(actual_quantiles), color = "blue") +
    theme(plot.title = element_text(size = 12, face = "bold"),
          axis.title = element_text(size = 10))

}
