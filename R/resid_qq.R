#' Q-Q Plot.
#'
#' Creates a Q-Q plot on the residuals from a model.
#'
#' @param resid Residuals from a model.
#' @export
#' @return A Q-Q Plot of \code{resid}. The method for creating this Q-Q plot follows that
#' used by SAS:
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
#' resid_qq(model$residuals)

resid_qq <- function(resid){

  #Sort the data.
  Actual_Quantiles <- sort(resid)
  #Create inexes of residuals
  i <- 1:length(Actual_Quantiles)

  #Calculate theoretical normal quantiles
  norm_quant <- qnorm((i - 0.375) / (length(Actual_Quantiles) + 0.25))

  #Enter into data set
  Quantiles <- data.frame(Actual_Quantiles, norm_quant)

  ggplot(Quantiles, aes(norm_quant, Actual_Quantiles))+
    geom_point()+
    theme_bw()+
    labs(x = "Quantiles", y = "Residuals", title = "Q-Q Plot")+
    geom_abline(intercept = mean(Actual_Quantiles), slope = sd(Actual_Quantiles), color = "blue") +
    theme(plot.title = element_text(size = 12, face = "bold"),
          axis.title = element_text(size = 10))

}
