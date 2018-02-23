#' Histogram of Residuals.
#'
#' Creates a histogram of the residuals from a model.
#'
#' @param resid Residuals from a model.
#' @export
#' @return A histogram of \code{resid} with a normal density curve overlaid with
#' mean equal to the mean of the residuals and standard deviation equal to the
#' standard deviation of the residuals.
#' @examples
#' model <- lm(Volume ~ Girth, data = trees)
#' resid_hist(model$residuals)


resid_hist <- function(resid, bins = NA){
  if(is.na(bins)){
    bins = 30
    warning("By default, bins = 30 in resid_hist. If needed, specify an appropriate number of bins.")
  }
  resid <- data.frame(resid)
  names(resid) <- "Residual"
  ggplot(resid, aes(Residual)) +
    geom_histogram(aes(y = ..density.., fill = ..count..),
                   color = "black", fill = "grey82", bins = bins) +
    theme_bw() +
    stat_function(fun = dnorm, color = "blue",
                  args = list(mean = 0,
                              sd = sd(resid$Residual))) +
    xlim(c(-4 * sd(resid$Residual), 4 * sd(resid$Residual))) +
    labs(x = "Residuals", y = "Density", title = "Histogram of Residuals") +
    theme(plot.title = element_text(size = 12, face = "bold"),
          axis.title = element_text(size = 10))

}
