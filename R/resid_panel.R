#' Panel of Diagnostic Residual Plots.
#'
#' Creates a panel of residual diagnostic plots.
#'
#' @param resid Residuals from a model.
#' @param pred Predicted values from a model.
#' @export
#' @importFrom ggplot2 ggplot aes geom_point geom_abline labs theme_bw geom_histogram stat_function xlim geom_boxplot
#' @importFrom cowplot plot_grid
#'
#' @return A panel of residual diagnostic plots including a residual plot, a histogram of the
#' residuals, a qqplot of the residuals, and a boxplot of the residuals.
#' @examples
#' model <- lm(Volume ~ Girth, data = trees)
#' resid_panel(resid(model), fitted(model))
#'

resid_panel <- function(resid, pred, bins = NA){

  # Create a data frame with the residuals and predicted values
  model.values <- data.frame(resid = resid, pred = pred)

  # Create a residual plot
  resid.plot <- resid_plot(resid, pred)

  # Create a histogram of the residuals
  if(is.na(bins)){
    bins = 30
    warning("By default, bins = 30 in resid_hist. If needed, specify an appropriate number of bins.")
  }
  resid.hist <- resid_hist(resid, bins = bins)

  # Create a q-q plot of the residuals
  resid.qq <- resid_qq(resid)

  # Create a boxplot of the residuals
  resid.boxplot <- resid_boxplot(resid)

  # Create a grid of the plots
  plot_grid(resid.plot, resid.hist, resid.qq, resid.boxplot, ncol = 2, nrow = 2)
}
