#' Panel of Diagnostic Residual Plots.
#'
#' Creates a panel of residual diagnostic plots.
#'
#' @param model Model fit using either lm, glm, lmer, or glmer.
#' @param bins Number of bins for histogram of the residuals.
#' @export
#' @importFrom ggplot2 ggplot aes geom_point geom_abline labs theme_bw geom_histogram stat_function xlim geom_boxplot
#' @importFrom cowplot plot_grid
#'
#' @return A panel of residual diagnostic plots including a residual plot, a histogram of the
#' residuals, a qqplot of the residuals, and a boxplot of the residuals.
#' @examples
#' model <- lm(Volume ~ Girth, data = trees)
#' resid_panel(model)

resid_panel <- function(model, plots = "SAS", bins = NA, scale = 1){

  # Return an error if a model is not entered in the function
  if(typeof(model) == "double")
    stop("The updated version of ggResidpanel requires a model to be input to the functions.
         Accepted models currently are lm and glm.")

  # Create a residual plot
  residplot <- resid_plot(model)

  # Create a histogram of the residuals
  hist <- resid_hist(model, bins = bins)

  # Create a q-q plot of the residuals
  qq <- resid_qq(model)

  # Create a boxplot of the residuals
  boxplot <- resid_boxplot(model)

  # Create a residual-leverage plot
  residlev <- residlev_plot(model)

  # Create a grid of plots based on the plots requested
  if(plots == "SAS"){
    plot_grid(residplot, hist, qq, boxplot,
              scale = scale)
  }
  else if (plots == "all") {
    plot_grid(residplot, hist, qq, boxplot, residlev,
              scale = scale)
  }
  else{
    stop("Invalid plots option entered")
  }

}
