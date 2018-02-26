#' Panel of Diagnostic Residual Plots.
#'
#' Creates a panel of residual diagnostic plots.
#'
#' @param model Model fit using either lm, glm, lmer, or glmer.
#' @param plots Plots chosen to include in the panel of plots (see details for options)
#' @param bins Number of bins for histogram of the residuals.
#' @param scale Scale of graphs in panel. Takes values in (0,1].
#' @export
#' @importFrom ggplot2 ggplot aes geom_point geom_abline labs theme_bw geom_histogram stat_function xlim geom_boxplot
#' @importFrom cowplot plot_grid
#' @importFrom gridExtra grid.arrange
#' @importFrom MASS stdres
#'
#' @return A panel of residual diagnostic plots containing plots specified.
#' @examples
#' model <- lm(Volume ~ Girth, data = trees)
#' resid_panel(model)

resid_panel <- function(model, plots = "SAS", bins = NA, scale = 1){

  ## Errors --------------------------------------------------------------------

  # Return an error if a model is not entered in the function
  if(typeof(model) == "double")
    stop("The updated version of ggResidpanel requires a model to be input to the functions.
         Accepted models currently are lm and glm.")

  # Return an error if a plots option is not specified correctly
  if("SAS" %in% plots | "R" %in% plots | "all" %in% plots | "residplot" %in% plots |
     "hist" %in% plots | "qq" %in% plots | "boxplot" %in% plots |
     "residlev" %in% plots){
  } else{
    stop("Plots option specified incorretly")
  }

  ## Creation of plots ---------------------------------------------------------

  # Create a residual plot if selected in plots otherwise set as NULL
  if("residplot" %in% plots | "SAS" %in% plots | "R" %in% plots | "all" %in% plots){
    residplot <- resid_plot(model)
  } else{
    residplot <- NULL
  }

  # Create a histogram of the residuals if selected in plots otherwise set as NULL
  if("hist" %in% plots | "SAS" %in% plots | "all" %in% plots){
    hist <- resid_hist(model, bins = bins)
  } else{
    hist <- NULL
  }

  # Create a q-q plot of the residuals if selected in plots otherwise set as NULL
  if("qq" %in% plots | "SAS" %in% plots | "R" %in% plots | "all" %in% plots){
    qq <- resid_qq(model)
  } else{
    qq <- NULL
  }

  # Create a boxplot of the residuals if selected in plots otherwise set as NULL
  if("boxplot" %in% plots | "SAS" %in% plots | "all" %in% plots){
    boxplot <- resid_boxplot(model)
  } else{
    boxplot <- NULL
  }

  # Create a residual-leverage plot if selected in plots otherwise set as NULL
  if("residlev" %in% plots | "R" %in% plots | "all" %in% plots){
    residlev <- resid_lev(model)
  } else{
    residlev <- NULL
  }

  ## Creation of grid of plots -------------------------------------------------

  # If individual plots have been specified, set plots equal to "individual"
  if("SAS" %in% plots | "R" %in% plots | "all" %in% plots){
    plots <- plots
  } else{
    plots <- "individual"
  }

  # Create a grid of plots based on the plots specified
  if(plots == "SAS"){

    # Create grid of SAS plots in resid panel
    plot_grid(residplot, hist, qq, boxplot, scale = scale)

  } else if (plots == "R") {

    # Create grid of R plots
    plot_grid(residplot, qq, residlev, scale = scale)

  } else if (plots == "all") {

    # Create grid of all plots
    plot_grid(residplot, hist, qq, boxplot, residlev, scale = scale)

  } else if (plots == "individual") {

    # Turn the specified plots into a list
    individual_plots <- list(residplot = residplot, hist = hist, qq = qq,
                             boxplot = boxplot, residlev = residlev)

    # Remove the plots which are null
    individual_plots <- individual_plots[-which(sapply(individual_plots, is.null))]

    # Turn the list of plots into a grob
    my_grobs = lapply(individual_plots, ggplotGrob)

    # Create grid of individual plots specified
    grid.arrange(grobs = my_grobs, ncol = 2)

  } else{

    stop("Invalid plots option entered")

  }

}
