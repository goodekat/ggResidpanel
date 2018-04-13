#' Panel of Diagnostic Residual Plots.
#'
#' Creates a panel of residual diagnostic plots.
#'
#' @param model Model fit using either \code{lm}, \code{glm}, \code{lmer}, or \code{glmer}.
#' @param plots Plots chosen to include in the panel of plots. (See details for options.)
#' @param bins Number of bins for histogram of the residuals.
#' @param scale Scale of graphs in panel. Takes values in (0,1].
#' @param type
#' @param smoother Indicates whether or not to include a smoother on the residual plot.
#' Specify TRUE or FALSE. Default is set to FALSE.
#' @param theme
#' @export
#' @importFrom ggplot2 ggplot aes geom_point geom_abline labs theme_bw theme geom_histogram
#' stat_function xlim geom_boxplot expand_limits geom_smooth element_text ggplotGrob geom_vline
#' theme_classic geom_hline geom_segment geom_line scale_x_continuous scale_y_continuous
#' @importFrom cowplot plot_grid
#' @importFrom gridExtra grid.arrange
#' @importFrom MASS stdres
#' @details The following grid options can be chosen for the \code{plots} argument.
#' \itemize{
#'   \item "all": This creates a panel of all plot types included in the package.
#'   \item "R": This creates a panel of a residual plot, a normal quantile plot of
#'   the residuals, and a leverage versus residuals plot. This was modeled after the
#'   plots shown in R if the \code{plots()} base function is applied to a \code{lm}
#'   model.
#'   \item "SAS": This is the default option. It creates a panel of a residual plot,
#'   a normal quantile plot of the residuals, a histogram of the residuals, and a
#'   boxplot of the residuals. This was modeled after the residpanel option in proc
#'   mixed from SAS version 9.4.
#'   \item A vector of individual plots can also be specified. For example, one can
#'   specify \code{plots = c("boxplot", "hist")} or \code{plots = "qq"}. The individual
#'   plot options are as follows.
#'   \itemize{
#'     \item "boxplot": A boxplot of residuals.
#'     \item "hist": A histogram of residuals.
#'     \item "ls": A location scale plot of the residuals.
#'     \item "qq": A normal quantile plot of residuals.
#'     \item "residlev": A plot of leverage values versus residuals.
#'     \item "residplot": A plot of residuals versus predicted values.
#'   }
#' }
#'
#' @return A panel of residual diagnostic plots containing plots specified.
#' @examples
#' lm_model <- lm(Volume ~ Girth, data = trees)
#' resid_panel(lm_model)
#'
#' library(lme4)
#' d1 <- data.frame(y = rnorm(54, 20, 4), trt = rep(c("A", "B"), each = 27), subject = rep(1:18, each = 3))
#' lmer_model <- lmer(y ~ trt + (1|subject), data = d1)
#' resid_panel(lmer_model, bins = 30)

resid_panel <- function(model, plots = "SAS", bins = NA, scale = 1,
                        type = NA, smoother = FALSE, theme = NA){

  ## Errors and Warnings -------------------------------------------------------

  # Return an error if a model is not entered in the function
  if(typeof(model) == "double")
    stop("The updated version of ggResidpanel requires a model to be input to the functions.
         Accepted models currently are lm, glm, lmer, and glmer.")

  # Return an error if a plots option is not specified correctly
  if("SAS" %in% plots | "R" %in% plots | "all" %in% plots | "residplot" %in% plots |
     "hist" %in% plots | "qq" %in% plots | "boxplot" %in% plots |
     "residlev" %in% plots | "ls" %in% plots | "cookd" %in% plots | "ls" %in% plots |
     "respred" %in% plots){
  } else{
    stop("Plots option specified incorretly")
  }

  # Return an error if a smoother option is not specified correctly
  if(smoother == TRUE | smoother == FALSE){
  }else{
    stop("Smoother option for residual plot not specified correctly. Choose either TRUE or FALSE.")
  }

  # Return a warning about choosing number of bins if a histogram is included
  if("SAS" %in% plots | "all" %in% plots | "hist" %in% plots){
    if(is.na(bins)){
      bins = 30
      warning("By default, bins = 30 in the histogram of residuals. If necessary, specify an appropriate number of bins.")
    }
  }

  ## Creation of plots ---------------------------------------------------------

  # Create a boxplot of the residuals if selected in plots otherwise set as NULL
  if("boxplot" %in% plots | "SAS" %in% plots | "all" %in% plots){
    boxplot <- resid_boxplot(model)
  } else{
    boxplot <- NULL
  }

  # Create a Cook's D plot if selected in plots otherwise set as NULL
  if("cookd" %in% plots | "all" %in% plots){
    cookd <- resid_cookd(model)
  } else{
    cookd <- NULL
  }

  # Create a histogram of the residuals if selected in plots otherwise set as NULL
  if("hist" %in% plots | "SAS" %in% plots | "all" %in% plots){
    hist <- resid_hist(model, bins = bins)
  } else{
    hist <- NULL
  }

  # Create a location-scale plot if selected in plots otherwise set as NULL
  if("ls" %in% plots | "R" %in% plots | "all" %in% plots){
    ls <- resid_ls(model)
  } else{
    ls <- NULL
  }

  # Create a q-q plot of the residuals if selected in plots otherwise set as NULL
  if("qq" %in% plots | "SAS" %in% plots | "R" %in% plots | "all" %in% plots){
    qq <- resid_qq(model)
  } else{
    qq <- NULL
  }

  # Create a residual-leverage plot if selected in plots otherwise set as NULL
  if("residlev" %in% plots | "R" %in% plots | "all" %in% plots){
    residlev <- resid_lev(model)
  } else{
    residlev <- NULL
  }

  # Create a residual plot if selected in plots otherwise set as NULL
  if("residplot" %in% plots | "SAS" %in% plots | "R" %in% plots | "all" %in% plots){
    residplot <- resid_plot(model, smoother)
  } else{
    residplot <- NULL
  }

  # Create a residual plot if selected in plots otherwise set as NULL
  if("respred" %in% plots | "all" %in% plots){
    respred <- resid_respred(model)
  } else{
    respred <- NULL
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
    plot_grid(residplot, qq, ls, residlev, scale = scale)

  } else if (plots == "all") {

    # Create grid of all plots
    plot_grid(residplot, hist, qq, boxplot, residlev, cookd, ls, respred, scale = scale)

  } else if (plots == "individual") {

    # Turn the specified plots into a list
    individual_plots <- list(residplot = residplot, hist = hist, qq = qq,
                             boxplot = boxplot, ls = ls, residlev = residlev,
                             cookd = cookd, respred = respred)

    # Remove the plots which are null
    individual_plots <- individual_plots[-which(sapply(individual_plots, is.null))]

    # Turn the list of plots into a grob
    my_grobs = lapply(individual_plots, ggplotGrob)

    # Specify number of columns for grid of plots based on number of plots specified
    ifelse(length(individual_plots) == 1, grid_col <- 1, grid_col <- 2)

    # Create grid of individual plots specified
    grid.arrange(grobs = my_grobs, ncol = grid_col, scale = scale)

  } else{

    stop("Invalid plots option entered")

  }

}
