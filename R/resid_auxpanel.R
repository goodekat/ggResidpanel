#' Panel of Diagnostic Residual Plots.
#'
#' Creates a panel of residual diagnostic plots given inputs of residuals and
#' fitted values.
#'
#' @param residuals The residuals from the model.
#' @param predicted The fitted values from the model.
#' @param plots Plots chosen to include in the panel of plots. The default panel
#'   includes a residual plot, a normal quantile plot, an index plot,
#'   and a histogram of the residuals. (See details for the options available.)
#' @param bins Number of bins for histogram of the residuals. Default is set to 30.
#' @param smoother Indicates whether or not to include a smoother on the
#'   residual plot and/or index plot. Specify TRUE or FALSE. Default is set to FALSE.
#' @param qqline Indicates whether to include a 1-1 line on the qq-plot. Specify
#'   TRUE or FALSE. Default is set to TRUE.
#' @param qqbands Indicates whether to include confidence bands on the qq-plot.
#'   Specify TRUE or FALSE. Default is set to FALSE.
#' @param scale Scales the size of the graphs in a panel. Takes values in (0,1].
#' @param theme ggplot2 theme to be used. Options are \code{"bw"},
#'   \code{"classic"}, and \code{"grey"} (or \code{"gray"}). Default is
#'   \code{"bw"}.
#' @param axis.text.size Specifies the size of the text for the axis labels of
#'   all plots.
#' @param title.text.size Specifies the size of the text for the titles of all
#'   plots.
#' @param title.opt Indicates whether or not to include a title on the plots.
#'   Specify TRUE or FALSE. Default is set to TRUE.
#' @param nrow Sets the number of rows in the panel.
#'
#' @export resid_auxpanel
#'
#' @details The following grid options can be chosen for the \code{plots}
#'   argument.
#'   \itemize{
#'   \item "all": This creates a panel of all plot types included in the package
#'   that are available for \code{resid_auxpanel}. (See plot descriptions under
#'   individual options.)
#'   \item "default": This creates a panel with a residual plot, a normal quantile plot
#'   of the residuals, an index plot of the residuals, and a histogram of the residuals.
#'   \item "SAS": This creates a panel of a residual plot, a normal quantile plot of the
#'   residuals, a histogram of the residuals, and a boxplot of the residuals. This was
#'   modeled after the residpanel option in proc mixed from SAS version 9.4.
#'   \item A vector of individual plots can also be specified. For example, one
#'   can specify \code{plots = c("boxplot", "hist")} or \code{plots = "qq"}. The
#'   individual plot options are as follows.
#'   \itemize{
#'   \item \code{"boxplot"}: A boxplot of residuals
#'   \item \code{"hist"}: A histogram of residuals
#'   \item \code{"index"}: A plot of residuals versus observation number
#'   \item \code{"qq"}: A normal quantile plot of residuals
#'   \item \code{"resid"}: A plot of residuals versus predicted values
#'   } }
#'
#'   Details on the creation of the plots can be found in the details section of
#'   the help file for \code{resid_panel}.
#'
#' @return A panel of residual diagnostic plots containing plots specified.
#'
#' @examples
#'
#' # Fit a random forest model to the penguins data
#' rf_model <- randomForest::randomForest(x = penguins[,2:3], y = penguins[,1])
#'
#' # Obtain the predictions from the model on the observed data
#' rf_pred <- predict(rf_model, penguins[,2:3])
#'
#' # Obtain the residuals from the model
#' rf_resid <- penguins[,1] - rf_pred
#'
#' # Create a panel with the residual and index plot
#' resid_auxpanel(rf_resid, rf_pred, plots = c("resid", "index"), theme = "classic")

resid_auxpanel <- function(residuals, predicted, plots = "default", bins = 30,
                           smoother = FALSE, qqline = TRUE, qqbands = FALSE,
                           scale = 1, theme = "bw", axis.text.size = 10,
                           title.text.size = 12, title.opt = TRUE, nrow = NULL){

  ## Errors and Warnings -------------------------------------------------------

  # Return an error if a model is input into the function
  if (class(residuals)[1] %in% c("lm", "glm", "lmerMod", "lmerModLmerTest", "glmerMod")){
    stop("'resid_auxpanel' recieves the residuals and fitted values. Please use
         'resid_panel' to input a model.")
  }

  # Checks that return a warning
  smoother <- check_smoother(smoother = smoother)
  theme <- check_theme(theme = theme)
  title.opt <- check_title(title.opt = title.opt)

  ## Creation of plots ---------------------------------------------------------

  # Create a boxplot of the residuals if selected in plots otherwise set as NULL
  if("boxplot" %in% plots | "SAS" %in% plots | "all" %in% plots){
    boxplot <- plot_auxboxplot(residuals,
                               theme = theme,
                               axis.text.size = axis.text.size,
                               title.text.size = title.text.size,
                               title.opt = title.opt)
  } else{
    boxplot <- NULL
  }

  # Create a histogram of the residuals if selected in plots otherwise set as NULL
  if("hist" %in% plots | "default" %in% plots | "SAS" %in% plots | "all" %in% plots){
    hist <- plot_auxhist(residuals,
                         bins = bins,
                         theme = theme,
                         axis.text.size = axis.text.size,
                         title.text.size = title.text.size,
                         title.opt = title.opt)
  } else{
    hist <- NULL
  }

  # Create an index plot of the residuals if selected in plots otherwise set as NULL
  if("index" %in% plots | "default" %in% plots | "all" %in% plots){
    index <- plot_auxindex(residuals,
                           theme = theme,
                           smoother = smoother,
                           axis.text.size = axis.text.size,
                           title.text.size = title.text.size,
                           title.opt = title.opt)
  } else{
    index <- NULL
  }


  # Create a q-q plot of the residuals if selected in plots otherwise set as NULL
  if("qq" %in% plots | "default" %in% plots | "SAS" %in% plots | "all" %in% plots){
    qq <- plot_auxqq(residuals,
                      theme = theme,
                      axis.text.size = axis.text.size,
                      title.text.size = title.text.size,
                      title.opt = title.opt,
                      qqline = qqline,
                      qqbands = qqbands)
  } else{
    qq <- NULL
  }

  # Create a residual plot if selected in plots otherwise set as NULL
  if("resid" %in% plots | "default" %in% plots | "SAS" %in% plots | "all" %in% plots){
    resid <- plot_auxresid(residuals,
                           predicted,
                           smoother = smoother,
                           theme = theme,
                           axis.text.size = axis.text.size,
                           title.text.size = title.text.size,
                           title.opt = title.opt)
  } else{
    resid <- NULL
  }

  ## Creation of grid of plots -------------------------------------------------

  # If individual plots have been specified, set plots equal to "individual"
  # Return an error if none of the correct plot options have been specified
  if("default" %in% plots | "SAS" %in% plots | "all" %in% plots){
    plots <- plots
  } else if("boxplot" %in% plots | "hist" %in% plots | "index" %in% plots |
            "qq" %in% plots | "resid" %in% plots){
    chosen <- plots
    plots <- "individual"
  } else{
    stop("Invalid plots option entered")
  }

  # Create a grid of plots based on the plots specified
  if (plots == "default"){

    # Create grid of default plots
    plot_grid(resid, qq, index, hist,
              scale = scale, nrow = nrow)

  } else if (plots == "SAS"){

    # Create grid of SAS plots
    plot_grid(resid, hist, qq, boxplot,
              scale = scale, nrow = nrow)

  } else if (plots == "all"){

    # Create grid of all plots
    plot_grid(resid, qq, hist, index, boxplot,
              scale = scale, nrow = nrow)

  } else if (plots == "individual") {

    # Turn the specified plots into a list
    individual_plots <- list(resid = resid,
                             hist = hist,
                             index = index,
                             qq = qq,
                             boxplot = boxplot)

    # Select the chosen plots
    individual_plots <- individual_plots[chosen]

    # Create grid of individual plots specified
    plot_grid(plotlist = individual_plots, scale = scale, nrow = nrow)

  }

}
