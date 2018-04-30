#' Panel of Diagnostic Residual Plots.
#'
#' Creates a panel of residual diagnostic plots given inputs of residuals and fitted values.
#'
#' @param resid The residuals from the model.
#' @param pred The fitted values from the model.
#' @param plots Plots chosen to include in the panel of plots. (See details for options.)
#' @param bins Number of bins for histogram of the residuals.
#' @param smoother Indicates whether or not to include a smoother on the residual plot.
#' Specify TRUE or FALSE. Default is set to FALSE.
#' @param qqline Indicates whether to include a 1-1 line on the qq-plot. Specify TRUE or
#' FALSE. Default is set to TRUE.
#' @param qqbands Indicates whether to include confidence bands on the qq-plot. Specify
#' TRUE or FALSE. Default is set to FALSE.
#' @param scale Scales the size of the graphs in a panel. Takes values in (0,1].
#' @param theme ggplot2 theme to be used. Options are \code{"bw"}, \code{"classic"}, and
#' \code{"grey"} (or \code{"gray"}). Default is \code{"bw"}.
#' @param axis.text.size Specifies the size of the text for the axis labels of all plots.
#' @param title.text.size Specifies the size of the text for the titles of all plots.
#' @param title.opt Indicates whether or not to include a title on the plots.
#' Specify TRUE or FALSE. Default is set to TRUE.
#' @param ind.ncol Sets the number of columns in the panel when more than one individual plot
#' has been specified. Default is set to 2 columns.
#'
#' @export
#'
#' @details The following grid options can be chosen for the \code{plots} argument.
#' \itemize{
#'   \item "SAS": This is the default option. It creates a panel of a residual plot,
#'   a normal quantile plot of the residuals, a histogram of the residuals, and a
#'   boxplot of the residuals. This was modeled after the residpanel option in proc
#'   mixed from SAS version 9.4.
#'   \item A vector of individual plots can also be specified. For example, one can
#'   specify \code{plots = c("boxplot", "hist")} or \code{plots = "qq"}. The individual
#'   plot options are as follows.
#'   \itemize{
#'     \item \code{"boxplot"}: A boxplot of residuals.
#'     \item \code{"hist"}: A histogram of residuals.
#'     \item \code{"qq"}: A normal quantile plot of residuals.
#'     \item \code{"resid"}: A plot of residuals versus predicted values.
#'   }
#' }
#'
#' Details on the creation of the plots can be found in the details section of the help
#' file for \code{resid_panel}.
#'
#' @return A panel of residual diagnostic plots containing plots specified.
#'
#' @examples
#' # Fit a linear regression model to predict the volume of a tree based on the
#' # girth of a tree using the R "trees" data
#' lm_model1 <- lm(Volume ~ Girth, data = trees)
#'
#' # Plot the residuals using the default panel
#' resid_auxpanel(resid(lm_model1), fitted(lm_model1), bins = 30)
#'
#' # Fit a linear model to compare the weights of plants bewteen different
#' # treatment groups using the R "PlantGrowth" data
#' lm_model2 <- lm(weight ~ group, data = PlantGrowth)
#'
#' # Create a grid of the residual plot and qq-plot
#' resid_auxpanel(resid(lm_model2), fitted(lm_model2), plots = c("resid", "qq"))
#'
#' # Fit a generalized linear regression model using a Poisson family to compare
#' # the insect counts between different sprays from the R "InsectSprays" data
#' glm_model <- glm(count ~ spray, family = "poisson", data = InsectSprays)
#'
#' # Create the SAS panel of plots without titles, with a grey theme, and with a
#' # smaller scaling of the plots
#' resid_auxpanel(resid(glm_model), fitted(glm_model), title.opt = F, theme = "grey",
#'                bins = 30, scale = 0.9)


resid_auxpanel <- function(residuals, predicted, plots = "SAS", bins = NA,
                           smoother = FALSE, qqline = TRUE, qqbands = FALSE,
                           scale = 1, theme = "bw", axis.text.size = 10,
                           title.text.size = 12, title.opt = TRUE, ind.ncol = 2){

  ## Errors and Warnings -------------------------------------------------------

  # Return an error if a model is input into the function
  if (class(residuals)[1] %in% c("lm", "glm", "lmerMod", "glmerMod")){
    stop("'resid_auxpanel' recieves the residuals and fitted values. Please use
         'resid_panel' to input a model.")
  }

  # Return a warning if the smoother option is not specified correctly
  if(smoother == TRUE | smoother == FALSE){
  } else{
    smoother <- FALSE
    warning("The smoother option for residual plot not was specified correctly.
            The default option will be used. Accepted options are TRUE or FALSE.")
  }

  # Return a warning if the theme is not specified correctly
  if(theme == "bw" | theme == "classic" | theme == "grey" | theme == "gray"){
  } else{
    theme <- "bw"
    warning("The theme option was not specified correctly. The default theme
            will be used. Accepted themes are 'bw', 'classic', and 'grey' (or 'gray').")
  }

  # Return a warning if the title option is not specified correctly
  if(title.opt == TRUE | title.opt == FALSE){
  } else{
    title.opt <- TRUE
    warning("The title option was not specified correctly. The default title
            option will be used. Accepted options are TRUE or FALSE.")
  }

  # Return a warning about choosing number of bins if a histogram is included
  if("SAS" %in% plots | "hist" %in% plots){
    if(is.na(bins)){
      bins = 30
      warning("By default, bins = 30 in the histogram of residuals. If necessary, specify
              an appropriate number of bins.")
    }
  }

  ## Creation of plots ---------------------------------------------------------

  # Create a boxplot of the residuals if selected in plots otherwise set as NULL
  if("boxplot" %in% plots | "SAS" %in% plots){
    boxplot <- resid_auxboxplot(residuals,
                                theme = theme,
                                axis.text.size = axis.text.size,
                                title.text.size = title.text.size,
                                title.opt = title.opt)
  } else{
    boxplot <- NULL
  }

  # Create a histogram of the residuals if selected in plots otherwise set as NULL
  if("hist" %in% plots | "SAS" %in% plots){
    hist <- resid_auxhist(residuals,
                          bins = bins,
                          theme = theme,
                          axis.text.size = axis.text.size,
                          title.text.size = title.text.size,
                          title.opt = title.opt)
  } else{
    hist <- NULL
  }

  # Create a q-q plot of the residuals if selected in plots otherwise set as NULL
  if("qq" %in% plots | "SAS" %in% plots){
    qq <- resid_auxqq(residuals,
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
  if("resid" %in% plots | "SAS" %in% plots){
    resid <- resid_auxplot(residuals,
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
  if("SAS" %in% plots){
    plots <- plots
  } else if("boxplot" %in% plots | "hist" %in% plots | "qq" %in% plots |
            "resid" %in% plots){
    plots <- "individual"
  } else{
    stop("Invalid plots option entered")
  }

  # Create a grid of plots based on the plots specified
  if(plots == "SAS"){

    # Create grid of SAS plots in resid panel
    plot_grid(resid, hist, qq, boxplot, scale = scale)

  } else if (plots == "individual") {

    # Turn the specified plots into a list
    individual_plots <- list(resid = resid, hist = hist,
                             qq = qq, boxplot = boxplot)

    # Remove the plots which are null
    individual_plots <- individual_plots[-which(sapply(individual_plots, is.null))]

    # Turn the list of plots into a grob
    my_grobs = lapply(individual_plots, ggplotGrob)

    # Specify number of columns for grid of plots based on number of plots specified
    ifelse(length(individual_plots) == 1, grid_col <- 1, grid_col <- ind.ncol)

    # Create grid of individual plots specified
    grid.arrange(grobs = my_grobs, ncol = grid_col, scale = scale)

  }

}
