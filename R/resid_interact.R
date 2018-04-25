#' Interaction Versions of Residual Diagnostics Plots.
#'
#' Creates interaction versions of all residual diagnostic plots.
#'
#' @param model Model fit using either \code{lm}, \code{glm}, \code{lmer}, or \code{glmer}.
#' @param plots Plots chosen to include in the panel of plots. (See details for options.)
#' @param bins Number of bins for histogram of the residuals.
#' @param type
#' @param smoother Indicates whether or not to include a smoother on the residual plot.
#' Specify TRUE or FALSE. Default is set to FALSE.
#' @param axis.text.size Specifies the size of the text for the axis labels of all plots.
#' @param title.text.size Specifies the size of the text for the titles of all plots.
#' @param theme ggplot2 theme to be used. Options are \code{"bw"}, \code{"classic"}, and
#' \code{"grey"} (or \code{"gray"}). Default is \code{"bw"}.
#' @param title Indicates whether or not to include a title on the plots.
#' Specify TRUE or FALSE. Default is set to TRUE.
#' @export
#' @importFrom plotly ggplotly
#' @details The following grid options can be chosen for the \code{plots} argument.
#' \itemize{
#'   \item "all": This creates a panel of all plot types included in the package.
#'   \item "R": This creates a panel of a residual plot, a normal quantile plot of
#'   the residuals, and a leverage versus residuals plot. This was modeled after the
#'   plots shown in R if the \code{plots()} base function is applied to an \code{lm}
#'   model.
#'   \item "SAS": This is the default option. It creates a panel of a residual plot,
#'   a normal quantile plot of the residuals, a histogram of the residuals, and a
#'   boxplot of the residuals. This was modeled after the residpanel option in proc
#'   mixed from SAS version 9.4.
#'   \item A vector of individual plots can also be specified. For example, one can
#'   specify \code{plots = c("boxplot", "hist")} or \code{plots = "qq"}. The individual
#'   plot options are as follows.
#'   \itemize{
#'     \item \code{"boxplot"}: A boxplot of residuals.
#'     \item \code{"cookd"}: A plot of Cook's D values versus observation number.
#'     \item \code{"hist"}: A histogram of residuals.
#'     \item \code{"ls"}: A location scale plot of the residuals.
#'     \item \code{"qq"}: A normal quantile plot of residuals.
#'     \item \code{"residlev"}: A plot of leverage values versus residuals.
#'     \item \code{"residplot"}: A plot of residuals versus predicted values.
#'     \item \code{"respred":}: A plot of the response variable versus the predicted values.
#'   }
#' }
#'
#' @return A panel of residual diagnostic plots containing plots specified.
#' @examples
#' # Fit a linear regression model and plot the residuals using the default panel
#' lm_model <- lm(Volume ~ Girth, data = trees)
#' resid_panel(lm_model, bins = 30)
#'
#' # Fit a generalized linear regression model and plot the residuals using
#' # the default panel
#' glm_model <- glm(Height ~ Girth, family = "poisson", data = trees)
#' resid_panel(glm_model, bins = 30)
#'
#' # Generate normal data, fit a mixed effects model, and plot the residuals
#' # using the default panel
#' library(lme4)
#' d1 <- data.frame(y = rnorm(54, 20, 4), trt = rep(c("A", "B"), each = 27), subject = rep(1:18, each = 3))
#' lmer_model <- lmer(y ~ trt + (1|subject), data = d1)
#' resid_panel(lmer_model, bins = 30)
#'
#' # Generate Poisson data, fit a mixed effects model, and plot the residuals
#' # using the default panel
#' d2 <- data.frame(y = rpois(54, 3), trt = rep(c("A", "B"), each = 27), subject = rep(1:18, each = 3))
#' glmer_model <- glmer(y ~ trt + (1|subject), family = "poisson", data = d2)
#' resid_panel(glmer_model, bins = 30)

resid_interact <- function(model, plots = NA, bins = NA,
                           type = NA, smoother = FALSE, theme = "bw",
                           axis.text.size = 10, title.text.size = 12,
                           title = TRUE, qqline = TRUE){

  ## Errors and Warnings -------------------------------------------------------

  if(is.na(plots) | !(plots %in% c("boxplot", "cookd", "hist", "ls", "qq",
                                   "residlev", "residplot", "respred"))){
    stop("Please specify a plot from the following list: boxplot, cookd, hist, ls, qq, residlev, residplot, respred.")
  }

  # Add error if they requested a type to make sure that type of residuals is
  # available for the model type.
  type <- tolower(type)
  if(!is.na(type)){
    if(class(model)[1] == "lm"){
      if(!(type %in% c("response", "pearson", "standardized"))){
        stop("The requested residual type is not available. Please select from the following
             options for a 'lm' model: response, pearson, or standardized.")
      }
    }else if(class(model)[1] == "glm"){
      if(!(type %in% c("response", "pearson", "deviance", "stand.pearson", "stand.deviance"))){
        stop("The requested residual type is not available. Please select from the following
             options for a 'glm' model: response, pearson, deviance, stand.deviance, or
             stand.pearson.")
      }
    }else if(class(model)[1] == "lmerMod"){
      if(!(type %in% c("response", "pearson"))){
        stop("The requested residual type is not available. Please select from the following
             options for a 'lmer' model: response or pearson.")
      }
    }else if(class(model)[1] == "glmerMod"){
      if(!(type %in% c("response", "pearson", "deviance"))){
        stop("The requested residual type is not available. Please select from the following
             options for a 'glmer' model: response, pearson, or deviance.")
      }
    }
  }

  # Add in error if request plots involving standardized residuals for a 'lmer' or 'glmer' model.
  if(class(model)[1] %in% c("lmerMod", "glmerMod")){
    if("ls" %in% plots |"residlev" %in% plots | "all" %in% plots | "SASextend" %in% plots |
       "R" %in% plots){
      stop("The requested plot or panel uses standardized residuals which are not
           currently available for 'lmer' or 'glmer' models.")
    }
  }

  # Stop and return an error if Cook's D plot is requested for a 'lmer' or 'glmer' model.
  if(class(model)[1] %in% c("lmerMod", "glmerMod")){
    if("cookd" %in% plots){
      stop("The Cook's D plot is unavailable for 'lmer' and 'glmer' models.")
    }
  }

  # Return an error if a model is not entered in the function
  if(typeof(model) == "double")
    stop("The updated version of ggResidpanel requires a model to be input to the functions.
         Accepted models currently are lm, glm, lmer, and glmer.")

  # Return an error if smoother option is not specified correctly
  if(smoother == TRUE | smoother == FALSE){
  }else{
    stop("Smoother option for residual plot not specified correctly.
         Choose either TRUE or FALSE.")
  }

  # Return an error if theme is not specified correctly
  if(theme == "bw" | theme == "classic" | theme == "grey" | theme == "gray"){
  }else{
    theme = "bw"
    warning("Theme option not specified correctly. Accepted themes are bw, classic, and
            grey (or gray). Default theme will be used.")
  }

  # Return an error if smoother option is not specified correctly
  if(title == TRUE | title == FALSE){
  }else{
    stop("Title option not specified correctly. Choose either TRUE or FALSE.")
  }

  # Return a warning about choosing number of bins if a histogram is included
  if("SAS" %in% plots | "all" %in% plots | "hist" %in% plots){
    if(is.na(bins)){
      bins = 30
      warning("By default, bins = 30 in the histogram of residuals. If necessary, specify
              an appropriate number of bins.")
    }
  }


  ## Creation of plots ---------------------------------------------------------

  # Create a boxplot of the residuals if selected in plots otherwise set as NULL
  if(plots == "boxplot"){
    plot_i <- resid_boxplot(model, type = type, theme, axis.text.size, title.text.size, title)
  } else if(plots == "cookd"){
    plot_i <- resid_cookd(model, theme, axis.text.size, title.text.size, title)
  } else if(plots == "hist"){
    plot_i <- resid_hist(model, type = type, bins = bins, theme, axis.text.size, title.text.size, title)
  } else if(plots == "ls"){
    plot_i <- resid_ls(model, type,theme, axis.text.size, title.text.size, title)
  } else if(plots == "qq"){
    plot_i <- resid_qq(model, type = type, theme, axis.text.size, title.text.size, title, qqline, qqbands = FALSE)
  } else if(plots == "residlev"){
    plot_i <- resid_lev(model, type = type, theme, axis.text.size, title.text.size, title)
  } else if(plots == "residplot"){
    plot_i <- resid_plot(model, type = type, smoother, theme, axis.text.size, title.text.size, title)
  } else if(plots == "respred"){
    plot_i <- resid_respred(model, type = type, theme, axis.text.size, title.text.size, title)
  }

  ## Creation of interactive plot -------------------------------------------------

  # Use plotly to plot interactive plot requested
  ggplotly(plot_i, tooltip = "Data")

}


