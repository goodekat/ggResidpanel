#' Interaction Versions of Residual Diagnostics Plots.
#'
#' Creates interaction versions of all residual diagnostic plots.
#'
#' @param model Model fit using either \code{lm}, \code{glm}, \code{lmer}, or \code{glmer}.
#' @param plot Plot chosen to create interactive plot. (See details for options.)
#' @param bins Number of bins for histogram of the residuals.
#' @param type The user may specify a type of residuals to use. Otherwise, the default
#' residual type for each model is used. (See details for options.)
#' @param smoother Indicates whether or not to include a smoother on the residual plot.
#' Specify TRUE or FALSE. Default is set to FALSE.
#' @param axis.text.size Specifies the size of the text for the axis labels of the plot.
#' @param title.text.size Specifies the size of the text for the titles of the plot.
#' @param theme ggplot2 theme to be used. Options are \code{"bw"}, \code{"classic"}, and
#' \code{"grey"} (or \code{"gray"}). Default is \code{"bw"}.
#' @param title.opt Indicates whether or not to include a title on the plot.
#' Specify TRUE or FALSE. Default is set to TRUE.
#' @export
#' @importFrom plotly ggplotly
#' @details The following...
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
#'
#' @return The interactive residual diagnostic plot specified.
#' @examples
#' # Fit a linear regression model and plot the residuals using the default panel
#' lm_model <- lm(Volume ~ Girth, data = trees)
#' resid_interact(lm_model, bins = 30)

resid_interact <- function(model, plot = NA, bins = NA, type = NA,
                           smoother = FALSE, theme = "bw",
                           axis.text.size = 10, title.text.size = 12,
                           title.opt = TRUE, qqline = TRUE){

  ## Errors and Warnings -------------------------------------------------------

  # Print a warning about not using the dev version of ggplot2 with ggplotly
  warning("Some of the interactive plots do not function with the dev version of
          ggplot from github. To achieve full functionallity, please install ggplot2
          from CRAN.")

  # Return an error if an acceptable model type is not entered in the function
  if(!(class(model)[1] %in% c("lm", "glm", "lmerMod", "glmerMod")))
    stop("resid_interact requires a model to be input. Accepted models
         currently are lm, glm, lmer, and glmer.")

  # Return an error if the plot type is not entered correctly
  if(is.na(plot) | !(plot %in% c("boxplot", "cookd", "hist", "ls", "qq",
                                   "residlev", "residplot", "respred"))){
    stop("Invalid plot option entered. See the resid_interact help file for
         available options.")
  }

  # Return an error if the requested residual type is not available for the model type
  type <- tolower(type)
  if(!is.na(type)){
    if(class(model)[1] == "lm"){
      if(!(type %in% c("response", "pearson", "standardized"))){
        stop("The requested residual type is not available. Please select from the following
             options for a 'lm' model: response, pearson, or standardized.")
      }
    } else if(class(model)[1] == "glm"){
      if(!(type %in% c("response", "pearson", "deviance", "stand.pearson", "stand.deviance"))){
        stop("The requested residual type is not available. Please select from the following
             options for a 'glm' model: response, pearson, deviance, stand.deviance, or
             stand.pearson.")
      }
    } else if(class(model)[1] == "lmerMod"){
      if(!(type %in% c("response", "pearson"))){
        stop("The requested residual type is not available. Please select from the following
             options for a 'lmer' model: response or pearson.")
      }
    } else if(class(model)[1] == "glmerMod"){
      if(!(type %in% c("response", "pearson", "deviance"))){
        stop("The requested residual type is not available. Please select from the following
             options for a 'glmer' model: response, pearson, or deviance.")
      }
    }
  }

  # Return an error if the requested plot involves standardizing residuals for an 'lmer' or
  # 'glmer' model
  if(class(model)[1] %in% c("lmerMod", "glmerMod")){
    if("ls" %in% plot |"residlev" %in% plot | "all" %in% plot | "R" %in% plot){
      stop("The requested plot or panel uses standardized residuals, which are not
           currently available for 'lmer' or 'glmer' models.")
    }
  }

  # Return an error if Cook's D plot is requested for an 'lmer' or 'glmer' model
  if(class(model)[1] %in% c("lmerMod", "glmerMod")){
    if("cookd" %in% plot){
      stop("The Cook's D plot is unavailable for 'lmer' and 'glmer' models.")
    }
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
  if("SAS" %in% plot | "all" %in% plot | "hist" %in% plot){
    if(is.na(bins)){
      bins = 30
      warning("By default, bins = 30 in the histogram of residuals. If necessary, specify
              an appropriate number of bins.")
    }
  }

  ## Creation of plot ---------------------------------------------------------

  # Create the requested plot
  if(plot == "boxplot"){

    # Boxplot
    plot_i <- resid_boxplot(model = model,
                            type = type,
                            theme = theme,
                            axis.text.size = axis.text.size,
                            title.text.size = title.text.size,
                            title.opt = title.opt)

  } else if(plot == "cookd"){

    # Cook's D plot
    plot_i <- resid_cookd(model = model,
                          theme = theme,
                          axis.text.size = axis.text.size,
                          title.text.size = title.text.size,
                          title.opt = title.opt)

  } else if(plot == "hist"){

    # Histogram
    plot_i <- resid_hist(model = model,
                         type = type,
                         bins = bins,
                         theme = theme,
                         axis.text.size = axis.text.size,
                         title.text.size = title.text.size,
                         title.opt = title.opt)

  } else if(plot == "ls"){

    # Location-Scale plot
    plot_i <- resid_ls(model = model,
                       type = type,
                       theme = theme,
                       axis.text.size = axis.text.size,
                       title.text.size = title.text.size,
                       title.opt = title.opt)

  } else if(plot == "qq"){

    # QQ plot
    plot_i <- resid_qq(model = model,
                       type = type,
                       theme = theme,
                       axis.text.size = axis.text.size,
                       title.text.size = title.text.size,
                       title.opt = title.opt,
                       qqline = qqline,
                       qqbands = FALSE)

  } else if(plot == "residlev"){

    # Leverage plot
    plot_i <- resid_lev(model = model,
                        type = type,
                        theme = theme,
                        axis.text.size = axis.text.size,
                        title.text.size = title.text.size,
                        title.opt = title.opt)

  } else if(plot == "residplot"){

    # Residual plot
    plot_i <- resid_plot(model = model,
                         type = type,
                         smoother = smoother,
                         theme = theme,
                         axis.text.size = axis.text.size,
                         title.text.size = title.text.size,
                         title.opt = title.opt)

  } else if(plot == "respred"){

    # Response vs Predicted plot
    plot_i <- resid_respred(model = model,
                            theme = theme,
                            axis.text.size = axis.text.size,
                            title.text.size = title.text.size,
                            title.opt = title.opt)

  }

  ## Creation of interactive plot -------------------------------------------------

  # Use plotly to create interactive plot requested
  if(plot == "cookd"){
    ggplotly(plot_i, tooltip = c("cooksd", "Data"))
  } else if (plot == "boxplot"){
    ggplotly(plot_i, tooltip = c("Residual", "Data"))
  } else if (plot == "residlev"){
    ggplotly(plot_i, tooltip = c("Leverage", "Std_Res", "Data"))
  }else if (plot == "ls"){
    if (class(model)[1] == "lm"){
      ggplotly(plot_i + labs(x = "Predicted Values", y = "sqrt(|Standardized Residuals|)"),
               tooltip=c("Prediction", "Sqrt_Std_Res", "Data"))
      } else if(is.na(type) | type == "deviance" | type == "stand.deviance"){
        ggplotly(plot_i + labs(x = "Predicted Values",
                               y = "sqrt(|Standardized Deviance Residuals|)"),
                 tooltip = c("Prediction", "Sqrt_Std_Res", "Data"))
      } else if(type == "pearson" | type == "stand.pearson"){
        ggplotly(plot_i + labs(x = "Predicted Values",
                               y = "sqrt(|Standardized Pearson Residuals|)"),
                 tooltip = c("Prediction", "Sqrt_Std_Res", "Data"))
    }
  } else{
    ggplotly(plot_i)
  }

}
