#' Interaction Versions of Residual Diagnostics Plots.
#'
#' Creates interaction versions of all residual diagnostic plots given a model.
#' Currently accepts models of type "lm", "glm", "lmerMod", and "glmerMod".
#'
#' @param model Model fit using either \code{lm}, \code{glm}, \code{lmer}, or \code{glmer}.
#' @param plot Specify plot to create an interactive version. (See details for options.)
#' @param type The user may specify a type of residuals to use. Otherwise, the default
#' residual type for each model is used. (See details in the help file for
#' \code{resid_panel} for options.)
#' @param bins Number of bins to use when creating a histogram of the residuals.
#' @param smoother Indicates whether or not to include a smoother on the residual plot.
#' Specify TRUE or FALSE. Default is set to FALSE.
#' @param qqline Indicates whether to include a 1-1 line on the qq-plot. Specify TRUE or
#' FALSE. Default is set to TRUE.
#' @param theme ggplot2 theme to be used. Current options are \code{"bw"}, \code{"classic"},
#' and \code{"grey"} (or \code{"gray"}). Default is \code{"bw"}.
#' @param axis.text.size Specifies the size of the text for the axis labels of all plots
#' in the panel.
#' @param title.text.size Specifies the size of the text for the titles of all plots in
#' the panel.
#' @param title.opt Indicates whether or not to include a title on the plots in the panel.
#' Specify TRUE or FALSE. Default is set to TRUE.
#'
#' @export
#'
#' @importFrom plotly ggplotly
#'
#' @details Currently, only one plot can be made interactive at a time. The options are as
#' follows.
#'   \itemize{
#'     \item \code{"boxplot"}: A boxplot of residuals.
#'     \item \code{"cookd"}: A plot of Cook's D values versus observation number.
#'     \item \code{"hist"}: A histogram of residuals.
#'     \item \code{"ls"}: A location scale plot of the residuals.
#'     \item \code{"qq"}: A normal quantile plot of residuals.
#'     \item \code{"lev"}: A plot of leverage values versus residuals.
#'     \item \code{"resid"}: A plot of residuals versus predicted values.
#'     \item \code{"yvp":}: A plot of the response variable versus the predicted values.
#'   }
#' Note: \code{"cookd"}, \code{"ls"}, and \code{"lev"} are not available for "lmer"
#' and "glmer" models.
#'
#' Details on the creation of the plots can be found in the details section of the help
#' file for \code{resid_panel}.
#'
#' @return The interactive residual diagnostic plot specified.
#'
#' @examples
#' ## --------------------------------------------------------------------------------
#' ## Linear Regression Models
#' ## --------------------------------------------------------------------------------
#'
#' # Fit a linear regression model to predict the volume of a tree based on the
#' # girth of a tree using the R "trees" data
#' lm_model1 <- lm(Volume ~ Girth, data = trees)
#'
#' # Create an interactive Cook's D plot
#' resid_interact(lm_model1, plot = "cookd")
#'
#' # Fit a linear model to compare the weights of plants bewteen different
#' # treatment groups using the R "PlantGrowth" data
#' lm_model2 <- lm(weight ~ group, data = PlantGrowth)
#'
#' # Create an interactive location-scale plot with them classic theme
#' resid_interact(lm_model2, plot = "ls", theme = "classic")
#'
#' ## --------------------------------------------------------------------------------
#' ## Generalized Linear Regression Models
#' ## --------------------------------------------------------------------------------
#'
#' # Fit a generalized linear regression model using a Poisson family to compare
#' # the insect counts between different sprays from the R "InsectSprays" data
#' glm_model <- glm(count ~ spray, family = "poisson", data = InsectSprays)
#'
#' # Create an interactive residual plot with a smoother
#' resid_interact(glm_model, plot = "resid", smoother = TRUE)
#'
#' ## --------------------------------------------------------------------------------
#' ## Linear Mixed Effects Models
#' ## --------------------------------------------------------------------------------
#'
#' # Load the lme4 package
#' library(lme4)
#'
#' # Fit a linear mixed effect model to compare weights of chicks between diets using
#' # the R "ChickWeight" data and including chick as a random effect to account for the
#' # multiple measurements over time
#' lmer_model <- lmer(weight ~ Time + Diet + Time*Diet + (1|Chick), data = ChickWeight)
#'
#' # Create an interactive qq-plot without a title
#' resid_interact(lmer_model, plot = "qq", title.opt = FALSE)
#'
#' ## --------------------------------------------------------------------------------
#' ## Generalized Linear Mixed Effects Models
#' ## --------------------------------------------------------------------------------
#'
#' # Generate Poisson data
#' example_data <- data.frame(y = rpois(54, 3),
#'                            trt = rep(c("A", "B"), each = 27),
#'                            subject = rep(1:18, each = 3))
#'
#' # Fit a generalized linear mixed effects model with a Poisson family to compare
#' # the response between the treatments with a random effect for subject to
#' # account for the dependence within a subject
#' glmer_model <- glmer(y ~ trt + (1|subject), family = "poisson", data = example_data)
#'
#' # Create an interactive residual plot with the Pearson residuals
#' resid_interact(glmer_model, plot = "resid", type = "pearson")

resid_interact <- function(model, plot = NA, type = NA, bins = NA,
                           smoother = FALSE, qqline = TRUE, theme = "bw",
                           axis.text.size = 10, title.text.size = 12,
                           title.opt = TRUE){

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
                                   "lev", "resid", "yvp"))){
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
    if("ls" %in% plot |"lev" %in% plot){
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

  } else if(plot == "lev"){

    # Leverage plot
    plot_i <- resid_lev(model = model,
                        type = type,
                        theme = theme,
                        axis.text.size = axis.text.size,
                        title.text.size = title.text.size,
                        title.opt = title.opt)

  } else if(plot == "resid"){

    # Residual plot
    plot_i <- resid_plot(model = model,
                         type = type,
                         smoother = smoother,
                         theme = theme,
                         axis.text.size = axis.text.size,
                         title.text.size = title.text.size,
                         title.opt = title.opt)

  } else if(plot == "yvp"){

    # Response vs Predicted plot
    plot_i <- resid_yvp(model = model,
                            theme = theme,
                            axis.text.size = axis.text.size,
                            title.text.size = title.text.size,
                            title.opt = title.opt)

  }

  ## Creation of interactive plot -------------------------------------------------

  # Use plotly to create interactive plot requested
  if(plot == "cookd"){
    ggplotly(plot_i, tooltip = c("CooksD", "Data"))
  } else if (plot == "boxplot"){
    ggplotly(plot_i, tooltip = c("Residual", "Data"))
  } else if (plot == "lev"){
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
  } else if (plot=="hist"){
    ggplotly(plot_i, tooltip=c("Residual", "Data", "count"))
  } else{
    ggplotly(plot_i)
  }

}
