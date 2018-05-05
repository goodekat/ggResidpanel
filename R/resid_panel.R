#' Panel of Diagnostic Residual Plots.
#'
#' Creates a panel of residual diagnostic plots given a model. Currently accepts models of
#' type "lm", "glm", "lmerMod", and "glmerMod".
#'
#' @param model Model fit using either \code{lm}, \code{glm}, \code{lmer}, or \code{glmer}.
#' @param plots Plots chosen to include in the panel of plots. Default is set to "SAS".
#' (See details for options.)
#' @param type The user may specify a type of residuals to use. Otherwise, the default
#' residual type for each model is used. (See details for options.)
#' @param bins Number of bins to use when creating a histogram of the residuals.
#' @param smoother Indicates whether or not to include a smoother on the residual plot.
#' Specify TRUE or FALSE. Default is set to FALSE.
#' @param qqline Indicates whether to include a 1-1 line on the qq-plot. Specify TRUE or
#' FALSE. Default is set to TRUE.
#' @param qqbands Indicates whether to include confidence bands on the qq-plot. Specify
#' TRUE or FALSE. Default is set to FALSE.
#' @param scale Scales the size of the graphs in a panel. Takes values in (0,1].
#' @param axis.text.size Specifies the size of the text for the axis labels of all plots
#' in the panel.
#' @param title.text.size Specifies the size of the text for the titles of all plots in
#' the panel.
#' @param title.opt Indicates whether or not to include a title on the plots in the panel.
#' Specify TRUE or FALSE. Default is set to TRUE.
#' @param theme ggplot2 theme to be used. Current options are \code{"bw"}, \code{"classic"},
#' and \code{"grey"} (or \code{"gray"}). Default is \code{"bw"}.
#' @param ind.ncol When a vector of individual plots has been requested, this specifies the
#' number of columns in the panel. Default is set to 2 columns.
#'
#'
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_abline labs theme_bw theme geom_histogram
#' stat_function xlim geom_boxplot expand_limits geom_smooth element_text ggplotGrob geom_vline
#' theme_classic geom_hline geom_segment geom_line scale_x_continuous scale_y_continuous
#' theme_grey
#' @importFrom cowplot plot_grid
#' @importFrom gridExtra grid.arrange tableGrob ttheme_minimal
#' @importFrom MASS stdres
#' @importFrom qqplotr stat_qq_point stat_qq_line stat_qq_band
#' @importFrom stringr str_sub
#'
#' @details
#'
#' The first two sections below contain information on the available input options for the
#' \code{plots} and \code{type} arguments in \code{resid_panel}. The third section contains
#' information on the details relating to the creation of the plots.
#'
#' \strong{Options for} \code{plots}
#'
#' The following options can be chosen for the \code{plots} argument.
#' \itemize{
#'   \item "all": This creates a panel of all plot types included in the package that
#'   are available for the model type input into \code{residpanel}. (See note below.)
#'   \item "R": This creates a panel with a residual plot, a normal quantile plot of
#'   the residuals, a location-scale plot, and a leverage versus residuals plot. This was modeled after the
#'   plots shown in R if the \code{plot()} base function is applied to an \code{lm}
#'   model. This option can only be used with an \code{lm} or \code{glm} model.
#'   \item "SAS": This is the default option. It creates a panel with a residual plot,
#'   a normal quantile plot of the residuals, a histogram of the residuals, and a
#'   boxplot of the residuals. This was modeled after the residpanel option in proc
#'   mixed from SAS version 9.4.
#'   \item A vector of individual plots can also be specified. For example, one can
#'   specify \code{plots = c("boxplot", "hist")} or \code{plots = "qq"}. The individual
#'   plot options are as follows.
#'   \itemize{
#'     \item \code{"boxplot"}: A boxplot of residuals
#'     \item \code{"cookd"}: A plot of Cook's D values versus observation numbers
#'     \item \code{"hist"}: A histogram of residuals
#'     \item \code{"ls"}: A location scale plot of the residuals
#'     \item \code{"qq"}: A normal quantile plot of residuals
#'     \item \code{"lev"}: A plot of leverage values versus residuals
#'     \item \code{"resid"}: A plot of residuals versus predicted values
#'     \item \code{"yvp":}: A plot of observed response values versus predicted values
#'   }
#' }
#' Note: \code{"cookd"}, \code{"ls"}, and \code{"lev"} are not available for "lmer"
#' and "glmer" models.
#'
#' \strong{Options for} \code{type}
#'
#' Several residual types are available to be requested based on the model type that
#' is input into \code{resid_panel}. These currently are as follows.
#' \itemize{
#'   \item \code{lm} residual options
#'   \itemize{
#'     \item \code{"pearson"}: The Pearson residuals
#'     \item \code{"response"}: The raw residuals (Default for "lm")
#'     \item \code{"standardized"}: The standardized raw residuals
#'   }
#'   \item \code{glm} residual options
#'   \itemize{
#'     \item \code{"pearson"}: The Pearson residuals
#'     \item \code{"deviance"}: The deviance residuals (Default for "glm")
#'     \item \code{"response"}: The raw residuals
#'     \item \code{"stand.deviance"}: The standardized deviance residuals
#'     \item \code{"stand.pearson"}: The standardized Pearson residuals
#'   }
#'   \item \code{lmer} residual options
#'   \itemize{
#'     \item \code{"pearson"}: The Pearson residuals (Default for "lmer")
#'     \item \code{"response"}: The raw residuals
#'   }
#'   \item \code{glmer} residual options
#'   \itemize{
#'     \item \code{"pearson"}: The Pearson residuals
#'     \item \code{"deviance"}: The deviance residuals (Default for "glmer")
#'     \item \code{"response"}: The raw residuals
#'   }
#' }
#' Note: The plots of \code{"ls"} and \code{"lev"} only accept standarized residuals.
#'
#' \strong{Details on the Creation of Plots}
#'
#' Boxplot (\code{boxplot}): Boxplot of the residuals.
#'
#' Cook's D (\code{cookd}): The horizontal line represents a cut-off to identify highly
#' influential points. The horizontal line is placed at 4/n where n is the number of
#' data points used in the \code{model}.
#'
#' Histogram (\code{hist}): Plots a historgram of the residuals. The density
#' curve overlaid has mean equal to zero and standard deviation
#' equal to the standard deviation of the residuals.
#'
#' Leverage Plot (\code{lev}): Plots the standardized residuals on the y-axis and the
#' leverage values on the x-axis. A lowess curve is overlaid, and Cook's D contours
#' are included for \eqn{\alpha = 0.5} and \eqn{\alpha = 1}.
#'
#' Location-Scale Plot (\code{ls}): Plots the square root of the absolute value of the
#' standardized residuals on the y-axis and the predicted values on the x-axis. The
#' predicted values are plotted on the original scale for \code{glm} and \code{glmer}
#' models. A lowess curve is overlaid.
#'
#' QQ Plot (\code{qq}): Makes use of the \code{R} package \code{qqplotr} for creating
#' a normal quantile plot of the residuals.
#'
#' Residual Plot (\code{resid}): Plots the residuals on the y-axis and the predicted
#' values on the x-axis. The predicted values are plotted on the original scale for
#' \code{glm} and \code{glmer} models.
#'
#' Response vs. Predicted (\code{yvp}): Plots the response variable from the model
#' on the y-axis and the predicted values on the x-axis. Both response variable and
#' predicted values are plotted on the original scale for \code{glm} and \code{glmer}
#' models.
#'
#' @return A panel of residual diagnostic plots containing plots specified.
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
#' # Plot the residuals using the default panel
#' resid_panel(lm_model1, bins = 30)
#'
#' # Create a panel of all diagnostic plot options and add a smoother to the
#' # residual plot
#' resid_panel(lm_model1, bins = 30, plots = "all", smoother = TRUE)
#'
#' # Fit a linear model to compare the weights of plants bewteen different
#' # treatment groups using the R "PlantGrowth" data
#' lm_model2 <- lm(weight ~ group, data = PlantGrowth)
#'
#' # Create a panel of the residual plot, the histogram, and the location-scale plot for
#' #"lm" models using the classic theme with three columns
#' resid_panel(lm_model2, plots = c("resid", "hist", "ls"), theme = "classic", ind.ncol=3)
#'
#' ## --------------------------------------------------------------------------------
#' ## Generalized Linear Regression Models
#' ## --------------------------------------------------------------------------------
#'
#' # Fit a generalized linear regression model using a Poisson family to compare
#' # the insect counts between different sprays from the R "InsectSprays" data
#' glm_model1 <- glm(count ~ spray, family = "poisson", data = InsectSprays)
#'
#' # Plot the residuals using the default panel without titles and with a gray theme
#' resid_panel(glm_model1, bins = 30, title.opt = FALSE, theme = "gray")
#'
#' # Plot the residuals using the default panel with standarized deviance residuals
#' # and use a smaller scaling of the plots
#' resid_panel(glm_model1, type = "stand.deviance", scale = 0.9)
#'
#' #Generate binomial data
#' total <- c(rpois(45,20))+1
#' example_data1 <- data.frame(success = rbinom(45, total,.56),
#' trt = rep(c("A", "B", "C"), each = 15))
#'
#' # Fit a generalized lineaer regression model using a binomial family to compare
#' # across treatments
#' glm_model2 <- glm(cbind(success, total-success) ~ trt, family = "binomial", data = example_data1)
#'
#' #Plot all residual plots with Pearson residuals
#' resid_panel(glm_model2, plot = "all", type = "pearson")
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
#' # Create a panel of the residual plot and the normal quantile plot with
#' # confidence bands
#' resid_panel(lmer_model, plots = c("resid", "qq"), qqbands = TRUE)
#'
#' ## --------------------------------------------------------------------------------
#' ## Generalized Linear Mixed Effects Models
#' ## --------------------------------------------------------------------------------
#'
#' # Generate Poisson data
#' example_data2 <- data.frame(y = rpois(54, 3),
#'                            trt = rep(c("A", "B"), each = 27),
#'                            subject = rep(1:18, each = 3))
#'
#' # Fit a generalized linear mixed effects model with a Poisson family to compare
#' # the response between the treatments with a random effect for subject to
#' # account for the dependence within a subject
#' glmer_model <- glmer(y ~ trt + (1|subject), family = "poisson", data = example_data2)
#'
#' # Plot the residual plot with the size of the title and axis lables increased
#' resid_panel(glmer_model, plots = "resid", title.text.size = 14, axis.text.size = 12)
#'
#' # Plot the residual plot using the Pearson residuals
#' resid_panel(glmer_model, plots = "resid", type = "pearson")

resid_panel <- function(model, plots = "SAS", type = NA, bins = NA,
                        smoother = FALSE, qqline = TRUE, qqbands = FALSE,
                        scale = 1, theme = "bw", axis.text.size = 10,
                        title.text.size = 12, title.opt = TRUE,
                        ind.ncol = 2){

  ## Errors and Warnings -------------------------------------------------------

  # Return an error if an acceptable model type is not entered in the function
  if(!(class(model)[1] %in% c("lm", "glm", "lmerMod", "glmerMod")))
    stop("The updated version of resid_panel requires a model to be input to the functions.
         Accepted models currently are 'lm', 'glm', 'lmer', and 'glmer'. If using residuals
         from a different model type, use the new function resid_auxpanel to create a panel
         using vectors of the residuals and fitted values.")

  # Return an error if the requested residual type is not available for the model type
  type <- tolower(type)
  if(!is.na(type)){
    if(class(model)[1] == "lm"){
      if(!(type %in% c("response", "pearson", "standardized"))){
        stop("The requested residual type is not available for an 'lm' model. Please select
             from the following options for an 'lm' model: response, pearson, or standardized.")
      }
    } else if(class(model)[1] == "glm"){
      if(!(type %in% c("response", "pearson", "deviance", "stand.pearson", "stand.deviance"))){
        stop("The requested residual type is not available for a 'glm' model. Please select
             from the following options for a 'glm' model: response, pearson, deviance,
             stand.deviance, or stand.pearson.")
      }
    } else if(class(model)[1] == "lmerMod"){
      if(!(type %in% c("response", "pearson"))){
        stop("The requested residual type is not available for an 'lmer' model. Please select
             from the following options for an 'lmer' model: response or pearson.")
      }
    } else if(class(model)[1] == "glmerMod"){
      if(!(type %in% c("response", "pearson", "deviance"))){
        stop("The requested residual type is not available for a 'glmer' model. Please select
             from the following options for a 'glmer' model: response, pearson, or deviance.")
      }
    }
  }

  # Return an error if the requested plots involve standardizing residuals for an 'lmer' or
  # a 'glmer' model
  if(class(model)[1] %in% c("lmerMod", "glmerMod")){
    if("ls" %in% plots |"lev" %in% plots | "R" %in% plots){
      stop("The requested plot or panel uses standardized residuals, which are not
           currently available for 'lmer' or 'glmer' models.")
    }
  }

  # Return an error if Cook's D plot is requested for an 'lmer' or 'glmer' model
  if(class(model)[1] %in% c("lmerMod", "glmerMod")){
    if("cookd" %in% plots){
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

  # Return a warning about choosing the number of bins if a histogram is included
  # and the number of bins has not been specified
  if("SAS" %in% plots | "all" %in% plots | "hist" %in% plots){
    if(is.na(bins)){
      bins = 30
      warning("By default, bins = 30 in the histogram of residuals. If necessary,
              specify an appropriate number of bins.")
    }
  }

  #Return warning if consant leverage
  if("all" %in% plots | "R" %in% plots | "lev" %in% plots){
    leverage_val <- hatvalues(model)
    zero_range <- function(x, tol = .Machine$double.eps ^ 0.5) {
      if (length(x) == 1) return(TRUE)
      x <- range(x) / mean(x)
      isTRUE(all.equal(x[1], x[2], tolerance = tol))
    }

    if(zero_range(leverage_val)==TRUE){
      warning("Note that this model has constant leverage.")
    }

  }
  ## Creation of plots ---------------------------------------------------------

  # Create a boxplot of the residuals if selected in plots otherwise set as NULL
  if("boxplot" %in% plots | "SAS" %in% plots | "all" %in% plots){
    boxplot <- resid_boxplot(type = type,
                             model = model,
                             theme = theme,
                             axis.text.size = axis.text.size,
                             title.text.size = title.text.size,
                             title.opt = title.opt)
  } else{
    boxplot <- NULL
  }

  # Create a Cook's D plot if selected in plots otherwise set as NULL
  if("cookd" %in% plots){
    cookd <- resid_cookd(model = model,
                         theme = theme,
                         axis.text.size = axis.text.size,
                         title.text.size = title.text.size,
                         title.opt = title.opt)
  } else if("all" %in% plots & !(class(model)[1] %in% c("lmerMod", "glmerMod"))){
    cookd <- resid_cookd(model = model,
                         theme = theme,
                         axis.text.size = axis.text.size,
                         title.text.size = title.text.size,
                         title.opt = title.opt)
  } else{
    cookd <- NULL
  }

  # Create a histogram of the residuals if selected in plots otherwise set as NULL
  if("hist" %in% plots | "SAS" %in% plots | "all" %in% plots){
    hist <- resid_hist(model = model,
                       type = type,
                       bins = bins,
                       theme = theme,
                       axis.text.size = axis.text.size,
                       title.text.size = title.text.size,
                       title.opt = title.opt)
  } else{
    hist <- NULL
  }

  # Create a location-scale plot if selected in plots otherwise set as NULL
  if("ls" %in% plots | "R" %in% plots){
    ls <- resid_ls(model = model,
                   type = type,
                   theme = theme,
                   axis.text.size = axis.text.size,
                   title.text.size = title.text.size,
                   title.opt = title.opt)
  } else if("all" %in% plots & !(class(model)[1] %in% c("lmerMod", "glmerMod"))){
    ls <- resid_ls(model = model,
                   type = type,
                   theme = theme,
                   axis.text.size = axis.text.size,
                   title.text.size = title.text.size,
                   title.opt = title.opt)
  } else{
    ls <- NULL
  }

  # Create a q-q plot of the residuals if selected in plots otherwise set as NULL
  if("qq" %in% plots | "SAS" %in% plots | "R" %in% plots | "all" %in% plots){
    qq <- resid_qq(model = model,
                   type = type,
                   theme = theme,
                   axis.text.size = axis.text.size,
                   title.text.size = title.text.size,
                   title.opt = title.opt,
                   qqline = qqline,
                   qqbands = qqbands)
  } else{
    qq <- NULL
  }

  # Create a residual-leverage plot if selected in plots otherwise set as NULL
  if("lev" %in% plots | "R" %in% plots){
    lev <- resid_lev(model = model,
                          type = type,
                          theme = theme,
                          axis.text.size = axis.text.size,
                          title.text.size = title.text.size,
                          title.opt = title.opt)
  } else if("all" %in% plots & !(class(model)[1] %in% c("lmerMod", "glmerMod"))){
    lev <- resid_lev(model = model,
                          type = type,
                          theme = theme,
                          axis.text.size = axis.text.size,
                          title.text.size = title.text.size,
                          title.opt = title.opt)
  } else{
    lev <- NULL
  }

  # Create a residual plot if selected in plots otherwise set as NULL
  if("resid" %in% plots | "SAS" %in% plots | "R" %in% plots | "all" %in% plots){
    resid <- resid_plot(model = model,
                            type = type,
                            smoother = smoother,
                            theme = theme,
                            axis.text.size = axis.text.size,
                            title.text.size = title.text.size,
                            title.opt = title.opt)
  } else{
    resid <- NULL
  }

  # Create a plot of the response variable vs the predicted values if selected
  # in plots otherwise set as NULL
  if("yvp" %in% plots | "all" %in% plots){
    yvp <- resid_yvp(model = model,
                             theme = theme,
                             axis.text.size = axis.text.size,
                             title.text.size = title.text.size,
                             title.opt = title.opt)
  } else{
    yvp <- NULL
  }

  ## Creation of grid of plots -------------------------------------------------

  # If individual plots have been specified, set plots equal to "individual"
  # Return an error if none of the correct plot options have been specified
  if("SAS" %in% plots | "R" %in% plots | "all" %in% plots){
    plots <- plots
  } else if("boxplot" %in% plots | "cookd" %in% plots | "hist" %in% plots |
            "ls" %in% plots | "qq" %in% plots | "lev" %in% plots |
            "resid" %in% plots | "yvp" %in% plots){
    plots <- "individual"
  } else{
    stop("Invalid plots option entered. See the resid_panel help file for
         available options.")
  }

  # Create a grid of plots based on the plots specified
  if(plots == "SAS"){

    # Create grid of SAS plots in resid panel
    plot_grid(resid, hist, qq, boxplot, scale = scale)

  } else if (plots == "R") {

    # Create grid of R plots
    plot_grid(resid, qq, ls, lev, scale = scale)

  } else if (plots == "all") {

    # Create grid of all plots
    if(class(model)[1] == "lm" | class(model)[1] == "glm"){
      plot_grid(resid, hist, qq, boxplot, cookd, ls, lev, yvp,
                scale = scale)
    } else{
      plot_grid(resid, hist, qq, boxplot, yvp, scale = scale)
    }

  } else if (plots == "individual") {

    # Turn the specified plots into a list
    individual_plots <- list(boxplot = boxplot,
                             cookd = cookd,
                             hist = hist,
                             ls = ls,
                             qq = qq,
                             lev = lev,
                             resid = resid,
                             yvp = yvp)

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
