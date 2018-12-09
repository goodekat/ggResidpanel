#' Panel of Diagnostic Residual Plots Across Multiple Models.
#'
#' Creates a panel of residual diagnostic plots given a list of models. Currently accepts
#' models of type "lm", "glm", "lmerMod", "lmerModLmerTest", and "glmerMod".
#'
#' @param models List of models fit using either \code{lm}, \code{glm}, \code{lmer},
#'   \code{lmerTest}, or \code{glmer}.
#' @param plots Plots chosen to include in the panel of plots. The default panel
#'   includes a residual plot, a normal quantile plot, an index plot,
#'   and a histogram of the residuals. (See details for the options available.)
#' @param type Type of residuals to use in the plot. If not specified, the
#'   default residual type for each model type is used. (See details for the
#'   options available.)
#' @param bins Number of bins to use when creating a histogram of the residuals.
#'   Default is set to 30.
#' @param smoother Indicates whether or not to include a smoother on the
#'   residual plot. Specify TRUE or FALSE. Default is set to FALSE.
#' @param qqline Indicates whether to include a 1-1 line on the qq-plot. Specify
#'   TRUE or FALSE. Default is set to TRUE.
#' @param qqbands Indicates whether to include confidence bands on the qq-plot.
#'   Specify TRUE or FALSE. Default is set to FALSE.
#' @param scale Scales the size of the graphs in the panel. Takes values in (0,1].
#' @param theme ggplot2 theme to be used. Current options are \code{"bw"},
#'   \code{"classic"}, and \code{"grey"} (or \code{"gray"}). Default is
#'   \code{"bw"}.
#' @param axis.text.size Specifies the size of the text for the axis labels of
#'   all plots in the panel.
#' @param title.text.size Specifies the size of the text for the titles of all
#'   plots in the panel.
#' @param title.opt Indicates whether or not to include a title on the plots in
#'   the panel. Specify TRUE or FALSE. Default is set to TRUE.
#' @param nrow Sets the number of rows in the panel.
#'
#' @export resid_compare
#'
#' @details
#'
#' The first two sections below contain information on the available input
#' options for the \code{plots} and \code{type} arguments in \code{resid_panel}.
#' The third section contains details relating to the creation of the plots.
#'
#' \strong{Options for Plots}
#'
#' The following options can be chosen for the \code{plots} argument.
#' \itemize{
#' \item "all": This creates a panel of all plot types included in the package
#' that are available for the model type input into \code{residpanel}. (See note
#' below.)
#' \item "default": This creates a panel with a residual plot, a normal quantile plot
#' of the residuals, an index plot of the residuals, and a histogram of the residuals.
#' \item "R": This creates a panel with a residual plot, a normal
#' quantile plot of the residuals, a location-scale plot, and a leverage versus
#' residuals plot. This was modeled after the plots shown in R if the
#' \code{plot()} base function is applied to an \code{lm} model. This option can
#' only be used with an \code{lm} or \code{glm} model.
#' \item "SAS": This creates a panel with a residual plot, a normal quantile plot of
#' the residuals, a histogram of the residuals, and a boxplot of the residuals.
#' This was modeled after the residpanel option in proc mixed from SAS version 9.4.
#' \item A vector of individual plots can also be specified.
#' For example, one can specify \code{plots = c("boxplot", "hist")} or
#' \code{plots = "qq"}. The individual plot options are as follows.
#' \itemize{
#' \item \code{"boxplot"}: A boxplot of residuals
#' \item \code{"cookd"}: A plot of Cook's D values versus observation numbers
#' \item \code{"hist"}: A histogram of residuals
#' \item \code{"index"}: A plot of residuals versus observation numbers
#' \item \code{"ls"}: A location scale plot of the residuals
#' \item \code{"qq"}: A normal quantile plot of residuals
#' \item \code{"lev"}: A plot of leverage values versus residuals
#' \item \code{"resid"}: A plot of residuals versus predicted values
#' \item \code{"yvp":}: A plot of observed response values versus predicted values
#' } }
#'
#' Note: \code{"cookd"}, \code{"ls"}, and \code{"lev"} are not available for
#' "lmer", "lmerTest", and "glmer" models.
#'
#' \strong{Options for Type}
#'
#' Several residual types are available to be requested based on the model type
#' that is input into \code{resid_panel}. These currently are as follows.
#' \itemize{
#' \item \code{lm} residual options
#' \itemize{
#' \item \code{"pearson"}:The Pearson residuals
#' \item \code{"response"}: The raw residuals (Default for "lm")
#' \item \code{"standardized"}: The standardized raw residuals
#' }
#' \item \code{glm} residual options
#' \itemize{
#' \item \code{"pearson"}: The Pearson residuals
#' \item \code{"deviance"}: The deviance residuals (Default for "glm")
#' \item \code{"response"}: The raw residuals
#' \item \code{"stand.deviance"}: The standardized deviance residuals
#' \item \code{"stand.pearson"}: The standardized Pearson residuals
#' }
#' \item \code{lmer} and \code{lmerTest} residual options
#' \itemize{
#' \item \code{"pearson"}: The Pearson residuals (Default for "lmer" and "lmerTest")
#' \item \code{"response"}: The raw residuals
#' }
#' \item \code{glmer} residual options
#' \itemize{
#' \item \code{"pearson"}: The Pearson residuals
#' \item \code{"deviance"}: The deviance residuals (Default for "glmer")
#' \item \code{"response"}: The raw residuals
#' } }
#'
#' Note: The plots of \code{"ls"} and \code{"lev"} only accept standarized residuals.
#'
#' \strong{Details on the Creation of Plots}
#'
#' \describe{
#' \item{Boxplot (\code{boxplot})}{Boxplot of the residuals.}
#'
#' \item{Cook's D (\code{cookd})}{ The horizontal line represents a cut-off to identify
#' highly influential points. The horizontal line is placed at 4/n where n is
#' the number of data points used in the \code{model}.}
#'
#' \item{Histogram (\code{hist})}{Plots a historgram of the residuals. The density
#' curve overlaid has mean equal to zero and standard deviation equal to the
#' standard deviation of the residuals.}
#'
#' \item{Index Plot (\code{index})}{Plots the residuals on the y-axis and the observation
#' number associated with the residual on the x-axis.}
#'
#' \item{Leverage Plot (\code{lev})}{Plots the standardized residuals on the y-axis
#' and the leverage values on the x-axis. A lowess curve is overlaid, and Cook's
#' D contours are included for \eqn{\alpha = 0.5} and \eqn{\alpha = 1}.}
#'
#' \item{Location-Scale Plot (\code{ls})}{Plots the square root of the absolute value
#' of the standardized residuals on the y-axis and the predicted values on the
#' x-axis. The predicted values are plotted on the original scale for \code{glm}
#' and \code{glmer} models. A lowess curve is overlaid.}
#'
#' \item{QQ Plot (\code{qq})}{Makes use of the \code{R} package \code{qqplotr} for
#' creating a normal quantile plot of the residuals.}
#'
#' \item{Residual Plot (\code{resid})}{Plots the residuals on the y-axis and the
#' predicted values on the x-axis. The predicted values are plotted on the
#' original scale for \code{glm} and \code{glmer} models.}
#'
#' \item{Response vs. Predicted (\code{yvp})}{Plots the response variable from the
#' model on the y-axis and the predicted values on the x-axis. Both response
#' variable and predicted values are plotted on the original scale for
#' \code{glm} and \code{glmer} models.}
#'}
#'
#' @return A panel of residual diagnostic plots containing plots specified for each model.
#'


resid_compare <- function(models, plots = "default", type = NA, bins = 30,
                        smoother = FALSE, qqline = TRUE, qqbands = FALSE,
                        scale = 1, theme = "bw", axis.text.size = 10,
                        title.text.size = 12, title.opt = TRUE, nrow = NULL){

  ## Set number of rows
  compare_rows <- length(plots)
  if(compare_rows==1){
    if(plots=="all"){
      compare_rows <- 9
    }else if (plots %in% c("default", "R", "SAS")){
      compare_rows <- 4
    }
  }

    ## Errors and Warnings -------------------------------------------------------

  # Checks that return an error
  for(i in 1:length(models)){
    check_modeltype(model = models[[i]])
    check_residualtype(model = models[[i]], type = type[i])
    check_standardized(model = models[[i]], plots = plots)
    check_cooksd(model = models[[i]], plots = plots)
    check_leverage(model = models[[i]], plots = plots)

  }

  # Checks that return a warning
  smoother <- check_smoother(smoother = smoother)
  theme <- check_theme(theme = theme)
  title.opt <- check_title(title.opt = title.opt)

  ## Creation of plots ---------------------------------------------------------
  # List of plots
  compare_list <- list()
  compare_index <- 1

  # Create a boxplot of the residuals if selected in plots otherwise set as NULL
  if("boxplot" %in% plots | "SAS" %in% plots | "all" %in% plots){

    for(i in 1:length(models)){
      compare_list[[compare_index]] <- plot_boxplot(type = type,
                   model = models[[1]],
                   theme = theme,
                   axis.text.size = axis.text.size,
                   title.text.size = title.text.size,
                   title.opt = title.opt)
      compare_index <- compare_index+1
    }

  } else{
    boxplot <- NULL
  }

  # Create a Cook's D plot if selected in plots otherwise set as NULL
  if("cookd" %in% plots){

    for(i in 1:length(models)){
      compare_list[[compare_index]] <- plot_cookd(model = models[[i]],
                                      theme = theme,
                                      axis.text.size = axis.text.size,
                                      title.text.size = title.text.size,
                                      title.opt = title.opt)
      compare_index <- compare_index+1
    }

  } else if("all" %in% plots){
    check_count <- 0
    for(i in 1:length(models)){
      if(!(class(models[[i]])[1] %in% c("lmerMod", "lmerModLmerTest", "glmerMod"))){
        check_count <- check_count+1
      }
    }
    if(check_count==length(models)){
      for(i in 1:length(models)){
        compare_list[[compare_index]] <- plot_cookd(model = models[[i]],
                                      theme = theme,
                                      axis.text.size = axis.text.size,
                                      title.text.size = title.text.size,
                                      title.opt = title.opt)
        compare_index <- compare_index+1
      }
    }
    } else{
    cookd <- NULL
  }

  # Create a histogram of the residuals if selected in plots otherwise set as NULL
  if("hist" %in% plots | "default" %in% plots | "SAS" %in% plots | "all" %in% plots){

    for(i in 1:length(models)){
      compare_list[[compare_index]] <- plot_hist(model = models[[i]],
                                     type = type,
                                     bins = bins,
                                     theme = theme,
                                     axis.text.size = axis.text.size,
                                     title.text.size = title.text.size,
                                     title.opt = title.opt)
      compare_index <- compare_index+1
    }


  } else{
    hist <- NULL
  }

  # Create an index plot of the residuals if selected in plots otherwise set as NULL
  if("index" %in% plots | "default" %in% plots | "all" %in% plots){

    for(i in 1:length(models)){
      compare_list[[compare_index]] <- plot_index(model = models[[i]],
                                      type = type,
                                      theme = theme,
                                      axis.text.size = axis.text.size,
                                      title.text.size = title.text.size,
                                      title.opt = title.opt)
      compare_index <- compare_index+1
    }

  } else{
    index <- NULL
  }

  # Create a residual-leverage plot if selected in plots otherwise set as NULL
  if("lev" %in% plots | "R" %in% plots){

    for(i in 1:length(models)){
      compare_list[[compare_index]] <- plot_lev(model = models[[i]],
                                    type = type,
                                    theme = theme,
                                    axis.text.size = axis.text.size,
                                    title.text.size = title.text.size,
                                    title.opt = title.opt)
      compare_index <- compare_index+1
    }


  } else if("all" %in% plots){
    check_count <- 0
    for(i in 1:length(models)){
      if(!(class(models[[i]])[1] %in% c("lmerMod", "lmerModLmerTest", "glmerMod"))){
        check_count <- check_count+1
      }
    }

    if(check_count==length(models)){
      for(i in 1:length(models)){
        compare_list[[compare_index]] <- plot_lev(model = models[[i]],
                                    type = type,
                                    theme = theme,
                                    axis.text.size = axis.text.size,
                                    title.text.size = title.text.size,
                                    title.opt = title.opt)
        compare_index <- compare_index+1
      }
    }
  } else{
    lev <- NULL
  }

  # Create a location-scale plot if selected in plots otherwise set as NULL
  if("ls" %in% plots | "R" %in% plots){

    for(i in 1:length(models)){
      compare_list[[compare_index]] <- plot_ls(model = models[[i]],
                                   type = type,
                                   theme = theme,
                                   axis.text.size = axis.text.size,
                                   title.text.size = title.text.size,
                                   title.opt = title.opt)
      compare_index <- compare_index+1
    }

  } else if("all" %in% plots){
    check_count <- 0
    for(i in 1:length(models)){
      if(!(class(models[[i]])[1] %in% c("lmerMod", "lmerModLmerTest", "glmerMod"))){
        check_count <- check_count+1
      }
    }

    if(check_count==length(models)){
      for(i in 1:length(models)){
        compare_list[[compare_index]] <- plot_ls(model = models[[i]],
                                   type = type,
                                   theme = theme,
                                   axis.text.size = axis.text.size,
                                   title.text.size = title.text.size,
                                   title.opt = title.opt)
        compare_index <- compare_index+1
      }
    }
  } else{
    ls <- NULL
  }

  # Create a q-q plot of the residuals if selected in plots otherwise set as NULL
  if("qq" %in% plots | "default" %in% plots | "SAS" %in% plots | "R" %in% plots | "all" %in% plots){

    for(i in 1:length(models)){
      compare_list[[compare_index]] <- plot_qq(model = models[[i]],
                                   type = type,
                                   theme = theme,
                                   axis.text.size = axis.text.size,
                                   title.text.size = title.text.size,
                                   title.opt = title.opt,
                                   qqline = qqline,
                                   qqbands = qqbands)
      compare_index <- compare_index+1
    }

  } else{
    qq <- NULL
  }

  # Create a residual plot if selected in plots otherwise set as NULL
  if("resid" %in% plots | "default" %in% plots | "SAS" %in% plots | "R" %in% plots | "all" %in% plots){

    for(i in 1:length(models)){
      compare_list[[compare_index]] <- plot_resid(model = models[[i]],
                                      type = type,
                                      smoother = smoother,
                                      theme = theme,
                                      axis.text.size = axis.text.size,
                                      title.text.size = title.text.size,
                                      title.opt = title.opt)
      compare_index <- compare_index+1
    }

    } else{
    resid <- NULL
  }

  # Create a plot of the response variable vs the predicted values if selected
  # in plots otherwise set as NULL
  if("yvp" %in% plots | "all" %in% plots){

    for(i in 1:length(models)){
      compare_list[[compare_index]] <- plot_yvp(model = models[[i]],
                                                theme = theme,
                                                axis.text.size = axis.text.size,
                                                title.text.size = title.text.size,
                                                title.opt = title.opt)
      compare_index <- compare_index+1
    }


  } else{
    yvp <- NULL
  }

  ## Creation of grid of plots -------------------------------------------------

  # If individual plots have been specified, set plots equal to "individual"
  # Return an error if none of the correct plot options have been specified
  if("default" %in% plots | "SAS" %in% plots | "R" %in% plots | "all" %in% plots){
    plots <- plots
  } else if("boxplot" %in% plots | "cookd" %in% plots | "index" %in% plots |
            "hist" %in% plots | "ls" %in% plots | "qq" %in% plots |
            "lev" %in% plots | "resid" %in% plots | "yvp" %in% plots){
    chosen <- plots
    plots <- "individual"
  } else{
    stop("Invalid plots option entered. See the resid_panel help file for
         available options.")
  }
#
#   # Create a grid of plots based on the plots specified
#   if (plots == "default"){
#
#     # Create grid of the default plots
#     plot_grid(resid, qq, index, hist,
#               scale = scale, nrow = nrow)
#
#   } else if (plots == "SAS"){
#
#     # Create grid of SAS plots
#     plot_grid(resid, hist, qq, boxplot,
#               scale = scale, nrow = nrow)
#
#   } else if (plots == "R") {
#
#     # Create grid of R plots
#     plot_grid(resid, qq, ls, lev,
#               scale = scale, nrow = nrow)
#
#   } else if (plots == "all") {
#
#     # Create grid of all plots
#     if(class(model)[1] == "lm" | class(model)[1] == "glm"){
#
#       # Create the grid
#       plot_grid(resid, hist, qq, boxplot, cookd, ls, lev, yvp, index,
#                 scale = scale, nrow = nrow)
#     } else{
#
#       # Create the grid
#       plot_grid(resid, hist, qq, boxplot, yvp, index,
#                 scale = scale, nrow = nrow)
#     }
#
#   } else if (plots == "individual") {
#
#     # Turn the specified plots into a list
#     individual_plots <- list(boxplot = boxplot,
#                              cookd = cookd,
#                              hist = hist,
#                              index = index,
#                              ls = ls,
#                              qq = qq,
#                              lev = lev,
#                              resid = resid,
#                              yvp = yvp)
#
#     # Select the chosen plots
#     individual_plots <- individual_plots[chosen]
#
#     # Create grid of individual plots specified
#     plot_grid(plotlist = individual_plots, scale = scale, nrow = nrow)
#
#   }
  plot_grid(plotlist = compare_list, scale = scale, nrow = compare_rows)
}
