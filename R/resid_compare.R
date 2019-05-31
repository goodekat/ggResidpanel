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
#' @param smoother Indicates whether or not to include a smoother on the index,
#'   residual-leverage, location-scale, and residual plots. Specify TRUE or FALSE.
#'   Default is set to FALSE.
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
#' options for the \code{plots} and \code{type} arguments in \code{resid_compare}.
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
#' Note: \code{"cookd"}, \code{"ls"}, and \code{"lev"} are only available for "lm"
#' and "glm" models.
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
#' \item \code{lmer}, \code{lmerTest}, and \code{lme} residual options
#' \itemize{
#' \item \code{"pearson"}: The Pearson residuals (Default for "lmer", "lmerTest", and "lme")
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
#' @examples
#'
#' # Fit two models to the penguins data
#' penguin_model <- lme4::lmer(heartrate ~ depth + duration + (1|bird), data = penguins)
#' penguin_model_log2 <- lme4::lmer(log(heartrate) ~ depth + duration + I(duration^2) +
#' (1|bird), data = penguins)
#'
#' # Compare the residuals from the model
#' resid_compare(list(penguin_model, penguin_model_log2))
#'
#' # Adjust some options in the panel of plots
#' resid_compare(list(penguin_model, penguin_model_log2), plots = c("resid", "yvp"),
#' smoother = TRUE, theme = "grey")

resid_compare <- function(models, plots = "default", type = NA, bins = 30,
                        smoother = FALSE, qqline = TRUE, qqbands = FALSE,
                        scale = 1, theme = "bw", axis.text.size = 10,
                        title.text.size = 12, title.opt = TRUE, nrow = NULL){

  ## Set number of rows
  compare_rows <- length(plots)
  if (compare_rows == 1) {
    if(plots == "all"){
      compare_rows <- 9
    } else if (plots %in% c("default", "R", "SAS")) {
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

  # Create a boxplot of the residuals if selected in plots otherwise set as NULL
  if("boxplot" %in% plots | "SAS" %in% plots | "all" %in% plots){
    boxplot_list <- list()
    for(i in 1:length(models)){
      boxplot_list[[i]] <- plot_boxplot(type = type,
                                        model = models[[1]],
                                        theme = theme,
                                        axis.text.size = axis.text.size,
                                        title.text.size = title.text.size,
                                        title.opt = title.opt)

    }

  } else{
    boxplot_list <- NULL
  }

  # Create a Cook's D plot if selected in plots otherwise set as NULL
  if("cookd" %in% plots){
    cookd_list <- list()
    for(i in 1:length(models)){
      cookd_list[[i]] <- plot_cookd(model = models[[i]],
                                    theme = theme,
                                    axis.text.size = axis.text.size,
                                    title.text.size = title.text.size,
                                    title.opt = title.opt)
    }

  } else if("all" %in% plots){
    check_count <- 0
    for(i in 1:length(models)){
      if(!(class(models[[i]])[1] %in% c("lme", "lmerMod", "lmerModLmerTest", "glmerMod"))){
        check_count <- check_count+1
      }
    }
    if(check_count == length(models)){
      cookd_list <- list()

      for(i in 1:length(models)){
        cookd_list[[i]] <- plot_cookd(model = models[[i]],
                                      theme = theme,
                                      axis.text.size = axis.text.size,
                                      title.text.size = title.text.size,
                                      title.opt = title.opt)
      }
    }
  } else{
    cookd_list <- NULL
  }

  # Create a histogram of the residuals if selected in plots otherwise set as NULL
  if("hist" %in% plots | "default" %in% plots | "SAS" %in% plots | "all" %in% plots){
    hist_list <- list()
    for(i in 1:length(models)){
      hist_list[[i]] <- plot_hist(model = models[[i]],
                                  type = type,
                                  bins = bins,
                                  theme = theme,
                                  axis.text.size = axis.text.size,
                                  title.text.size = title.text.size,
                                  title.opt = title.opt)
    }


  } else{
    hist_list <- NULL
  }

  # Create an index plot of the residuals if selected in plots otherwise set as NULL
  if("index" %in% plots | "default" %in% plots | "all" %in% plots){
    index_list <- list()
    for(i in 1:length(models)){
      index_list[[i]] <- plot_index(model = models[[i]],
                                    type = type,
                                    theme = theme,
                                    smoother = smoother,
                                    axis.text.size = axis.text.size,
                                    title.text.size = title.text.size,
                                    title.opt = title.opt)
    }

  } else{
    index_list <- NULL
  }

  # Create a residual-leverage plot if selected in plots otherwise set as NULL
  if("lev" %in% plots | "R" %in% plots){
    lev_list <- list()
    for(i in 1:length(models)){
      lev_list[[i]] <- plot_lev(model = models[[i]],
                                type = type,
                                smoother = smoother,
                                theme = theme,
                                axis.text.size = axis.text.size,
                                title.text.size = title.text.size,
                                title.opt = title.opt)
    }


  } else if("all" %in% plots){
    check_count <- 0
    for(i in 1:length(models)){
      if(!(class(models[[i]])[1] %in% c("lme", "lmerMod", "lmerModLmerTest", "glmerMod"))){
        check_count <- check_count+1
      }
    }

    if(check_count == length(models)){
      lev_list <- list()
      for(i in 1:length(models)){
        lev_list[[i]] <- plot_lev(model = models[[i]],
                                  type = type,
                                  smoother = smoother,
                                  theme = theme,
                                  axis.text.size = axis.text.size,
                                  title.text.size = title.text.size,
                                  title.opt = title.opt)
      }
    }
  } else{
    lev_list <- NULL
  }

  # Create a location-scale plot if selected in plots otherwise set as NULL
  if("ls" %in% plots | "R" %in% plots){
    ls_list <- list()
    for(i in 1:length(models)){
      ls_list[[i]] <- plot_ls(model = models[[i]],
                              type = type,
                              smoother = smoother,
                              theme = theme,
                              axis.text.size = axis.text.size,
                              title.text.size = title.text.size,
                              title.opt = title.opt)
    }

  } else if("all" %in% plots){
    check_count <- 0
    for(i in 1:length(models)){
      if(!(class(models[[i]])[1] %in% c("lme", "lmerMod", "lmerModLmerTest", "glmerMod"))){
        check_count <- check_count+1
      }
    }

    if(check_count == length(models)){
      ls_list <- list()
      for(i in 1:length(models)){
        ls_list[[i]] <- plot_ls(model = models[[i]],
                                type = type,
                                smoother = smoother,
                                theme = theme,
                                axis.text.size = axis.text.size,
                                title.text.size = title.text.size,
                                title.opt = title.opt)
      }
    }
  } else{
    ls_list <- NULL
  }

  # Create a q-q plot of the residuals if selected in plots otherwise set as NULL
  if("qq" %in% plots | "default" %in% plots | "SAS" %in% plots | "R" %in% plots | "all" %in% plots){
    qq_list <- list()
    for(i in 1:length(models)){
      qq_list[[i]] <- plot_qq(model = models[[i]],
                              type = type,
                              theme = theme,
                              axis.text.size = axis.text.size,
                              title.text.size = title.text.size,
                              title.opt = title.opt,
                              qqline = qqline,
                              qqbands = qqbands)
    }

  } else{
    qq_list <- NULL
  }

  # Create a residual plot if selected in plots otherwise set as NULL
  if("resid" %in% plots | "default" %in% plots | "SAS" %in% plots | "R" %in% plots | "all" %in% plots){
    resid_list <- list()
    for(i in 1:length(models)){
      resid_list[[i]] <- plot_resid(model = models[[i]],
                                    type = type,
                                    smoother = smoother,
                                    theme = theme,
                                    axis.text.size = axis.text.size,
                                    title.text.size = title.text.size,
                                    title.opt = title.opt)
    }

  } else{
    resid_list <- NULL
  }

  # Create a plot of the response variable vs the predicted values if selected
  # in plots otherwise set as NULL
  if("yvp" %in% plots | "all" %in% plots){
    yvp_list <- list()
    for(i in 1:length(models)){
      yvp_list[[i]] <- plot_yvp(model = models[[i]],
                                theme = theme,
                                axis.text.size = axis.text.size,
                                title.text.size = title.text.size,
                                title.opt = title.opt)
    }


  } else{
    yvp_list <- NULL
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

  # Create a grid of plots based on the plots specified
  if (plots == "default"){

    # Create grid of the default plots
    suppressWarnings(plot_grid(plotlist = c(resid_list, qq_list, index_list, hist_list), scale = scale, nrow = compare_rows))


  } else if (plots == "SAS"){


    # Create grid of the default plots
    suppressWarnings(plot_grid(plotlist = c(resid_list, hist_list, qq_list, boxplot_list), scale = scale, nrow = compare_rows))


  } else if (plots == "R") {


    # Create grid of the default plots
    suppressWarnings(plot_grid(plotlist = c(resid_list, qq_list, ls_list, lev_list), scale = scale, nrow = compare_rows))

  } else if (plots == "all") {

    # Create grid of all plots
    if(class(models)[1] == "lm" | class(models)[1] == "glm"){

      # Create grid of the default plots
      suppressWarnings(plot_grid(plotlist = c(resid_list, index_list, yvp_list, qq_list, hist_list, boxplot_list,
                                              cookd_list, ls_list, lev_list), scale = scale, nrow = compare_rows))

    } else{

      # Create grid of the default plots
      suppressWarnings(plot_grid(plotlist = c(resid_list, index_list, yvp_list, qq_list, hist_list, boxplot_list),
                                              scale = scale, nrow = compare_rows))

    }

  } else if (plots == "individual") {

    # Turn the specified plots into a list
    individual_plots <- list(boxplot = boxplot_list,
                             cookd = cookd_list,
                             hist = hist_list,
                             index = index_list,
                             ls = ls_list,
                             qq = qq_list,
                             lev = lev_list,
                             resid = resid_list,
                             yvp = yvp_list)

    # Select the chosen plots
    individual_plots <- individual_plots[chosen]

    # Creates a sublevel list so need to turn into a list with only one level
    individual_plots_final <- list()
    index <- 1
    for(i in 1:length(chosen)){
      for(j in 1:length(models)){
      individual_plots_final[[index]] <- individual_plots[[i]][[j]]
      index <- index +1
      }
    }

    # Create grid of the default plots
    suppressWarnings(plot_grid(plotlist = individual_plots_final, scale = scale, nrow = compare_rows))

  }


}
