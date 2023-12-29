#' Panel of Interactive Versions of Diagnostic Residual Plots.
#'
#' Creates a panel of interactive residual diagnostic plots given a model. Currently
#' accepts models of type "lm", "glm", "lmerMod", "lmerModLmerTest", "lme", and "glmerMod".
#'
#' @param model Model fit using either \code{lm}, \code{glm}, \code{lmer},
#'   \code{lmerTest}, \code{lme}, or \code{glmer}.
#' @param plots Plots chosen to include in the panel of plots. The default panel
#'   includes a residual plot, a normal quantile plot, an index plot,
#'   and a histogram of the residuals. (See details in the help file
#'   for \code{resid_panel} for the options available.)
#' @param type Type of residuals to use in the plot. If not specified, the
#'   default residual type for each model type is used. (See details in the help file
#'   for \code{resid_panel} for the options available.)
#' @param bins Number of bins to use when creating a histogram of the residuals.
#'   Default is set to 30.
#' @param smoother Indicates whether or not to include a smoother on the residual 
#'      vs fitted and index plots. Specify TRUE or FALSE. Default is set to TRUE.
#' @param qqline Indicates whether to include a 1-1 line on the qq-plot. Specify
#'   TRUE or FALSE. Default is set to TRUE. (The option of \code{qqbands} has not
#'   been implemented in plotly, so it is not available as an option with
#'   \code{resid_interact}.)
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
#' @param alpha Sets the alpha level for displays with points. Default is set to 0.6.
#'
#' @export resid_interact
#'
#' @importFrom plotly ggplotly subplot %>% layout style
#'
#' @details Details on the creation of the plots can be found in the details section of
#'   the help file for \code{resid_panel}.
#'
#' @return A panel of interactive residual diagnostic plots containing plots specified.
#'
#' @examples
#'
#' # Fit a model to the penguin data
#' penguin_model <- lme4::lmer(heartrate ~ depth + duration + (1|bird), data = penguins)
#'
#' # Create the default interactive panel
#' resid_interact(penguin_model)
#' 
#' # Select only the residual plot and qq-plot to be included in the panel,
#' # set the number of rows to 2, change the theme to classic
#' resid_interact(penguin_model, plots = c("resid", "qq"), nrow = 2, theme = "classic")

resid_interact <- function(model, plots = "default", type = NA, bins = 30,
                           smoother = TRUE, qqline = TRUE, scale = 0.9,
                           theme = "bw", axis.text.size = 10, title.text.size = 12,
                           title.opt = TRUE, nrow = NULL, alpha = 0.6){

  ## Errors and Warnings -------------------------------------------------------

  # Checks that return an error
  check_modeltype(model = model)
  check_residualtype(model = model, type = type)
  check_standardized(model = model, plots = plots)
  check_cooksd(model = model, plots = plots)

  # Checks that return a warning
  smoother <- check_smoother(smoother = smoother)
  theme <- check_theme(theme = theme)
  title.opt <- check_title(title.opt = title.opt)
  check_leverage(model = model, plots = plots)

  ## Creation of plot ---------------------------------------------------------

  # Create the requested interactive plots

  # Boxplot
  if("boxplot" %in% plots | "SAS" %in% plots | "all" %in% plots){
    boxplot <- plot_boxplot(model = model,
                            type = type,
                            theme = theme,
                            axis.text.size = axis.text.size,
                            title.text.size = title.text.size,
                            title.opt = title.opt)
    if(title.opt == TRUE) {
      title = helper_plotly_title(boxplot)
      boxplot <- ggplotly(boxplot, tooltip = c("Residual", "Data")) %>%
        layout(annotations = title, title = FALSE)
    } else{
      boxplot <- ggplotly(boxplot, tooltip = c("Residual", "Data"))
    }
  } else{
    boxplot <- NULL
  }

  # Cook's D plot
  if("cookd" %in% plots){
    cookd <- plot_cookd(model = model,
                        theme = theme,
                        axis.text.size = axis.text.size,
                        title.text.size = title.text.size,
                        title.opt = title.opt,
                        alpha = alpha)
    if(title.opt == TRUE){
      title = helper_plotly_title(cookd)
      cookd <- style(ggplotly(cookd, tooltip = c("y", "Data")), hoverinfo = "skip", traces = 2) %>%
        layout(annotations = title, title = FALSE)


    } else{
      cookd <- style(ggplotly(cookd, tooltip = c("y", "Data")), hoverinfo = "skip", traces = 2)
    }
  } else if("all" %in% plots & !(class(model)[1] %in% c("lme", "lmerMod", "lmerModLmerTest", "glmerMod"))){
    cookd <- plot_cookd(model = model,
                        theme = theme,
                        axis.text.size = axis.text.size,
                        title.text.size = title.text.size,
                        title.opt = title.opt,
                        alpha = alpha)
    if(title.opt == TRUE){
      title = helper_plotly_title(cookd)
      cookd <- ggplotly(cookd, tooltip = c("y", "Data")) %>%
        layout(annotations = title, title = FALSE)
    } else{
      cookd <- ggplotly(cookd, tooltip = c("y", "Data"))
    }
  } else{
    cookd <- NULL
  }

  # Histogram
  if("hist" %in% plots | "default" %in% plots | "SAS" %in% plots | "all" %in% plots){
    hist <- plot_hist(model = model,
                      type = type,
                      bins = bins,
                      theme = theme,
                      axis.text.size = axis.text.size,
                      title.text.size = title.text.size,
                      title.opt = title.opt)
    if(title.opt == TRUE){
      title = helper_plotly_title(hist)
      hist <- ggplotly(hist, tooltip = c("Data", "density", "fill")) %>%
        layout(annotations = title, title = FALSE)
    } else{
      hist <- ggplotly(hist, tooltip = c("Data", "density", "fill"))
    }
  } else{
    hist <- NULL
  }

  # Index
  if("index" %in% plots | "default" %in% plots | "all" %in% plots){
    index <- plot_index(model = model,
                        type = type,
                        smoother = smoother,
                        theme = theme,
                        axis.text.size = axis.text.size,
                        title.text.size = title.text.size,
                        title.opt = title.opt,
                        alpha = alpha)
    if(title.opt == TRUE){
      title = helper_plotly_title(index)
      index <- ggplotly(index, tooltip = c("Observation", "Residual", "Data")) %>%
        layout(annotations = title, title = FALSE)
    } else{
      index <- ggplotly(index,tooltip = c("Observation", "Residual", "Data"))
    }
  } else{
    index <- NULL
  }

  # Leverage plot
  if("lev" %in% plots | "R" %in% plots){
    lev <- plot_lev(model = model,
                    type = type,
                    smoother = smoother,
                    theme = theme,
                    axis.text.size = axis.text.size,
                    title.text.size = title.text.size,
                    title.opt = title.opt,
                    alpha = alpha)
    if(title.opt == TRUE){
      title = helper_plotly_title(lev)
      lev <- suppressWarnings(style(ggplotly(lev, tooltip = c("Leverage", "Std_Res", "CooksD", "Data")), hoverinfo = "skip", traces = c(6))) %>%
        layout(annotations = title, title = FALSE)
    } else{
      lev <- suppressWarnings(style(ggplotly(lev, tooltip = c("Leverage", "Std_Res", "CooksD", "Data")), hoverinfo = "skip", traces = c(6)))
    }
  } else if("all" %in% plots & !(class(model)[1] %in% c("lme", "lmerMod", "lmerModLmerTest", "glmerMod"))){
    lev <- plot_lev(model = model,
                    type = type,
                    smoother = smoother,
                    theme = theme,
                    axis.text.size = axis.text.size,
                    title.text.size = title.text.size,
                    title.opt = title.opt,
                    alpha = alpha)
    if(title.opt == TRUE){
      title = helper_plotly_title(lev)
      lev <- suppressWarnings(ggplotly(lev, tooltip = c("Leverage", "Std_Res", "CooksD")) %>%
        layout(annotations = title, title = FALSE))
    } else{
      lev <- suppressWarnings(ggplotly(lev, tooltip = c("Leverage", "Std_Res", "CooksD")))
    }
  } else{
    lev <- NULL
  }

  # Location-Scale plot
  if("ls" %in% plots | "R" %in% plots){
    ls <- plot_ls(model = model,
                  type = type,
                  smoother = smoother,
                  theme = theme,
                  axis.text.size = axis.text.size,
                  title.text.size = title.text.size,
                  title.opt = title.opt,
                  alpha = alpha)
    if(title.opt == TRUE){
      title = helper_plotly_title(ls)
      if (class(model)[1] == "lm"){
        ls <- suppressWarnings(ggplotly(ls + labs(x = "Predicted Values", y = "sqrt(|Standardized Residuals|)"),
                       tooltip = c("Prediction", "Sqrt_Std_Res", "Data"))) %>%
          layout(annotations = title, title = FALSE)
      } else if(is.na(type) | type == "deviance" | type == "stand.deviance"){
        ls <- suppressWarnings(ggplotly(ls + labs(x = "Predicted Values",
                                 y = "sqrt(|Standardized Deviance Residuals|)"),
                       tooltip = c("Prediction", "Sqrt_Std_Res", "Data"))) %>%
          layout(annotations = title, title = FALSE)
      } else if(type == "pearson" | type == "stand.pearson"){
        ls <- suppressWarnings(ggplotly(ls + labs(x = "Predicted Values",
                                 y = "sqrt(|Standardized Pearson Residuals|)"),
                       tooltip = c("Prediction", "Sqrt_Std_Res", "Data"))) %>%
          layout(annotations = title, title = FALSE)
      }
    } else {
      if (class(model)[1] == "lm"){
        ls <- suppressWarnings(ggplotly(ls + labs(x = "Predicted Values", y = "sqrt(|Standardized Residuals|)"),
                       tooltip=c("Prediction", "Sqrt_Std_Res", "Data")))
      } else if(is.na(type) | type == "deviance" | type == "stand.deviance"){
        ls <- suppressWarnings(ggplotly(ls + labs(x = "Predicted Values",
                                 y = "sqrt(|Standardized Deviance Residuals|)"),
                       tooltip = c("Prediction", "Sqrt_Std_Res", "Data")))
      } else if(type == "pearson" | type == "stand.pearson"){
        ls <- suppressWarnings(ggplotly(ls + labs(x = "Predicted Values",
                                 y = "sqrt(|Standardized Pearson Residuals|)"),
                       tooltip = c("Prediction", "Sqrt_Std_Res", "Data")))
      }
    }
  } else if("all" %in% plots & !(class(model)[1] %in% c("lme", "lmerMod", "lmerModLmerTest", "glmerMod"))){
    ls <- plot_ls(model = model,
                  type = type,
                  smoother = smoother,
                  theme = theme,
                  axis.text.size = axis.text.size,
                  title.text.size = title.text.size,
                  title.opt = title.opt,
                  alpha = alpha)
    if(title.opt == TRUE){
      title = helper_plotly_title(ls)
      if (class(model)[1] == "lm"){
        ls <- suppressWarnings(ggplotly(ls + labs(x = "Predicted Values", y = "sqrt(|Standardized Residuals|)"),
                       tooltip=c("Prediction", "Sqrt_Std_Res", "Data"))) %>%
          layout(annotations = title, title = FALSE)
      } else if(is.na(type) | type == "deviance" | type == "stand.deviance"){
        ls <- suppressWarnings(ggplotly(ls + labs(x = "Predicted Values",
                                 y = "sqrt(|Standardized Deviance Residuals|)"),
                       tooltip = c("Prediction", "Sqrt_Std_Res", "Data"))) %>%
          layout(annotations = title, title = FALSE)
      } else if(type == "pearson" | type == "stand.pearson"){
        ls <- suppressWarnings(ggplotly(ls + labs(x = "Predicted Values",
                                 y = "sqrt(|Standardized Pearson Residuals|)"),
                       tooltip = c("Prediction", "Sqrt_Std_Res", "Data"))) %>%
          layout(annotations = title, title = FALSE)
      }
    } else{
      if (class(model)[1] == "lm"){
        ls <- suppressWarnings(ggplotly(ls + labs(x = "Predicted Values", y = "sqrt(|Standardized Residuals|)"),
                       tooltip=c("Prediction", "Sqrt_Std_Res", "Data")))
      } else if(is.na(type) | type == "deviance" | type == "stand.deviance"){
        ls <- suppressWarnings(ggplotly(ls + labs(x = "Predicted Values",
                                 y = "sqrt(|Standardized Deviance Residuals|)"),
                       tooltip = c("Prediction", "Sqrt_Std_Res", "Data")))
      } else if(type == "pearson" | type == "stand.pearson"){
        ls <- suppressWarnings(ggplotly(ls + labs(x = "Predicted Values",
                                 y = "sqrt(|Standardized Pearson Residuals|)"),
                       tooltip = c("Prediction", "Sqrt_Std_Res", "Data")))
      }
    }
  } else{
    ls <- NULL
  }

  # QQ plot
  if("qq" %in% plots | "default" %in% plots | "SAS" %in% plots | "R" %in% plots | "all" %in% plots){
    qq <- plot_qq(model = model,
                  type = type,
                  theme = theme,
                  axis.text.size = axis.text.size,
                  title.text.size = title.text.size,
                  title.opt = title.opt,
                  qqline = qqline,
                  qqbands = FALSE,
                  alpha = alpha)
    if(title.opt == TRUE){
      title = helper_plotly_title(qq)
      qq <- ggplotly(qq, tooltip = c("Data", "Residual", "Theoretical")) %>%
        layout(annotations = title, title = FALSE)
    } else {
      qq <- ggplotly(qq, tooltip = c("Data", "Residual", "Theoretical"))
    }
  } else{
    qq <- NULL
  }

  # Residual plot
  if("resid" %in% plots | "default" %in% plots | "SAS" %in% plots | "R" %in% plots | "all" %in% plots){
    resid <- plot_resid(model = model,
                        type = type,
                        smoother = smoother,
                        theme = theme,
                        axis.text.size = axis.text.size,
                        title.text.size = title.text.size,
                        title.opt = title.opt,
                        alpha = alpha)
    if(title.opt == TRUE){
      title = helper_plotly_title(resid)
      resid <- ggplotly(resid, tooltip = c("Prediction", "Residual", "Data")) %>%
        layout(annotations = title, title = FALSE)
    } else {
      resid <- ggplotly(resid, tooltip = c("Prediction", "Residual", "Data"))
    }
  } else{
    resid <- NULL
  }

  # Response vs Predicted plot
  if("yvp" %in% plots | "all" %in% plots){
    yvp <- plot_yvp(model = model,
                    theme = theme,
                    axis.text.size = axis.text.size,
                    title.text.size = title.text.size,
                    title.opt = title.opt,
                    alpha = alpha)
    if(title.opt == TRUE){
      title = helper_plotly_title(yvp)
      yvp <- ggplotly(yvp) %>%
        layout(annotations = title, title = FALSE)
    } else {
      yvp <- ggplotly(yvp)
    }
  } else{
    yvp <- NULL
  }

  ## Creation of interactive plot -------------------------------------------------

  # Use plotly to create interactive plot requested

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
    stop("Invalid plots option entered. See the resid_interact help file for
         available options.")
  }

  # Create a grid of plots based on the plots specified
  if (plots == "default"){

    # Determine the number of rows in the panel
    if (is.null(nrow)) nrow = 2

    # Create grid of the default plots
    subplot(resid, qq, index, hist,
            nrows = nrow, titleX = TRUE, titleY = TRUE, margin = 1 - scale)

  } else if (plots == "SAS"){

    # Determine the number of rows in the panel
    if (is.null(nrow)) nrow = 2

    # Create grid of SAS plots
    subplot(resid, hist, qq, boxplot,
            nrows = nrow, titleX = TRUE, titleY = TRUE, margin = 1 - scale)

  } else if (plots == "R") {

    # Determine the number of rows in the panel
    if (is.null(nrow)) nrow = 2

    # Create grid of R plots
    subplot(resid, qq, ls, lev,
            nrows = nrow, titleX = TRUE, titleY = TRUE, margin = 1 - scale)

  } else if (plots == "all") {

    # Create grid of all plots
    if(class(model)[1] == "lm" | class(model)[1] == "glm"){

      # Determine the number of rows in the panel
      if (is.null(nrow)) nrow = 3

      # Create the panel
      subplot(resid, index, yvp,
              qq, hist, boxplot,
              cookd, ls, lev,
              nrows = nrow, titleX = TRUE, titleY = TRUE, margin = 1 - scale)

    } else{

      # Determine the number of rows in the panel
      if (is.null(nrow)) nrow = 2

      # Create the panel
      subplot(resid, index, yvp,
              qq, hist, boxplot,
              nrows = nrow, titleX = TRUE, titleY = TRUE, margin = 1 - scale)

    }

  } else if (plots == "individual") {

    # Turn the specified plots into a list
    individual_plots <- list(boxplot = boxplot,
                             cookd = cookd,
                             hist = hist,
                             index = index,
                             ls = ls,
                             qq = qq,
                             lev = lev,
                             resid = resid,
                             yvp = yvp)

    # Select the chosen plots
    individual_plots <- individual_plots[chosen]

    # Determine the number of rows in the panel
    if (is.null(nrow) & length(individual_plots) %in% 1:3){
      nrow = 1
    } else if (is.null(nrow) & length(individual_plots) > 3){
      nrow = 2
    }

    # Create grid of individual plots specified
    subplot(individual_plots,
            nrows = nrow, titleX = TRUE, titleY = TRUE, margin = 1 - scale)

  }

}
