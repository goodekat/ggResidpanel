#' Interaction Versions of Residual Diagnostics Plots.
#'
#' Creates interactive versions of all residual diagnostic plots given a model.
#' Currently accepts models of type "lm", "glm", "lmerMod", "lmerModLmerTest", and "glmerMod".
#'
#' @param model Model fit using either \code{lm}, \code{glm}, \code{lmer},
#'   \code{lmerTest}, or \code{glmer}.
#' @param plots Plots chosen to include in the panel of plots. Default is set to
#'   "default". (See details for options.)
#' @param type The user may specify a type of residuals to use. Otherwise, the
#'   default residual type for each model is used. (See details in the help file
#'   for \code{resid_panel} for options.)
#' @param bins Number of bins to use when creating a histogram of the residuals.
#' @param smoother Indicates whether or not to include a smoother on the
#'   residual plot. Specify TRUE or FALSE. Default is set to FALSE.
#' @param qqline Indicates whether to include a 1-1 line on the qq-plot. Specify
#'   TRUE or FALSE. Default is set to TRUE.
#' @param theme ggplot2 theme to be used. Current options are \code{"bw"},
#'   \code{"classic"}, and \code{"grey"} (or \code{"gray"}). Default is
#'   \code{"bw"}.
#' @param axis.text.size Specifies the size of the text for the axis labels of
#'   all plots in the panel.
#' @param title.text.size Specifies the size of the text for the titles of all
#'   plots in the panel.
#' @param title.opt Indicates whether or not to include a title on the plots in
#'   the panel. Specify TRUE or FALSE. Default is set to TRUE.
#'
#' @export
#'
#' @importFrom plotly ggplotly subplot %>% layout
#'
#' @details Currently, only one plot can be made interactive at a time. The
#'   options are as follows.
#'   \itemize{
#'   \item \code{"boxplot"}: A boxplot of residuals
#'   \item \code{"cookd"}: A plot of Cook's D values versus observation numbers
#'   \item \code{"hist"}: A histogram of residuals
#'   \item \code{"index"}: A plot of residuals versus observation numbers
#'   \item \code{"ls"}: A location scale plot of the residuals
#'   \item \code{"qq"}: A normal quantile plot of residuals
#'   \item \code{"lev"}: A plot of leverage values versus residuals
#'   \item \code{"resid"}: A plot of residuals versus predicted values
#'   \item \code{"yvp":}: A plot of observed response values versus predicted values
#'   }
#'
#'   Note: \code{"cookd"}, \code{"ls"}, and \code{"lev"} are not available for "
#'   lmer", "lmerTest", and "glmer" models.
#'
#'   Details on the creation of the plots can be found in the details section of
#'   the help file for \code{resid_panel}.
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
#' resid_interact(lmer_model, plot = "hist", title.opt = FALSE)
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

resid_interact <- function(model, plots = "default", type = NA, bins = NA,
                           smoother = FALSE, qqline = TRUE, theme = "bw",
                           axis.text.size = 10, title.text.size = 12,
                           title.opt = TRUE, nrow = NULL, scale = 0.9){

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
  bins <- check_bins(plots = plots, bins = bins)
  check_leverage(model = model, plots = plots)

  # Print a warning about not using the dev version of ggplot2 with ggplotly
  warning("Some of the interactive plots do not function with the dev version of
          ggplot from github. To achieve full functionallity, please install ggplot2
          from CRAN.")

  ## Creation of plot ---------------------------------------------------------

  # Create the requested interactive plots

  # Boxplot
  if("boxplot" %in% plots | "SAS" %in% plots | "all" %in% plots){
    boxplot <- resid_boxplot(model = model,
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
    cookd <- resid_cookd(model = model,
                          theme = theme,
                          axis.text.size = axis.text.size,
                          title.text.size = title.text.size,
                          title.opt = title.opt)
    if(title.opt == TRUE){
      title = helper_plotly_title(cookd)
      cookd <- ggplotly(cookd, tooltip = c("CooksD", "Data")) %>%
        layout(annotations = title, title = FALSE)
    } else{
      cookd <- ggplotly(cookd, tooltip = c("CooksD", "Data"))
    }
  } else if("all" %in% plots & !(class(model)[1] %in% c("lmerMod", "lmerModLmerTest", "glmerMod"))){
    cookd <- resid_cookd(model = model,
                         theme = theme,
                         axis.text.size = axis.text.size,
                         title.text.size = title.text.size,
                         title.opt = title.opt)
    if(title.opt == TRUE){
      title = helper_plotly_title(cookd)
      cookd <- ggplotly(cookd, tooltip = c("CooksD", "Data")) %>%
        layout(annotations = title, title = FALSE)
    } else{
      cookd <- ggplotly(cookd, tooltip = c("CooksD", "Data"))
    }
  } else{
    cookd <- NULL
  }

  # Histogram
  if("hist" %in% plots | "default" %in% plots | "SAS" %in% plots | "all" %in% plots){
    hist <- resid_hist(model = model,
                         type = type,
                         bins = bins,
                         theme = theme,
                         axis.text.size = axis.text.size,
                         title.text.size = title.text.size,
                         title.opt = title.opt)
    if(title.opt == TRUE){
      title = helper_plotly_title(hist)
      hist <- ggplotly(hist, tooltip = c("Data", "count")) %>%
        layout(annotations = title, title = FALSE)
    } else{
      hist <- ggplotly(hist, tooltip = c("Data", "count"))
    }
  } else{
    hist <- NULL
  }

  # Index
  if("index" %in% plots | "default" %in% plots | "all" %in% plots){
    index <- resid_index(model = model,
                          type = type,
                          theme = theme,
                          axis.text.size = axis.text.size,
                          title.text.size = title.text.size,
                          title.opt = title.opt)
    if(title.opt == TRUE){
      title = helper_plotly_title(index)
      index <- ggplotly(index) %>%
        layout(annotations = title, title = FALSE)
    } else{
      index <- ggplotly(index)
    }
  } else{
    index <- NULL
  }

  # Leverage plot
  if("lev" %in% plots | "R" %in% plots){
    lev <- resid_lev(model = model,
                     type = type,
                     theme = theme,
                     axis.text.size = axis.text.size,
                     title.text.size = title.text.size,
                     title.opt = title.opt)
    if(title.opt == TRUE){
      title = helper_plotly_title(lev)
      lev <- ggplotly(lev, tooltip = c("Leverage", "Std_Res", "Data")) %>%
        layout(annotations = title, title = FALSE)
    } else{
      lev <- ggplotly(lev, tooltip = c("Leverage", "Std_Res", "Data"))
    }
  } else if("all" %in% plots & !(class(model)[1] %in% c("lmerMod", "lmerModLmerTest", "glmerMod"))){
    lev <- resid_lev(model = model,
                     type = type,
                     theme = theme,
                     axis.text.size = axis.text.size,
                     title.text.size = title.text.size,
                     title.opt = title.opt)
    if(title.opt == TRUE){
      title = helper_plotly_title(lev)
      lev <- ggplotly(lev, tooltip = c("Leverage", "Std_Res", "Data")) %>%
        layout(annotations = title, title = FALSE)
    } else{
      lev <- ggplotly(lev, tooltip = c("Leverage", "Std_Res", "Data"))
    }
  } else{
    lev <- NULL
  }

  # Location-Scale plot
  if("ls" %in% plots | "R" %in% plots){
    ls <- resid_ls(model = model,
                   type = type,
                   theme = theme,
                   axis.text.size = axis.text.size,
                   title.text.size = title.text.size,
                   title.opt = title.opt)
    if(title.opt == TRUE){
      title = helper_plotly_title(ls)
      if (class(model)[1] == "lm"){
        ls <- ggplotly(ls + labs(x = "Predicted Values", y = "sqrt(|Standardized Residuals|)"),
                       tooltip=c("Prediction", "Sqrt_Std_Res", "Data")) %>%
          layout(annotations = title, title = FALSE)
      } else if(is.na(type) | type == "deviance" | type == "stand.deviance"){
        ls <- ggplotly(ls + labs(x = "Predicted Values",
                                 y = "sqrt(|Standardized Deviance Residuals|)"),
                       tooltip = c("Prediction", "Sqrt_Std_Res", "Data")) %>%
          layout(annotations = title, title = FALSE)
      } else if(type == "pearson" | type == "stand.pearson"){
        ls <- ggplotly(ls + labs(x = "Predicted Values",
                                 y = "sqrt(|Standardized Pearson Residuals|)"),
                       tooltip = c("Prediction", "Sqrt_Std_Res", "Data")) %>%
          layout(annotations = title, title = FALSE)
      }
    } else {
      if (class(model)[1] == "lm"){
        ls <- ggplotly(ls + labs(x = "Predicted Values", y = "sqrt(|Standardized Residuals|)"),
                       tooltip=c("Prediction", "Sqrt_Std_Res", "Data"))
      } else if(is.na(type) | type == "deviance" | type == "stand.deviance"){
        ls <- ggplotly(ls + labs(x = "Predicted Values",
                                 y = "sqrt(|Standardized Deviance Residuals|)"),
                       tooltip = c("Prediction", "Sqrt_Std_Res", "Data"))
      } else if(type == "pearson" | type == "stand.pearson"){
        ls <- ggplotly(ls + labs(x = "Predicted Values",
                                 y = "sqrt(|Standardized Pearson Residuals|)"),
                       tooltip = c("Prediction", "Sqrt_Std_Res", "Data"))
      }
    }
  } else if("all" %in% plots & !(class(model)[1] %in% c("lmerMod", "lmerModLmerTest", "glmerMod"))){
    ls <- resid_ls(model = model,
                   type = type,
                   theme = theme,
                   axis.text.size = axis.text.size,
                   title.text.size = title.text.size,
                   title.opt = title.opt)
    if(title.opt == TRUE){
      title = helper_plotly_title(ls)
      if (class(model)[1] == "lm"){
        ls <- ggplotly(ls + labs(x = "Predicted Values", y = "sqrt(|Standardized Residuals|)"),
                       tooltip=c("Prediction", "Sqrt_Std_Res", "Data")) %>%
          layout(annotations = title, title = FALSE)
      } else if(is.na(type) | type == "deviance" | type == "stand.deviance"){
        ls <- ggplotly(ls + labs(x = "Predicted Values",
                                 y = "sqrt(|Standardized Deviance Residuals|)"),
                       tooltip = c("Prediction", "Sqrt_Std_Res", "Data")) %>%
          layout(annotations = title, title = FALSE)
      } else if(type == "pearson" | type == "stand.pearson"){
        ls <- ggplotly(ls + labs(x = "Predicted Values",
                                 y = "sqrt(|Standardized Pearson Residuals|)"),
                       tooltip = c("Prediction", "Sqrt_Std_Res", "Data")) %>%
          layout(annotations = title, title = FALSE)
      }
    } else{
      if (class(model)[1] == "lm"){
        ls <- ggplotly(ls + labs(x = "Predicted Values", y = "sqrt(|Standardized Residuals|)"),
                       tooltip=c("Prediction", "Sqrt_Std_Res", "Data"))
      } else if(is.na(type) | type == "deviance" | type == "stand.deviance"){
        ls <- ggplotly(ls + labs(x = "Predicted Values",
                                 y = "sqrt(|Standardized Deviance Residuals|)"),
                       tooltip = c("Prediction", "Sqrt_Std_Res", "Data"))
      } else if(type == "pearson" | type == "stand.pearson"){
        ls <- ggplotly(ls + labs(x = "Predicted Values",
                                 y = "sqrt(|Standardized Pearson Residuals|)"),
                       tooltip = c("Prediction", "Sqrt_Std_Res", "Data"))
      }
    }
  } else{
    ls <- NULL
  }

  # QQ plot
  if("qq" %in% plots | "default" %in% plots | "SAS" %in% plots | "R" %in% plots | "all" %in% plots){
    qq <- resid_qq(model = model,
                       type = type,
                       theme = theme,
                       axis.text.size = axis.text.size,
                       title.text.size = title.text.size,
                       title.opt = title.opt,
                       qqline = qqline,
                       qqbands = FALSE)
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
    resid <- resid_plot(model = model,
                         type = type,
                         smoother = smoother,
                         theme = theme,
                         axis.text.size = axis.text.size,
                         title.text.size = title.text.size,
                         title.opt = title.opt)
    if(title.opt == TRUE){
      title = helper_plotly_title(resid)
      resid <- ggplotly(resid) %>%
        layout(annotations = title, title = FALSE)
    } else {
      resid <- ggplotly(resid)
    }
  } else{
    resid <- NULL
  }

  # Response vs Predicted plot
  if("yvp" %in% plots | "all" %in% plots){
    yvp <- resid_yvp(model = model,
                            theme = theme,
                            axis.text.size = axis.text.size,
                            title.text.size = title.text.size,
                            title.opt = title.opt)
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
      subplot(resid, hist, qq, boxplot, cookd, ls, lev, yvp, index,
              nrows = nrow, titleX = TRUE, titleY = TRUE, margin = 1 - scale)

    } else{

      # Determine the number of rows in the panel
      if (is.null(nrow)) nrow = 2

      # Create the panel
      subplot(resid, hist, qq, boxplot, yvp, index,
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
