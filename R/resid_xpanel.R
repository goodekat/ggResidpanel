#' Panel of Plots of Residuals or Response Variable versus Predictor Variables.
#'
#' Creates a panel of plots of the residuals or response variable versus the
#' predictor (x) variables in the model. Interactions between predictor variables
#' are not included. Currently accepts models of type "lm", "glm", "lmerMod",
#' "lmerModLmerTest", "lme", and "glmerMod".
#'
#' @param model Model fit using either \code{lm}, \code{glm}, \code{lmer},
#'   \code{lmerTest}, \code{lme}, or \code{glmer}.
#' @param yvar Specifies the variable to put on the y-axis of the plots. Options
#'   are "residual" (default) or "response".
#' @param type Type of residuals to use in the plot. If not specified, the
#'   default residual type for each model type is used. (See details for the
#'   options available.)
#' @param smoother Indicates whether or not to include a smoother on the
#'   plots. Specify TRUE or FALSE. Default is set to FALSE.
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
#' @param jitter.width Specifies the amount of jitter to add in the plots of categorical variables. (Default is 0.)
#' @param alpha Sets the alpha level for displays with points. Default is set to 0.6.
#' 
#' @export resid_xpanel
#'
#' @importFrom grid textGrob gpar
#' @importFrom ggplot2 geom_jitter geom_violin
#'
#' @details Note that for x variables that are factors, the levels shown on the
#' x-axis will be in the order that the levels are ordered in the dataframe.
#' This can be adjusted by reordering the levels of the factor before the model
#' is fit.
#'
#' @return A panel of plots of the residuals or response variable versus the
#' predictor variables. Violin plots are included with categorical variables. 
#'
#' @examples
#'
#' # Fit a model to the penguin data
#' penguin_model <- lme4::lmer(heartrate ~ depth + duration + (1|bird), data = penguins)
#'
#' # Create plots of the residuals versus the predictor variables
#' resid_xpanel(penguin_model, theme = "classic", jitter.width = 0.1)
#'
#' # Create plots of the response variable versus the predictor variables
#' resid_xpanel(
#'      model = penguin_model, 
#'      yvar = "response", 
#'      theme = "classic",
#'       smoother = TRUE, 
#'       jitter.width = 0.1
#' )
#' 

resid_xpanel <- function(model, yvar = "residual", type = NA,
                         smoother = FALSE, scale = 1, theme = "bw",
                         axis.text.size = 10, title.text.size = 12,
                         title.opt = TRUE, nrow = NULL, jitter.width = 0, 
                         alpha = 0.6){

  ## Errors and Warnings -------------------------------------------------------

  # Checks that return an error
  check_modeltype(model = model)
  check_residualtype(model = model, type = type)

  # Checks that return a warning
  smoother <- check_smoother(smoother = smoother)
  theme <- check_theme(theme = theme)
  title.opt <- check_title(title.opt = title.opt)

  ## Creation of Plots and Grid ------------------------------------------------

  # Create a data frame with the residuals
  if(is.na(type)){
    residuals <- helper_resid(type = NA, model = model)
  } else{
    residuals <- helper_resid(type = type, model = model)
  }

  # Create model data based on the type of model
  if (class(model)[1] %in% c("lm")){
    model_data <- data.frame(Residual = residuals, model$model)
  } else if (class(model)[1] == "glm"){
    if (model$family[[1]] == "binomial"){
      model_data <- data.frame(Residual = residuals,
                               proportion = helper_glm_actual(model),
                               model$model[-1])
    } else {
      model_data <- data.frame(Residual = residuals, model$model)
    }
  } else if (class(model)[1] %in% c("lmerMod", "lmerModLmerTest")){
    model_data <- cbind(Residual = residuals, model@frame)
  } else if (class(model)[1] == "lme"){
    model_data <- cbind(Residual = residuals, model$data)
  } else if (class(model)[1] == "glmerMod"){
    if (model@resp$family[[1]] == "binomial") {
      model_data <- data.frame(Residual = residuals,
                               proportion = helper_glm_actual(model),
                               model@frame[-1])
    } else {
      model_data <- cbind(Residual = residuals, model@frame)
    }
  }

  # Determine the column number of the data to use based on yvar chosen
  if (yvar == "residual"){
    y_column_number = 1
  } else if (yvar == "response"){
    y_column_number = 2
  } else {
    stop("The value specified for yvar is not a valid option. The options are either
            residual or response.")
  }

  # Create the predictor plots
  predictor_plots <- lapply(3:dim(model_data)[2],
                            FUN = create_predictor_plots,
                            y_column_number = y_column_number,
                            data = model_data,
                            type = type,
                            model = model,
                            smoother = smoother,
                            theme = theme,
                            axis.text.size = axis.text.size, 
                            jitter.width = jitter.width,
                            alpha = alpha)

  # Create the panel of the predictor plots
  predictor_panel <- suppressWarnings(plot_grid(plotlist = predictor_plots, scale = scale, nrow = nrow))

  # Add a title if requested and return the panel
  if(title.opt == TRUE){

    # Create the title
    yname <- ifelse(yvar == "residual", "Residuals", "Response Variable")
    title <- ggdraw() +
      draw_label(paste("Plots of", yname, "vs Predictor Variables"),
                 fontface = 'bold',
                 size = title.text.size)

    # Create and return a panel with the title and the plots
    plot_grid(title, predictor_panel, nrow = 2, rel_heights = c(0.1, 1))


  } else {

    # Create and return a panel with only the plots
    predictor_panel

  }

}

# Function for creating a scatter plot of the chosen yvar vs a predictor variable
create_predictor_plots <- function(x_column_number, y_column_number,
                                   data, type, model, smoother, theme,
                                   axis.text.size, jitter.width, alpha){

  # Create axis labels
  xlab <- names(data)[x_column_number]
  if(y_column_number == 1){
    ylab <- helper_label(type = type, model = model)
  } else if (y_column_number == 2){
    ylab <- names(data)[y_column_number]
  }

  # Subset the data to contain the residuals or response and one predictor
  data_sub <- data.frame(y = data[,y_column_number],
                         x = data[,x_column_number])

  # Create the plot (use violin plot if variable is a factor/character)
  if (is.factor(data[,x_column_number]) | is.character(data[,x_column_number])) {
    plot <- ggplot(data_sub, aes(x = data_sub$x, y = data_sub$y)) +
      geom_violin() +
      geom_jitter(width = jitter.width, alpha = alpha) +
      theme_bw() +
      labs(x = xlab, y = ylab) 
  } else {
    plot <- ggplot(data_sub, aes(x = data_sub$x, y = data_sub$y)) +
      geom_point(alpha = alpha) +
      theme_bw() +
      labs(x = xlab, y = ylab)
  }
  
  # Add a horizontal line if plotting residuals
  if (y_column_number == 1){
    plot <- plot + geom_hline(yintercept = 0, color = "blue")
  }

  # Add a smoother to the plots if requested
  if(smoother == TRUE){
    plot <- 
      plot + 
      geom_smooth(
        method = "loess", 
        se = FALSE, 
        color = "red", 
        linewidth = 0.5,
        formula = 'y ~ x'
      )
  }

  # Add theme to plot
  if (theme == "bw"){
    plot <- plot + theme_bw()
  } else if (theme == "classic"){
    plot <- plot + theme_classic()
  } else if (theme == "gray" | theme == "grey"){
    plot <- plot + theme_grey()
  }

  # Set text size of axis labels and return plot
  plot + theme(axis.title = element_text(size = axis.text.size))

}
