#'
#' @param nrow Sets the number of rows in the panel.
#'
#' @export resid_xpanel
#'
#' @importFrom grid textGrob gpar
#'
resid_xpanel <- function(model, yvar = "residual", type = NA,
                         smoother = FALSE, scale = 1, theme = "bw",
                         axis.text.size = 10, title.text.size = 12,
                         title.opt = TRUE, nrow = NULL){

  ## Errors and Warnings -------------------------------------------------------

  # Checks that return an error
  check_modeltype(model = model)
  check_residualtype(model = model, type = type)
  check_standardized(model = model, plots = plots)

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
  if (class(model)[1] %in% c("lm", "glm")){
      model_data <- data.frame(Residual = residuals, model$model)
  } else if (class(model)[1] %in% c("lmerMod", "lmerModLmerTest", "glmerMod")){
    model_data <- cbind(Residual = residuals, model@frame)
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
                            axis.text.size = axis.text.size)

  # Create the panel of the predictor plots
  predictor_panel <- plot_grid(plotlist = predictor_plots, scale = scale, nrow = nrow)

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
                                   axis.text.size){

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

  # Create the plot
  if(y_column_number == 1){
    plot <- ggplot(data_sub, aes(x = x, y = y)) +
      geom_point() +
      geom_hline(yintercept = 0, color = "blue") +
      theme_bw() +
      labs(x = xlab, y = ylab)
  } else if (y_column_number == 2){
    plot <- ggplot(data_sub, aes(x = x, y = y)) +
      geom_point() +
      theme_bw() +
      labs(x = xlab, y = ylab)
  }

  # Add a smoother to the plots if requested
  if(smoother == TRUE){
    plot <- plot + geom_smooth(method = "loess", se = FALSE, color = "red", size = 0.5)
  }

  # Add theme to plot
  if (theme == "bw"){
    plot <- plot + theme_bw()
  } else if (theme == "classic"){
    plot <- plot + theme_classic()
  } else if (theme == "gray" | theme == "grey"){
    plot <- plot + theme_grey()
  }

  # Set text size of axis lables and return plot
  plot + theme(axis.title = element_text(size = axis.text.size))

}
