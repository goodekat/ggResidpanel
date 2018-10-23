#' @export predictor_panel

## The start of a function for create a panel of plots with the response vs predictors

# To do:
#  - add code for lmer models
#  - add options
#  - add a title (?)
#  - add documentation

predictor_panel <- function(model, type = NA, smoother = FALSE,
                            scale = 1, theme = "bw", axis.text.size = 10,
                            title.text.size = 12, title.opt = TRUE,
                            ncol = NULL, nrow = NULL){

  ## Errors and Warnings -------------------------------------------------------

  ## Creation of Plots and Grid ------------------------------------------------

  model_data <- cbind(Residual = resid(model), model$model)

  predictor_plots <- lapply(2:dim(model_data)[2], FUN = create_predictor_plots, data = model_data)

  predictor_grobs <- lapply(predictor_plots, ggplotGrob)

  grid.arrange(grobs = predictor_grobs)

}


create_predictor_plots <- function(column_number, data){

  # Create axis labels
  xlab <- names(data)[column_number]
  ylab <- names(data)[1]

  # Subset the data to contain the response and one predictor
  data_sub <- data.frame(response = data[,1],
                         predictor = data[,column_number])

  # Create the plot
  ggplot(data_sub, aes(x = predictor, y = response)) +
    geom_point() +
    geom_hline(yintercept = 0, color = "blue") +
    theme_bw() +
    labs(x = xlab, y = ylab)

}
