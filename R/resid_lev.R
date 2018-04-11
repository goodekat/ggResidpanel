#' Residual-Leverage plot.
#'
#' Creates a plot of the residuals versus leverage from a model.
#'
#' @param model Model fit using lm.
#' @return A plot of residuals versus leverage values from the \code{model}.
#' @examples
#' model <- lm(Volume ~ Girth, data = trees)
#' residlev_plot(model)

resid_lev <- function(model){

  # Return an error if a model is not entered in the function
  if(typeof(model) == "double")
    stop("The updated version of ggResidpanel requires a model to be input to the functions.
         Accepted models currently are lm and glm.")

  # Create a data frame with the leverage values and standardized residuals
  model_values <- data.frame(leverage = hatvalues(model),
                             std_res = stdres(model))

  # Create the leverage plot
  ggplot(model_values, aes(x = leverage, y = std_res)) +
    geom_point() +
    labs(x = "Leverage", y = "Standardized Residuals", title = "Residuals vs Leverage") +
    expand_limits(x = 0) +
    geom_smooth(color = "red", se = FALSE, method = 'loess', size = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    theme_classic() +
    theme(plot.title = element_text(size = 12, face = "bold"),
          axis.title = element_text(size = 10))
}
