

library(MASS)

residlev_plot <- function(model){

  # Create a data frame with the leverage values and standardized residuals
  model_values <- data.frame(leverage = hatvalues(model),
                             std_res = stdres(model))

  # Create the leverage plot
  ggplot(model_values, aes(x = leverage, y = std_res)) +
    geom_point() +
    geom_abline(slope = 0, intercept = 0) +
    labs(x = "Leverage", y = "Standardized Residuals", title = "Residuals vs Leverage") +
    expand_limits(x = 0) +
    geom_smooth(color = "red", se = FALSE, method = 'loess') +
    theme_bw(base_size = 10)
}
