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
  if(class(model)[1] == "double")
    stop("The updated version of ggResidpanel requires a model to be input to the functions.
         Accepted models currently are lm and glm.")
  if(!(class(model)[1] %in% c("lm", "glm")))
    stop("Accepted models currently are lm and glm.")

  # Create a data frame with the leverage values and standardized residuals
  model_values <- data.frame(leverage = hatvalues(model),
                             std_res = stdres(model))

  cook.levels = c(0.5, 1)
  hii <- (infl <- influence(model, do.coef = FALSE))$hat
  r.hat <- range(hii, na.rm = TRUE)
  p <- model$rank
  usr <- par("usr")
  hh <- seq.int(min(r.hat[1L], r.hat[2L]/100), usr[2L], length.out = 101)
  cl.h1 <- sqrt(cook.levels[1] * p * (1 - hh)/hh)
  cl.h2 <- sqrt(cook.levels[2] * p * (1 - hh)/hh)

  # Create the leverage plot
  ggplot(model_values, aes(x = leverage, y = std_res)) +
    geom_point() +
    labs(x = "Leverage", y = "Standardized Residuals", title = "Residuals vs Leverage") +
    expand_limits(x = 0) +
    geom_smooth(color = "red", se = FALSE, method = 'loess', size = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_line(d = data.frame(x = hh, y = cl.h1), aes(x, y), linetype = "dashed", color = "red") +
    geom_line(d = data.frame(x = hh, y = -cl.h1), aes(x, y), linetype = "dashed", color = "red") +
    geom_line(d = data.frame(x = hh, y = cl.h2), aes(x, y), linetype = "dashed", color = "red") +
    geom_line(d = data.frame(x = hh, y = -cl.h2), aes(x, y), linetype = "dashed", color = "red") +
    scale_x_continuous(limits = c(0, max(model_values$leverage, na.rm = TRUE))) +
    scale_y_continuous(limits = extendrange(range(model_values$std_res, na.rm = TRUE), f = 0.08)) +
    theme_bw() +
    theme(plot.title = element_text(size = 12, face = "bold"),
          axis.title = element_text(size = 10))
}
