#' Boxplot of Residuals.
#'
#' Creates a boxplot on the residuals from a model.
#'
#' @param model Model fit using either lm, glm, lmer, or glmer.
#' @export
#' @return A boxplot of the residuals from the \code{model}.
#' @examples
#' model <- lm(Volume ~ Girth, data = trees)
#' resid_boxplot(model)
#'

resid_boxplot <- function(model){

  # Create a data frame with the residuals
  model_values <- data.frame(resid = resid(model))

  # Create the boxplot of residuals
  ggplot(model_values, aes(x = " ", y = resid)) +
    geom_boxplot() +
    theme_bw() +
    labs(x = " ", y = "Residuals", title = "Boxplot of Residuals") +
    theme(plot.title = element_text(size = 12, face = "bold"),
          axis.title = element_text(size = 10))

}
