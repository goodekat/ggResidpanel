#' Boxplot of Residuals.
#'
#' Creates a boxplot on the residuals from a model.
#'
#' @param resid Residuals from a model.
#' @export
#' @return A boxplot of \code{resid}.
#' @examples
#' model <- lm(Volume ~ Girth, data = trees)
#' resid_boxplot(model$residuals)
#'

resid_boxplot <- function(resid){
  r <- data.frame(resid)
  ggplot(r, aes(x = " ", y = resid)) +
    geom_boxplot() +
    theme_bw() +
    labs(x = " ", y = "Residuals", title = "Boxplot of Residuals")
}
