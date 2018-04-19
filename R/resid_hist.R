<<<<<<< HEAD
#' Histogram of Residuals.
#'
#' Creates a histogram of the residuals from a model.
#'
#' @param model Model fit using either lm, glm, lmer, or glmer.
#' @return A histogram of the residuals from the \code{model} with a normal
#' @param type The user may specify a type of residuals to use, otherwise the default residual type for each model is used.
#' density curve overlaid with mean equal to the mean of the residuals and
#' standard deviation equal to the standard deviation of the residuals.
#' @examples
#' model <- lm(Volume ~ Girth, data = trees)
#' resid_hist(model)


resid_hist <- function(model, bins = NA, theme=NA, axis.text.size=12, title.text.size=12, title=TRUE){

  #If bins=NA, use default
  if(is.na(bins)){
    bins <- 30
  }
  # Return an error if a model is not entered in the function
  if(typeof(model) == "double")
    stop("The updated version of ggResidpanel requires a model to be input to the functions.
         Accepted models currently are lm, glm, lmer, and glmer.")

  #call function to return appropriate residual label
  r_label <- resid_label(type, model)
  # Create a data frame with the residuals
  if(is.na(type)){
    model_values <- data.frame(resid = resid(model))
  }else{
    model_values <- data.frame(resid = resid(model), type=type)
  }

  Default_Title <- paste("Boxplot of", r_label)
  #Step to make sure are not cutting out any huge outliers
  if (min(model_values$resid) < -4*sd(model_values$resid)){
    min_x <- NA
  }else{
    min_x <- -4*sd(model_values$resid)
  }

  if (max(model_values$resid) > 4*sd(model_values$resid)){
    max_x <- NA
  }else{
    max_x <- 4*sd(model_values$resid)
  }

  #do not want xlim if data outside 4*sd
  if (is.na(min_x)&is.na(max_x)){
    # Create the histogram of residuals
    plot <- ggplot(model_values, aes(x = resid)) +
      geom_histogram(aes(y = ..density.., fill = ..count..),
                     color = "black", fill = "grey82", bins = bins) +
      stat_function(fun = dnorm, color = "blue",
                    args = list(mean = 0,
                                sd = sd(model_values$resid))) +
      labs(x = r_label, y = "Density") +
      theme(plot.title = element_text(size = 12, face = "bold"),
            axis.title = element_text(size = 10))
  }else{
  # Create the histogram of residuals
  plot <- ggplot(model_values, aes(x = resid)) +
    geom_histogram(aes(y = ..density.., fill = ..count..),
                   color = "black", fill = "grey82", bins = bins) +
    stat_function(fun = dnorm, color = "blue",
                  args = list(mean = 0,
                              sd = sd(model_values$resid))) +
    labs(x = r_label, y = "Density") +
      xlim(c(min_x, max_x))+
    theme(plot.title = element_text(size = 12, face = "bold"),
          axis.title = element_text(size = 10))+
    labs(x = r_label, y = "Density")
  }


  # Add theme to plot
  if(!is.na(theme)){
  if (theme == "bw"){
    plot <- plot + theme_bw()
  } else if (theme == "classic"){
    plot <- plot + theme_classic()
  } else if (theme == "gray" | theme == "grey"){
    plot <- plot + theme_grey()
  }
}
  # Set text size of title and axis lables, determine whether to include a title, and return plot
  if(title == TRUE){
    plot +
      labs(title =  Default_Title) +
      theme(plot.title = element_text(size = title.text.size, face = "bold"),
            axis.title = element_text(size = axis.text.size))
  } else if (title == FALSE){
    plot + theme(axis.title = element_text(size = axis.text.size))
  }

}
