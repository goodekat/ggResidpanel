#' Labels for Plots.
#'
#' Creates a label for the plots based on the type of residuals used.
#'
#' @param type Specifies type of desired residuals. If NA, the default residual type for each  model is used.
#' @param model Model fit using either lm, glm, lmer, or glmer.
#' @export
#' @return The appropriate residual label.

resid_label <- function(type=NA, model){
  if(!(is.na(type))){
    if(!(type %in% c("standard", "Standard","Pearson", "pearson", "pear", "Pear","Deviance", "deviance", "dev", "Dev")))
      stop("The requested residual type is not available. Choose from the following options: standard, pearson, deviance.")
  }

  if (type%in%c("standard", "Standard")){
    return("Residuals")
  }else if (type %in% c("Pearson", "pearson", "pear", "Pear")){
    return("Pearson Residuals")
  }else if (type %in% c("Deviance", "deviance", "dev", "Dev")){
    return("Deviance Residuals")
  }else if (is.na(type)){
    if(class(model)[1]=="lm"){
      return("Residuals")
    }else if (class(model)[1]=="lmerMod"){
      return("Pearson Residuals")
    }else if (class(model)[1]=="gls"){
      #GLS defaults to raw residuals
      return("Residuals")
    }else if (class(model)[1]=="glm"){
      #GLS defaults to raw residuals
      return("Deviance Residuals")
    }else if (class(model)[1]=="glmerMod"){
      #GLS defaults to raw residuals
      return("Deviance Residuals")
    }
  }
}
