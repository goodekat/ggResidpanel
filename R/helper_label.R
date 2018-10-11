# Labels for Plots.

# Creates a label for the plots based on the type of residuals used
helper_label <- function(type = NA, model){
  if(!is.na(type)){
    if (type == "response"){
      return("Residuals")
    } else if (type =="pearson"){
      return("Pearson Residuals")
    } else if (type == "deviance"){
      return("Deviance Residuals")
    } else if (type == "standardized"){
      return("Standardized Residuals")
    } else if (type == "stand.deviance"){
      return("Standardized Deviance Residuals")
    } else if (type == "stand.pearson"){
      return("Standardized Pearson Residuals")
    }
  } else {
    if(class(model)[1] == "lm"){
      return("Residuals")
    } else if (class(model)[1] == "lmerMod"){
      return("Pearson Residuals")
    } else if (class(model)[1] == "lmerModLmerTest"){
      return("Pearson Residuals")
    } else if (class(model)[1] == "glm"){
      return("Deviance Residuals")
    } else if (class(model)[1] == "glmerMod"){
      return("Deviance Residuals")
    } else{
      return("Residuals")
    }
  }
}
