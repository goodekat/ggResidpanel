# Labels for Plots.
#
# Creates a label for the plots based on the type of residuals used.
#
# @param type Specifies type of desired residuals. If NA, the default residual type for each  model is used.
# @param model Model fit using either lm, glm, lmer, or glmer.
# @return The appropriate residual label.

resid_label <- function(type=NA, model){


  if(!is.na(type)){
  if (type=="response"){
    return("Residuals")
  }else if (type =="pearson"){
    return("Pearson Residuals")
  }else if (type == "deviance"){
    return("Deviance Residuals")
  }else if (type=="standardized"){
    return("Standardized Residuals")
  }else if (type=="stand.deviance"){
    return("Standardized Deviance Residuals")
  }else if (type=="stand.pearson"){
    return("Standardized Pearson Residuals")
  }}else {
    if(class(model)[1]=="lm"){
      return("Residuals")
    }else if (class(model)[1]=="lmerMod"){
      return("Pearson Residuals")
    }else if (class(model)[1]=="glm"){
      #GLS defaults to raw residuals
      return("Deviance Residuals")
    }else if (class(model)[1]=="glmerMod"){
      #GLS defaults to raw residuals
      return("Deviance Residuals")
    }
  }


}
