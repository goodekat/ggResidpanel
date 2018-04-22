# Extended Residual Calculations
#
# Calculates residuals beyond what 'resid' can do
#
# @param model Specifies type of desired residuals. If NA, the default residual type for each  model is used.
# @param model Model fit using either lm, glm, lmer, or glmer.
# @return The appropriate residual label.

resid_label <- function(type=NA, model){
  if(!(is.na(type))){
    if(!(type %in% c("response", "pearson", "deviance", "standardized", "stand.pearson", "stand.deviance")))
      stop("The requested residual type is not available. Choose from the following options: standard, pearson, deviance.")
  }

  if(!is.na(type)){
    if (type=="response"){
      return("Residuals")
    }else if (type =="pearson"){
      return("Pearson Residuals")
    }else if (type == "deviance"){
      return("Deviance Residuals")
    }}else {
      if(class(model)[1]=="lm"){
        return("Residuals")
      }else if (class(model)[1]=="lmerMod"){
        return("Conditional Residuals")
      }else if (class(model)[1]=="glm"){
        #GLS defaults to raw residuals
        return("Deviance Residuals")
      }else if (class(model)[1]=="glmerMod"){
        #GLS defaults to raw residuals
        return("Deviance Residuals")
      }
    }


}
