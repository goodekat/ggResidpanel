# Extended Residual Calculations
#
# Calculates residuals beyond what 'resid' can do
#
# @param model Specifies type of desired residuals. If NA, the default residual type for each  model is used.
# @param model Model fit using either lm, glm, lmer, or glmer.
# @return The appropriate residual label.

resid_label <- function(type=NA, model){


  if(class(model)[1]=="lm"){
    if(is.na(type)|type=="response"){
      return(resid(model, type="response"))
    }else if(type=="pearson"){
      return(resid(model, type="response")/summary(model)$sigma)
    }else(iftype=="standardized"){
      return(stdres(model))
    }
  }


  if (class(model)[1]=="glm"){
    if(is.na(type)|type=="deviance"){
      return(resid(model, type="deviance"))
    }else if (type=="response"){
      return(resid(model, type="response"))
    }else if (type="pearson"){
      return(resid(model, type="pearson"))
    }else if (type=="stand.deviance"){
      return((resid(model, type="deviance"))/(sqrt(summary(model)$dispersion*(1-hatvalues(model)))))
    }else if (type=="stand.pearson"){
      return((resid(model, type="pearson"))/(sqrt(summary(model)$dispersion*(1-hatvalues(model)))))
    }
  }

  if (class(model)[1]=="lmerMod"){
    if(is.na(type)|type="pearson"){
      #condtional on blups
      return(resid(model, type="response")/summary(model)$sigma)
    }else if (type=="response"){
      return(resid(model, type="response"))
    }else if (type="standardized"){

    }
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
