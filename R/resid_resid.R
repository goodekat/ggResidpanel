# Extended Residual Calculations.

# Calculates residuals beyond what 'resid' can do
resid_resid <- function(type=NA, model){

  # lm residuals
  if(class(model)[1] == "lm"){

    # Default: raw residuals
    if(is.na(type) | type == "response"){
      return(resid(model, type = "response"))
    }else if(type == "pearson"){
      return(resid(model, type = "response") / summary(model)$sigma)
    }else if(type == "standardized"){
      return(stdres(model))
    }

  # glm residuals
  } else if (class(model)[1] == "glm"){

    # Default: deviance residuals
    if(is.na(type) | type == "deviance"){
      return(resid(model, type = "deviance"))
    }else if (type == "response"){
      return(resid(model, type = "response"))
    }else if (type == "pearson"){
      return(resid(model, type = "pearson"))
    }else if (type == "stand.deviance"){
      return((resid(model, type = "deviance")) / (sqrt(summary(model)$dispersion*(1 - hatvalues(model)))))
    }else if (type == "stand.pearson"){
      return((resid(model, type = "pearson")) / (sqrt(summary(model)$dispersion*(1 - hatvalues(model)))))
    }

  # lmer residuals
  } else if (class(model)[1] == "lmerMod"){

    # Default: Pearson residuals (condtional on BLUPs)
    if(is.na(type) | type == "pearson"){
        return(resid(model, type = "response") / summary(model)$sigma)
    }else if (type == "response"){
      return(resid(model, type = "response"))
    }

  # glmer residuals
  } else if (class(model)[1] == "glmerMod"){

    # Default: deviance residuals
    if(is.na(type) | type == "deviance"){
      return(resid(model, type = "deviance"))
    }else if (type == "response"){
      return(resid(model, type = "response"))
    }else if (type == "pearson"){
        return(resid(model, type = "pearson"))
      }
   }

}

