# Extended Residual Calculations
#
# Calculates residuals beyond what 'resid' can do
#
# @param model Specifies type of desired residuals. If NA, the default residual type for each  model is used.
# @param model Model fit using either lm, glm, lmer, or glmer.
# @return The appropriate residual label.

resid_resid <- function(type=NA, model){


  if(class(model)[1]=="lm"){
      ##DEFAULT
    if(is.na(type)|type=="response"){
      return(resid(model, type="response"))
    }else if(type=="pearson"){
      return(resid(model, type="response")/summary(model)$sigma)
    }else if(type=="standardized"){
      return(stdres(model))
    }
  }else if (class(model)[1]=="glm"){
    ###DEFAULT
    if(is.na(type)|type=="deviance"){
      return(resid(model, type="deviance"))
    }else if (type=="response"){
      return(resid(model, type="response"))
    }else if (type=="pearson"){
      return(resid(model, type="pearson"))
    }else if (type=="stand.deviance"){
      return((resid(model, type="deviance"))/(sqrt(summary(model)$dispersion*(1-hatvalues(model)))))
    }else if (type=="stand.pearson"){
      return((resid(model, type="pearson"))/(sqrt(summary(model)$dispersion*(1-hatvalues(model)))))
    }
  }else if (class(model)[1]=="lmerMod"){
    ###Default is pearson
    if(is.na(type)|type=="pearson"){
      #condtional on blups
      return(resid(model, type="response")/summary(model)$sigma)
    }else if (type=="response"){
      return(resid(model, type="response"))
    }
  }else if (class(model)[1]=="glmerMod"){
    ##DEFAULT
    if(is.na(type)|type=="deviance"){
      return(resid(model, type="deviance"))
    }else if (type=="response"){
      return(resid(model, type="response"))
    }else if (type=="pearson"){
        return(resid(model, type="pearson"))
      }
   }#else if(class(model)[1]=="randomForest.formula"){
  #   return(model$y-model$predicted)
  # }else if(class(model)[1]=="function"){
  #   return(model$residuals)
  # }else if(class(model)[1]=="nn"){
  #   return(model$response-model$net.result[[1]])
  # }

}

