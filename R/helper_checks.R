# Check functions to use in plot functions that return errors or warnings to users.

## CHECKS THAT RETURN ERRORS ---------------------------------------------------------------

# Return an error if an acceptable model type is not entered in the function
check_modeltype <- function(model){

  if(!(class(model)[1] %in% c("lm", "glm", "lme", "lmerMod", "lmerModLmerTest", "glmerMod")))
    stop("This function requires a model to be input. Accepted models
         currently are lm, glm, lme, lmer, lmerTest, and glmer.")

}

# Return an error if the requested residual type is not available for the model type
check_residualtype <- function(model, type){

  type <- tolower(type)

  if(!is.na(type)){
    if(class(model)[1] == "lm"){
      if(!(type %in% c("response", "pearson", "standardized"))){
        stop("The requested residual type is not available for an 'lm' model. Please select
             from the following options for an 'lm' model: response, pearson, or standardized.")
      }
      } else if(class(model)[1] == "glm"){
        if(!(type %in% c("response", "pearson", "deviance", "stand.pearson", "stand.deviance"))){
          stop("The requested residual type is not available for a 'glm' model. Please select
               from the following options for a 'glm' model: response, pearson, deviance,
               stand.deviance, or stand.pearson.")
        }
        } else if(class(model)[1] == "lmerMod"){
          if(!(type %in% c("response", "pearson"))){
            stop("The requested residual type is not available for an 'lmer' model. Please
                 select from the following options for an 'lmer' model: response or pearson.")
          }
          } else if(class(model)[1] == "lmerModLmerTest"){
            if(!(type %in% c("response", "pearson"))){
              stop("The requested residual type is not available for an 'lmerTest' model. Please
                   select from the following options for an 'lmerTest' model: response or pearson.")
            }
            } else if(class(model)[1] == "lme"){
              if(!(type %in% c("response", "pearson"))){
                stop("The requested residual type is not available for an 'lme' model. Please
                   select from the following options for an 'lme' model: response or pearson.")
              }
            } else if(class(model)[1] == "glmerMod"){
              if(!(type %in% c("response", "pearson", "deviance"))){
                stop("The requested residual type is not available for a 'glmer' model. Please
                     select from the following options for a 'glmer' model: response, pearson,
                     or deviance.")
              }
              }
  }

}

# Return an error if the requested plots involve standardizing residuals for an 'lmer' or
# a 'glmer' model
check_standardized <- function(model, plots){

  if(class(model)[1] %in% c("lme", "lmerMod", "lmerModLmerTest", "glmerMod")){
    if("ls" %in% plots |"lev" %in% plots | "R" %in% plots){
      stop("The requested plot or panel uses standardized residuals, which are not
           currently available for 'lme', 'lmer', 'lmerTest', or 'glmer' models.")
    }
  }

}

# Return an error if Cook's D plot is requested for an 'lmer' or 'glmer' model
check_cooksd <- function(model, plots){

  if(class(model)[1] %in% c("lme", "lmerMod", "lmerModLmerTest", "glmerMod")){
    if("cookd" %in% plots){
      stop("The Cook's D plot is unavailable for 'lme', 'lmer', 'lmerTest', and 'glmer' models.")
    }
  }

}


## CHECKS THAT RETURN WARNINGS -------------------------------------------------------------

# Return a warning if the smoother option is not specified correctly and return
# the default option if not specified
check_smoother <- function(smoother){

  if(smoother == TRUE | smoother == FALSE){
  } else{
    smoother <- FALSE
    warning("The smoother option for residual plot not was specified correctly.
            The default option will be used. Accepted options are TRUE or FALSE.")
  }

  return(smoother)

}

# Return a warning if the theme is not specified correctly and return the default
# option if not specified
check_theme <- function(theme){

  if(theme == "bw" | theme == "classic" | theme == "grey" | theme == "gray"){
  } else{
    theme <- "bw"
    warning("The theme option was not specified correctly. The default theme
            will be used. Accepted themes are 'bw', 'classic', and 'grey' (or 'gray').")
  }

  return(theme)
}

# Return a warning if the title option is not specified correctly and return the
# default option if not specified
check_title <- function(title.opt){

  if(title.opt == TRUE | title.opt == FALSE){
  } else{
    title.opt <- TRUE
    warning("The title option was not specified correctly. The default title
            option will be used. Accepted options are TRUE or FALSE.")
  }

  return(title.opt)

}

# Return warning if consant leverage
check_leverage <- function(model, plots){

  if(class(model) %in% c("lm", "glm")){

    if("all" %in% plots | "R" %in% plots | "lev" %in% plots){

      leverage_val <- hatvalues(model)

      zero_range <- function(x, tol = .Machine$double.eps ^ 0.5) {
        if (length(x) == 1) return(TRUE)
        x <- range(x) / mean(x)
        isTRUE(all.equal(x[1], x[2], tolerance = tol))
      }

      if(zero_range(leverage_val)==TRUE){
        warning("Note that this model has constant leverage.")
      }

    }

  }

}

# Return a warning about choosing the number of bins if a histogram is included
# and the number of bins has not been specified and return the default option if
# not specified
# check_bins <- function(plots, bins){
#
#   if("default" %in% plots | "SAS" %in% plots | "all" %in% plots | "hist" %in% plots){
#     if(is.na(bins)){
#       bins = 30
#       warning("By default, bins = 30 in the histogram of residuals. If necessary,
#             specify an appropriate number of bins.")
#     }
#   }
#
#   return(bins)
#
# }
