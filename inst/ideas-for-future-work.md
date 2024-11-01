# To Do List

## Current

- Write up list of questions
- Work through Mark's examples
- Check in with Mark
- Work in updates from posit course
- Clean up old to do list
- Ask Katie about changing her email address

## Old List

Katie's Tasks:
- Work on residual vignette (IN PROGRESS)
  - include a link to the residual document in the documentation
- modify so works for predictive models (Research at least)(NOT FOR CRAN)
  -randomForest (from randomForest): contains 'y' and 'predicted' (class is randomForest.formula)(
  - neuralnet (from neuralnet): contains 'response', 'covariates' and 'data' (class is nn)
  - nnet (from nnet): contains 'residuals' and 'fitted.values' (class is nnet)
  - svm (from e1071): contains 'fitted' and 'residuals' (class is svm)
- test file using testthat
  - how to add lmertest to testthat, but still test lmer as well ???
  - Added in section for warnings and errors, tested all formatting options with 2 plots
  - NOTE:USING THE WORD plot instead of plots
  - NOTE: if plotting a panel of four plots, nrow DOES NOT USE THIRD ROW when nrow = 3
- round all values shown in interactive plots
  - Check resid and predicted

- Completed:
  - Rounded data tooltip to 2 decimal places if a decimal is present in any of the numbers and the variable is numeric
      - Added in a check so only rounds if has integers preceding the decimal to avoid roundin 0.0023 to 0
  - add code used to create sticker under data raw (code currently not working)
  - shorten title of constant leverage plot

  - fix resid_interact for binomial case to be more general (Double check names with Katherine)
  - make smoother tooltips consistent (Do we want observation added for smoother?)
  - proof read introduction:
        + menion that is not on CRAN yet, will need to change soon
        + leave Github installatin instructions, but mention to check github for working version?


Katherine's Tasks
- current:

- later:
  - cleaning board game data and coach salary data to use for paper
    (and vignettes?)
  - add in links in vignettes to the residual vignette (when finished)
  - contact Jessica Meir to ask for citation of penguin data
  - figure out how to use the "safely" and "quietly" functions to only show
    desired warnings and suppress the error:
    grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  :
      polygon edge not found


Ideas for Future Package Versions:
- write a function for computing standardized residuals for lmer and glmer (Katie)
- look into testthat and vistest packages for testing
- create an option for the user to add their own theme
- add in minimum package versions?? (under imports)
- add interactive versions of the auxpanel, xpanel, and compare plots
  - maybe add an option in each of the functions for an interactive version
- option to color residuals by predictor variables?
- add a resid versus y plot
- read about mixed model influence diagnostics in SAS for mixed models (Stroup 2018)
- nested residuals/plots for assessing random effects
