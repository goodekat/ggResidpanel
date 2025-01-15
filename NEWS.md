# ggResidpanel 0.4.0

## Major 

- Added resid_calibrate
- For lm model:
  - Standardized residuals are now the default instead of raw residuals
  - Fixed issue when leverage is equal to 1 when computing standardized residuals - report Pearson residuals instead
- Added alpha value in all functions
- Adjustments to Cook's D plots
  - "Return a warning if any of the leverage values are equal to 1 and add lines to plot"
  - Need to add more descriptions here
- In plot_constlev, removed "Compute the values for the lowess curve"
- In plot_lev, :
  - "Create the cutoff SAS uses with Cook's D"
  - "Count number over cutoff"
  - Adjusted the way constant leverage is handled
  - Added Cook's D values
  - When creating hat values, "Add more resolution for large n"
- In plot_ls, "If smoother is set to true, do not add it to the plot"
- Adjustments to the smoother options across various plot types

## Minor

- Typo fixes in documentation
- Small fixes to code for better practices
- Small updates to plot_hist grid
- Clean up of some plot labels

# ggResidpanel 0.3.0

## Major:

- changed resid_interact to allow for a panel of interactive plots
- added the functions of resid_xpanel and resid_compare
- added model types of lmerTest and lme
- added an introductory vignette

## Minor: 

- updated documentation
- added Cook's distance values to residual-leverage plot
- reordered plots in "all" panels
- included penguins data
- rounded number in tool tip in resid_interact
- fixed some bugs in the code

# ggResidpanel 0.2.0 and earlier

- first versions of ggResidpanel (the progression was not documented)
- introduced the functions of resid_panel, resid_interact, and resid_auxpanel
- allowed options in the functions for formatting options
