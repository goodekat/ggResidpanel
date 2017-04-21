# ggResidpanel
An R package for creating a panel of diagnostic plots for residuals from a model using ggplot2.

```{r}
model <- lm(Volume ~ Girth, data = trees)
resid_panel(model$residuals, model$fitted.values)
```
