% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resid_calibrate.R
\name{resid_calibrate}
\alias{resid_calibrate}
\title{Panel of Diagnostic Residual Plots from a fitted model and simulated responses from the same model.}
\usage{
resid_calibrate(
  model,
  plots = "default",
  nsim = 1,
  identify = TRUE,
  shuffle = FALSE,
  type = NA,
  bins = 30,
  smoother = TRUE,
  qqline = TRUE,
  qqbands = FALSE,
  scale = 1,
  theme = "bw",
  axis.text.size = 10,
  title.text.size = 12,
  title.opt = TRUE,
  nrow = NULL,
  alpha = 0.6
)
}
\arguments{
\item{model}{Model fit using either \code{lm}, \code{glm}, \code{lmer},
\code{lmerTest}, or \code{glmer}.}

\item{plots}{Plots chosen to include in the panel of plots. The default panel
includes a residual plot, a normal quantile plot, an index plot,
and a histogram of the residuals. (See details for the options available.)}

\item{nsim}{Number of simulated models, defaults to 1}

\item{identify}{TRUE to label true residuals, FALSE to hide}

\item{shuffle}{TRUE to shuffle the order of the residuals, FALSE to have real 
data last}

\item{type}{Type of residuals to use in the plot. If not specified, the
default residual type for each model type is used. (See details for the
options available.)}

\item{bins}{Number of bins to use when creating a histogram of the residuals.
Default is set to 30.}

\item{smoother}{Indicates whether or not to include a smoother on the residual 
vs fitted and index plots. Specify TRUE or FALSE. Default is set to TRUE.}

\item{qqline}{Indicates whether to include a 1-1 line on the qq-plot. Specify
TRUE or FALSE. Default is set to TRUE.}

\item{qqbands}{Indicates whether to include confidence bands on the qq-plot.
Specify TRUE or FALSE. Default is set to FALSE.}

\item{scale}{Scales the size of the graphs in the panel. Takes values in (0,1].}

\item{theme}{ggplot2 theme to be used. Current options are \code{"bw"},
\code{"classic"}, and \code{"grey"} (or \code{"gray"}). Default is
\code{"bw"}.}

\item{axis.text.size}{Specifies the size of the text for the axis labels of
all plots in the panel.}

\item{title.text.size}{Specifies the size of the text for the titles of all
plots in the panel.}

\item{title.opt}{Indicates whether or not to include a title on the plots in
the panel. Specify TRUE or FALSE. Default is set to TRUE.}

\item{nrow}{Sets the number of rows in the panel.}

\item{alpha}{Sets the alpha level for displays with points. Default is set to 0.6.}
}
\value{
A panel of residual diagnostic plots containing plots specified for each model.
}
\description{
Used to calibrate expectations for simulation variability when assumptions are true (see \code{simulate}),
to compare to the actual observed residuals in any of a suite of diagnostic plots. 
This function is based on the \code{resid_compare} function and requires the fitted model and the data set.
}
\details{
Creates a panel of residual diagnostic plots the actual model and simulated responses from that model. 
Currently accepts
models of type "lm" (future versions will accept "glm", "lmerMod", "lmerModLmerTest", and "glmerMod").



The first two sections below contain information on the available input
options for the \code{plots} and \code{type} arguments in \code{resid_compare}.
The third section contains details relating to the creation of the plots.

\strong{Options for Plots}

The following options can be chosen for the \code{plots} argument.
\itemize{
\item "all": This creates a panel of all plot types included in the package
that are available for the model type input into \code{residpanel}. (See note
below.)
\item "default": This creates a panel with a residual plot, a normal quantile plot
of the residuals, an index plot of the residuals, and a histogram of the residuals.
\item "R": This creates a panel with a residual plot, a normal
quantile plot of the residuals, a location-scale plot, and a leverage versus
residuals plot. This was modeled after the plots shown in R if the
\code{plot()} base function is applied to an \code{lm} model. This option can
only be used with an \code{lm} or \code{glm} model.
\item "SAS": This creates a panel with a residual plot, a normal quantile plot of
the residuals, a histogram of the residuals, and a boxplot of the residuals.
This was modeled after the residpanel option in proc mixed from SAS version 9.4.
\item A vector of individual plots can also be specified.
For example, one can specify \code{plots = c("boxplot", "hist")} or
\code{plots = "qq"}. The individual plot options are as follows.
\itemize{
\item \code{"boxplot"}: A boxplot of residuals
\item \code{"cookd"}: A plot of Cook's D values versus observation numbers
\item \code{"hist"}: A histogram of residuals
\item \code{"index"}: A plot of residuals versus observation numbers
\item \code{"ls"}: A location scale plot of the residuals
\item \code{"qq"}: A normal quantile plot of residuals
\item \code{"lev"}: A plot of leverage values versus residuals
\item \code{"resid"}: A plot of residuals versus predicted values
\item \code{"yvp":}: A plot of observed response values versus predicted values
} }

Note: \code{"cookd"}, \code{"ls"}, and \code{"lev"} are only available for "lm"
and "glm" models.

\strong{Options for Type}

Several residual types are available to be requested based on the model type
that is input into \code{resid_panel}. These currently are as follows.
\itemize{
\item \code{lm} residual options
\itemize{
\item \code{"pearson"}:The Pearson residuals
\item \code{"response"}: The raw residuals (Default for "lm")
\item \code{"standardized"}: The standardized raw residuals
}
\item \code{glm} residual options
\itemize{
\item \code{"pearson"}: The Pearson residuals
\item \code{"deviance"}: The deviance residuals (Default for "glm")
\item \code{"response"}: The raw residuals
\item \code{"stand.deviance"}: The standardized deviance residuals
\item \code{"stand.pearson"}: The standardized Pearson residuals
}
\item \code{lmer}, \code{lmerTest}, and \code{lme} residual options
\itemize{
\item \code{"pearson"}: The Pearson residuals (Default for "lmer", "lmerTest", and "lme")
\item \code{"response"}: The raw residuals
}
\item \code{glmer} residual options
\itemize{
\item \code{"pearson"}: The Pearson residuals
\item \code{"deviance"}: The deviance residuals (Default for "glmer")
\item \code{"response"}: The raw residuals
} }

Note: The plots of \code{"ls"} and \code{"lev"} only accept standardized residuals.

\strong{Details on the Creation of Plots}

\describe{
\item{Boxplot (\code{boxplot})}{Boxplot of the residuals.}

\item{Cook's D (\code{cookd})}{ The horizontal line represents a cut-off to identify
highly influential points. The horizontal line is placed at 4/n where n is
the number of data points used in the \code{model}.}

\item{Histogram (\code{hist})}{Plots a histogram of the residuals. The density
curve overlaid has mean equal to zero and standard deviation equal to the
standard deviation of the residuals.}

\item{Index Plot (\code{index})}{Plots the residuals on the y-axis and the observation
number associated with the residual on the x-axis.}

\item{Leverage Plot (\code{lev})}{Plots the standardized residuals on the y-axis
and the leverage values on the x-axis. A lowess curve is overlaid, and Cook's
D contours are included for \eqn{\alpha = 0.5} and \eqn{\alpha = 1}.}

\item{Location-Scale Plot (\code{ls})}{Plots the square root of the absolute value
of the standardized residuals on the y-axis and the predicted values on the
x-axis. The predicted values are plotted on the original scale for \code{glm}
and \code{glmer} models. A lowess curve is overlaid.}

\item{QQ Plot (\code{qq})}{Makes use of the \code{R} package \code{qqplotr} for
creating a normal quantile plot of the residuals.}

\item{Residual Plot (\code{resid})}{Plots the residuals on the y-axis and the
predicted values on the x-axis. The predicted values are plotted on the
original scale for \code{glm} and \code{glmer} models.}

\item{Response vs. Predicted (\code{yvp})}{Plots the response variable from the
model on the y-axis and the predicted values on the x-axis. Both response
variable and predicted values are plotted on the original scale for
\code{glm} and \code{glmer} models.}
}
}
\examples{

# Fit a model to the penguins data
penguin_model <- lm(heartrate ~ depth + duration, data = penguins)

resid_calibrate(
  model = penguin_model, 
  plots = "qq", 
  nsim = 3, 
  shuffle = TRUE, 
  identify = TRUE
 )
}
