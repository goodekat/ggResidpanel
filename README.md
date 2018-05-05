
ggResidpanel <img align="right" width="110" height="125" src="./images/gg_resid_sticker4.png">
==============================================================================================

ggResidpanel is an R package for creating panels of diagnostic plots for residuals from a model using ggplot2 and interactive versions of the plots using plotly.

Overview
--------

The package provides three functions that allow the user to assess diagnostic plots of residuals from a model.

**`resid_panel`** This function creates a panel of residual diagnostic plots given a model of type "lm", "glm", "lmerMod", and "glmerMod". It allows the user to select a panel of plots from the options in the package or create their own panel by selecting from the plots available for this function.

**`resid_auxpanel`**: This function creates a panel of residual diagnostic plots given inputs of residuals and fitted values to use for models not accepted by `resid_panel`. Users can select the "SAS" panel option or create their own panel from the plots available for this function.

**`resid_interact`**: This function creates interactive versions of residual diagnostic plots given a model. It accepts models of type "lm", "glm", "lmerMod", and "glmerMod". It allows the user to select one plot to make interactive from the plots available for this function.

Below are the descriptions of the plots currently available.

-   `"boxplot"`: A boxplot of residuals
-   `"cookd"`: A plot of Cook's D values versus observation numbers
-   `"hist"`: A histogram of residuals (optional number of bins)
-   `"ls"`: A location-scale plot of residuals
-   `"qq"`: A normal quantile plot of residuals (optional confidence bands)
-   `"lev"`: A plot of leverage values versus residuals
-   `"resid"`: A plot of residuals versus predicted values (optional smoother)
-   `"yvp"`: A plot of observed response values versus predicted values

Note that not all plots are available for all model types.

The package allows for the formatting options of `scale`, `theme`, `axis.text.size`, `title.text.size`, `title.opt`.

See the documentation for more details.

Installation
------------

Follow these instructions to install ggResidualpanel.

Install the package devtools (if not already installed).

``` r
install.packages("tidyverse")
```

Load the devtools library.

``` r
library(devtools)
```

Install ggResidpanel from the GitHub repository.

``` r
devtools::install_github("goodekat/ggResidpanel")
```

Examples
--------

``` r
# Load the library
library(ggResidpanel)

# Fit a linear model
lm_model <- lm(Volume ~ Girth, data = trees)

# Create the default panel of plots
resid_panel(lm_model, bins = 20)
```

![](README_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-1.png)

``` r
# Create the R panel of plots and change the theme to classic
resid_panel(lm_model, bins = 20, plots = "R", theme = "classic")
```

![](README_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-2.png)

``` r
# Create a panel with all plots available
resid_panel(lm_model, plots = "all", bins = 20)
```

![](README_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-3.png)

``` r
# Fit a glme model using a Poisson family
glm_model <- glm(count ~ spray, family = "poisson", data = InsectSprays)

# Specify three plots to use, request no title, change the theme to gray, and 
# indicate three columns
resid_panel(glm_model, plots = c("resid", "qq", "hist"), bins = 20, 
            title.opt = FALSE, theme = "gray", ind.ncol = 3)
```

![](README_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-1.png)

``` r
# Create an interactive plot
resid_interact(lm_model, plot = "resid")
```

<!--html_preserve-->

<script type="application/json" data-for="26cf199aac73">{"x":{"data":[{"x":[5.10314918502479,6.62290611187783,7.6360773964466,16.248033315281,17.2612045998497,17.7677902421341,18.7809615267029,18.7809615267029,19.2875471689873,19.7941328112716,20.300718453556,20.8073040958404,20.8073040958404,22.3270610226935,23.8468179495466,28.406088730106,28.406088730106,30.4324312992436,32.4587738683811,32.9653595106654,33.9785307952342,34.9917020798029,36.5114590066561,44.1102436409217,45.6300005677749,50.6958569906186,51.7090282751874,53.7353708443249,54.2419564866093,54.2419564866093,67.4131831860031],"y":[5.19685081497521,3.67709388812218,2.5639226035534,0.151966684719002,1.53879540015025,1.93220975786587,-3.18096152670288,-0.580961526702881,3.31245283101275,0.105867188728367,3.89928154644399,0.19269590415961,0.592695904159609,-1.02706102269351,-4.74681794954665,-6.20608873010604,5.39391126989396,-3.03243129924355,-6.75877386838105,-8.06535951066543,0.521469204765816,-3.29170207980293,-0.21145900665607,-5.81024364092172,-3.03000056777485,4.70414300938138,3.99097172481263,4.56462915567513,-2.74195648660925,-3.24195648660925,9.58681681399695],"text":["Prediction:  5.103149<br />Residual:  5.1968508<br />Data: <br /> Volume: 10.3 <br /> Girth: 8.3 <br /> Obs: 1","Prediction:  6.622906<br />Residual:  3.6770939<br />Data: <br /> Volume: 10.3 <br /> Girth: 8.6 <br /> Obs: 2","Prediction:  7.636077<br />Residual:  2.5639226<br />Data: <br /> Volume: 10.2 <br /> Girth: 8.8 <br /> Obs: 3","Prediction: 16.248033<br />Residual:  0.1519667<br />Data: <br /> Volume: 16.4 <br /> Girth: 10.5 <br /> Obs: 4","Prediction: 17.261205<br />Residual:  1.5387954<br />Data: <br /> Volume: 18.8 <br /> Girth: 10.7 <br /> Obs: 5","Prediction: 17.767790<br />Residual:  1.9322098<br />Data: <br /> Volume: 19.7 <br /> Girth: 10.8 <br /> Obs: 6","Prediction: 18.780962<br />Residual: -3.1809615<br />Data: <br /> Volume: 15.6 <br /> Girth: 11 <br /> Obs: 7","Prediction: 18.780962<br />Residual: -0.5809615<br />Data: <br /> Volume: 18.2 <br /> Girth: 11 <br /> Obs: 8","Prediction: 19.287547<br />Residual:  3.3124528<br />Data: <br /> Volume: 22.6 <br /> Girth: 11.1 <br /> Obs: 9","Prediction: 19.794133<br />Residual:  0.1058672<br />Data: <br /> Volume: 19.9 <br /> Girth: 11.2 <br /> Obs: 10","Prediction: 20.300718<br />Residual:  3.8992815<br />Data: <br /> Volume: 24.2 <br /> Girth: 11.3 <br /> Obs: 11","Prediction: 20.807304<br />Residual:  0.1926959<br />Data: <br /> Volume: 21 <br /> Girth: 11.4 <br /> Obs: 12","Prediction: 20.807304<br />Residual:  0.5926959<br />Data: <br /> Volume: 21.4 <br /> Girth: 11.4 <br /> Obs: 13","Prediction: 22.327061<br />Residual: -1.0270610<br />Data: <br /> Volume: 21.3 <br /> Girth: 11.7 <br /> Obs: 14","Prediction: 23.846818<br />Residual: -4.7468179<br />Data: <br /> Volume: 19.1 <br /> Girth: 12 <br /> Obs: 15","Prediction: 28.406089<br />Residual: -6.2060887<br />Data: <br /> Volume: 22.2 <br /> Girth: 12.9 <br /> Obs: 16","Prediction: 28.406089<br />Residual:  5.3939113<br />Data: <br /> Volume: 33.8 <br /> Girth: 12.9 <br /> Obs: 17","Prediction: 30.432431<br />Residual: -3.0324313<br />Data: <br /> Volume: 27.4 <br /> Girth: 13.3 <br /> Obs: 18","Prediction: 32.458774<br />Residual: -6.7587739<br />Data: <br /> Volume: 25.7 <br /> Girth: 13.7 <br /> Obs: 19","Prediction: 32.965360<br />Residual: -8.0653595<br />Data: <br /> Volume: 24.9 <br /> Girth: 13.8 <br /> Obs: 20","Prediction: 33.978531<br />Residual:  0.5214692<br />Data: <br /> Volume: 34.5 <br /> Girth: 14 <br /> Obs: 21","Prediction: 34.991702<br />Residual: -3.2917021<br />Data: <br /> Volume: 31.7 <br /> Girth: 14.2 <br /> Obs: 22","Prediction: 36.511459<br />Residual: -0.2114590<br />Data: <br /> Volume: 36.3 <br /> Girth: 14.5 <br /> Obs: 23","Prediction: 44.110244<br />Residual: -5.8102436<br />Data: <br /> Volume: 38.3 <br /> Girth: 16 <br /> Obs: 24","Prediction: 45.630001<br />Residual: -3.0300006<br />Data: <br /> Volume: 42.6 <br /> Girth: 16.3 <br /> Obs: 25","Prediction: 50.695857<br />Residual:  4.7041430<br />Data: <br /> Volume: 55.4 <br /> Girth: 17.3 <br /> Obs: 26","Prediction: 51.709028<br />Residual:  3.9909717<br />Data: <br /> Volume: 55.7 <br /> Girth: 17.5 <br /> Obs: 27","Prediction: 53.735371<br />Residual:  4.5646292<br />Data: <br /> Volume: 58.3 <br /> Girth: 17.9 <br /> Obs: 28","Prediction: 54.241956<br />Residual: -2.7419565<br />Data: <br /> Volume: 51.5 <br /> Girth: 18 <br /> Obs: 29","Prediction: 54.241956<br />Residual: -3.2419565<br />Data: <br /> Volume: 51 <br /> Girth: 18 <br /> Obs: 30","Prediction: 67.413183<br />Residual:  9.5868168<br />Data: <br /> Volume: 77 <br /> Girth: 20.6 <br /> Obs: 31"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,0,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,0,0,1)"}},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1.98764748497588,70.528684886052],"y":[0,0],"text":"intercept: 0<br />slope: 0","type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(0,0,255,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":42.1685346616853,"r":7.30593607305936,"b":38.854296388543,"l":35.9319219593192},"plot_bgcolor":"rgba(255,255,255,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":"<b> Residual Plot <\/b>","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":15.9402241594022},"xaxis":{"domain":[0,1],"type":"linear","autorange":false,"tickmode":"array","range":[1.98764748497588,70.528684886052],"ticktext":["20","40","60"],"tickvals":[20,40,60],"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":"Predicted Values","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":13.2835201328352},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"type":"linear","autorange":false,"tickmode":"array","range":[-8.94796832689855,10.4694256302301],"ticktext":["-5","0","5","10"],"tickvals":[-5,0,5,10],"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":"Residuals","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":13.2835201328352},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":"transparent","line":{"color":"rgba(51,51,51,1)","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest"},"source":"A","attrs":{"26cf36476775":{"x":{},"y":{},"label":{},"type":"ggplotly"},"26cf9d94e2a":{"intercept":{},"slope":{}}},"cur_data":"26cf36476775","visdat":{"26cf36476775":["function (y) ","x"],"26cf9d94e2a":["function (y) ","x"]},"config":{"modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"cloud":false},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1}},"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":{"render":[{"code":"function(el, x) { var ctConfig = crosstalk.var('plotlyCrosstalkOpts').set({\"on\":\"plotly_click\",\"persistent\":false,\"dynamic\":false,\"selectize\":false,\"opacityDim\":0.2,\"selected\":{\"opacity\":1}}); }","data":null}]}}</script>
<!--/html_preserve-->
