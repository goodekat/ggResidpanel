library(ggplot2)
library(png)
library(grid)
library(hexSticker)
library(qqplotr)
library(cowplot)
library(gridExtra)

# the hexSticker package is currently producing an error even with using test example from: https://github.com/GuangchuangYu/hexSticker

# The following code creats the panel to show on the sticker

make_plot <- function(){
resid_spanel <- function(resid, pred, plots = "SAS", bins = NA, scale = 1,
                         smoother = FALSE, theme = "bw",
                         axis.text.size = 10, title.text.size = 12,
                         title = TRUE, qqline = TRUE, qqbands = FALSE){

  ## Errors and Warnings -------------------------------------------------------

  # Return an error if the request residual type is not available for the model type

  # Return an error if the requested plots involve standardizing residuals for an 'lmer' or
  # 'glmer' model

  # Return an error if smoother option is not specified correctly
  if(smoother == TRUE | smoother == FALSE){
  }else{
    stop("Smoother option for residual plot not specified correctly.
         Choose either TRUE or FALSE.")
  }

  # Return an error if theme is not specified correctly
  if(theme == "bw" | theme == "classic" | theme == "grey" | theme == "gray"){
  }else{
    theme = "bw"
    warning("Theme option not specified correctly. Accepted themes are bw, classic,
            and grey (or gray). Default theme will be used.")
  }

  # Return an error if smoother option is not specified correctly
  if(title == TRUE | title == FALSE){
  }else{
    stop("Title option not specified correctly. Choose either TRUE or FALSE.")
  }

  # Return a warning about choosing number of bins if a histogram is included
  if("SAS" %in% plots | "hist" %in% plots){
    if(is.na(bins)){
      bins = 30
      warning("By default, bins = 30 in the histogram of residuals. If necessary, specify
              an appropriate number of bins.")
    }
    }

  ## Creation of plots ---------------------------------------------------------

  # Create a boxplot of the residuals if selected in plots otherwise set as NULL
  if("boxplot" %in% plots | "SAS" %in% plots | "all" %in% plots){
    boxplot <- resid_sboxplot(resid,pred, theme, axis.text.size, title.text.size, title)+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y=element_blank())
  } else{
    boxplot <- NULL
  }

  # Create a histogram of the residuals if selected in plots otherwise set as NULL
  if("hist" %in% plots | "SAS" %in% plots | "all" %in% plots | "SASextend" %in% plots){
    hist <- resid_shist(resid, pred, bins = bins, theme, axis.text.size,
                        title.text.size, title)+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y=element_blank())
  } else{
    hist <- NULL
  }

  # Create a q-q plot of the residuals if selected in plots otherwise set as NULL
  if("qq" %in% plots | "SAS" %in% plots){
    qq <- resid_sqq(resid, pred, theme, axis.text.size, title.text.size, title, qqline, qqbands)+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y=element_blank())
  } else{
    qq <- NULL
  }



  # Create a residual plot if selected in plots otherwise set as NULL
  if("residplot" %in% plots | "SAS" %in% plots){
    residplot <- resid_splot(resid, pred,  smoother, theme, axis.text.size, title.text.size, title)+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y=element_blank())
  } else{
    residplot <- NULL
  }


  ## Creation of grid of plots -------------------------------------------------

  # If individual plots have been specified, set plots equal to "individual"
  # Return an error if none of the correct plot options have been specified
  if(plots=="SAS"){
    plots <- plots
  } else if("boxplot" %in% plots | "cookd" %in% plots | "hist" %in% plots |
            "ls" %in% plots | "qq" %in% plots | "residlev" %in% plots |
            "residplot" %in% plots | "respred" %in% plots){
    plots <- "individual"
  } else{
    stop("Invalid plots option entered")
  }

  # Create a grid of plots based on the plots specified
  if(plots == "SAS"){

    # Create grid of SAS plots in resid panel
    plot_grid(residplot, hist, qq, boxplot, scale = scale)

  }else if (plots == "individual") {

    # Turn the specified plots into a list
    individual_plots <- list(residplot = residplot, hist = hist, qq = qq,
                             boxplot = boxplot)

    # Remove the plots which are null
    individual_plots <- individual_plots[-which(sapply(individual_plots, is.null))]

    # Turn the list of plots into a grob
    my_grobs = lapply(individual_plots, ggplotGrob)

    # Specify number of columns for grid of plots based on number of plots specified
    ifelse(length(individual_plots) == 1, grid_col <- 1, grid_col <- 2)

    # Create grid of individual plots specified
    grid.arrange(grobs = my_grobs, ncol = grid_col, scale = scale)

  }

  }


resid_splot <- function(resid, pred,smoother, theme, axis.text.size, title.text.size, title){



  # Create a data frame with the residuals

  model_values <- data.frame(Residual=resid, Prediction=pred)

  model_values$Lowess.x <- lowess(x=model_values$Prediction, y=model_values$Residual)$x
  model_values$Lowess.y <- lowess(x=model_values$Prediction, y=model_values$Residual)$y

  # Create the residual plot
  plot <- ggplot(data=model_values, aes(x = Prediction, y = Residual)) +
    geom_point(size=4) +
    geom_abline(slope = 0, intercept = 0, color = "blue",size=2) +
    labs(x = "Predicted Values", y = "Residuals")


  # If smoother is set to true, add it to the plot
  if (smoother == TRUE){
    plot <- plot +
      geom_line(aes(Lowess.x, Lowess.y),colour = "red", size = 0.5)

  }

  # Add theme to plot
  if (theme == "bw"){
    plot <- plot + theme_bw()
  } else if (theme == "classic"){
    plot <- plot + theme_classic()
  } else if (theme == "gray" | theme == "grey"){
    plot <- plot + theme_grey()
  }

  # Set text size of title and axis labels, determine whether to include a title, and return plot
  if(title == TRUE){
    plot +
      labs(title = "Residuals Plot") +
      theme(plot.title = element_text(size = title.text.size, face = "bold"),
            axis.title = element_text(size = axis.text.size))
  } else if (title == FALSE){
    plot + theme(axis.title = element_text(size = axis.text.size))
  }

}


resid_sqq <- function(resid, pred, theme, axis.text.size, title.text.size, title, qqline, qqbands){


  r <- data.frame(r=resid)

  if(qqbands==TRUE){
    plot <- ggplot(data = r, mapping = aes(sample = r)) +
      stat_qq_band()+
      stat_qq_point(size=4) +labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
  }else{
    plot <- ggplot(data = r, mapping = aes(sample = r)) +
      stat_qq_point(size=4) +labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
  }



  if(qqline==TRUE){
    plot <- plot+stat_qq_line(color="blue",linewidth=2)
  }




  # Add theme to plot
  if (theme == "bw"){
    plot <- plot + theme_bw()
  } else if (theme == "classic"){
    plot <- plot + theme_classic()
  } else if (theme == "gray" | theme == "grey"){
    plot <- plot + theme_grey()
  }


  # Set text size of title and axis labels, determine whether to include a title, and return plot
  if(title == TRUE){
    plot +
      labs(title = "Q-Q Plot of Residuals") +
      theme(plot.title = element_text(size = title.text.size, face = "bold"),
            axis.title = element_text(size = axis.text.size))
  } else if (title == FALSE){
    plot + theme(axis.title = element_text(size = axis.text.size))
  }

}

resid_sboxplot <- function(resid,pred, theme, axis.text.size, title.text.size, title){


  model_values <- data.frame(Residual=resid)
  model_values$Observation <- 1:nrow(model_values)

  # Create the boxplot of residuals
  plot <- ggplot(model_values, aes(x = " ", y = Residual)) +
    geom_boxplot(size=2,outlier.size = 4,width=.5) +
    geom_point(alpha=0)+
    labs(x = " ", y = "Residuals") +
    theme(plot.title = element_text(size = 12, face = "bold"),
          axis.title = element_text(size = 10))

  # Add theme to plot

  if (theme == "bw"){
    plot <- plot + theme_bw()
  } else if (theme == "classic"){
    plot <- plot + theme_classic()
  } else if (theme == "gray" | theme == "grey"){
    plot <- plot + theme_grey()
  }

  # Set text size of title and axis labels, determine whether to include a title, and return plot
  if (title == TRUE){
    plot +
      labs(title = "Boxplot of Residuals") +
      theme(plot.title = element_text(size = title.text.size, face = "bold"),
            axis.title = element_text(size = axis.text.size))
  } else if (title == FALSE){
    plot + theme(axis.title = element_text(size = axis.text.size))
  }


}
resid_shist <- function(resid, pred, bins, theme, axis.text.size, title.text.size, title){

  #If bins=NA, use default
  if(is.na(bins)){
    bins <- 30
  }

  model_values <- data.frame(resid=resid)
  #Step to make sure are not cutting out any huge outliers
  if (min(model_values$resid) < -4*sd(model_values$resid)){
    min_x <- NA
  }else{
    min_x <- -4*sd(model_values$resid)
  }

  if (max(model_values$resid) > 4*sd(model_values$resid)){
    max_x <- NA
  }else{
    max_x <- 4*sd(model_values$resid)
  }

  #do not want xlim if data outside 4*sd
  if (is.na(min_x)&is.na(max_x)){
    # Create the histogram of residuals
    plot <- ggplot(model_values, aes(x = resid)) +
      geom_histogram(aes(y = ..density.., fill = ..count..),
                     color = "black", fill = "grey82", bins = 12,size=1.5) +
      stat_function(fun = dnorm, color = "blue",size=2,
                    args = list(mean = 0,
                                sd = sd(model_values$resid))) +
      labs(x = "Residuals", y = "Density") +
      theme(plot.title = element_text(size = 12, face = "bold"),
            axis.title = element_text(size = 10))
  }else{
    # Create the histogram of residuals
    plot <- ggplot(model_values, aes(x = resid)) +
      geom_histogram(aes(y = ..density.., fill = ..count..),
                     color = "black", fill = "grey82", bins = 12,size=1.5) +
      stat_function(fun = dnorm, color = "blue",size=2,
                    args = list(mean = 0,
                                sd = sd(model_values$resid))) +
      labs(x = "Residuals", y = "Density") +
      xlim(c(min_x, max_x))+
      theme(plot.title = element_text(size = 12, face = "bold"),
            axis.title = element_text(size = 10))
  }


  # Add theme to plot
  if (theme == "bw"){
    plot <- plot + theme_bw()
  } else if (theme == "classic"){
    plot <- plot + theme_classic()
  } else if (theme == "gray" | theme == "grey"){
    plot <- plot + theme_grey()
  }

  # Set text size of title and axis labels, determine whether to include a title, and return plot
  if(title == TRUE){
    plot +
      labs(title =  "Histogram of Residuals") +
      theme(plot.title = element_text(size = title.text.size, face = "bold"),
            axis.title = element_text(size = axis.text.size))
  } else if (title == FALSE){
    plot + theme(axis.title = element_text(size = axis.text.size))
  }

}


x <- rnorm(30)
hist(x)
y <- x+rnorm(30,0,1)
plot(x,y)

m <- lm(y~x)

gg <- resid_spanel(resid(m), fitted(m),title = FALSE, theme="classic")
return(gg)
}

gg <- make_plot()
gg


#pic <- readPNG("sticker_plot2.png")
#pic <- rasterGrob(pic, width = 1, x = 0.5, y = 0.61, interpolate = TRUE)

# gg <- ggplot() +
#   annotation_custom(pic) +
#   theme_void()

set.seed(123)
#col_border <- "dodgerblue4"                 # Cambridge Blue
#col_border <- "mediumblue"
col_border <- "blue3"

#col_border <- "navyblue"
#too dark

#col_bg <- "grey90"
col_bg <- "grey25"
#col_bg <- "grey70"

col_text <- col_border
col_text <- "#FFFFFF"
url_color <- col_text
sticker(gg,
        package="ggResidpanel",
        p_size = 12,
        p_y=1.5,
        s_x = 1,
        s_y = .8,
        s_width = 1.4,
        s_height = 1.4,
        h_fill = col_bg,
        h_color = col_border,
        p_family = "Aller_Lt",
        filename="gg_resid_sticker4.png",
        spotlight = FALSE,
        l_x = 1.0,
        l_y = 1.5,
        l_alpha = 0.3,
        p_color = col_text,
        url = "https://github.com/goodekat/ggResidpanel",
        u_color = url_color,
        u_size=3.5)

