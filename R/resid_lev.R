# Residual-Leverage plot.
#
# Creates a plot of the residuals versus leverage from a model.
#
# @param model Model fit using lm.
# @return A plot of residuals versus leverage values from the \code{model}.
# @examples
# model <- lm(Volume ~ Girth, data = trees)
# residlev_plot(model)

resid_lev <- function(model, type, theme, axis.text.size, title.text.size, title){



  # Create a data frame with the leverage values and standardized residuals
  if(class(model)[1]=="lm"){
  model_values <- data.frame(leverage = hatvalues(model),
                             std_res = resid_resid(model, type="standardized"))
  r_label <- resid_label(type="standardized", model)
  }else if (class(model)[1]=="glm"){
    if(is.na(type)|type=="deviance"|type=="stand.deviance"){
      model_values <- data.frame(leverage = hatvalues(model),
                                 std_res = resid_resid(model, type="stand.deviance"))
      r_label <- resid_label(type="stand.deviance", model)
    }else if (type=="pearson"|type=="stand.pearson"){
      model_values <- data.frame(leverage = hatvalues(model),
                                 std_res = resid_resid(model, type="stand.pearson"))
      r_label <- resid_label(type="stand.pearson", model)}
  }
  #Create data for labels

  #Get names of variables
  names_data <- names(model$model)
  #Get data used in model from model
  plotly_data <- as.data.frame(as.matrix(model$model))

  #If binomial, the response variables are two columsn
  if(class(model)[1]=="glm"){
    if(model$family[[1]]=="binomial"){

      #Find first parentheses
      firstp <- as.numeric(gregexpr(pattern ='\\(',names_data[1])[1])
      #Find end parentheses
      lastp <- as.numeric(gregexpr(pattern ='\\)',names_data[1])[1])
      #Find first comma
      firstc <- as.numeric(gregexpr(pattern ='\\,',names_data[1])[1])
      #Grab the name of the number of successes
      names(plotly_data)[1] <- str_sub(names_data[1], {firstp+1}, {firstc-1})

      #Check if the person gave the number of failures or calculated the number
      #of failures
      if (grepl("\\-", names_data[1])){
        #First the '-' sign
        firstm <- as.numeric(gregexpr(pattern ='\\-',names_data[1])[1])
        #Set the name of the total
        names(plotly_data)[2] <- gsub(" ", "",str_sub(names_data[1], {firstc+1}, {firstm-1}))

        #Make sure are numeric
        plotly_data[,1] <- as.numeric(as.character(plotly_data[,1]))
        plotly_data[,2] <- as.numeric(as.character(plotly_data[,2]))

        #Second column needs to contain total so add first two
        plotly_data[,2] <- plotly_data[,1]+plotly_data[,2]
      }else{
        names(plotly_data)[2] <- gsub(" ", "",str_sub(names_data[1], {firstc+1}, {lastp-1}))
      }
    }
  }
  plotly_data$Obs <- 1:nrow(plotly_data)


  #Trim down to 3 decimal places
  for(i in 1:ncol(plotly_data)){
    plotly_data[grepl("\\.", as.character(plotly_data[,i])),i] <- round(as.numeric(as.character(plotly_data[grepl("\\.", as.character(plotly_data[,i])),i])), 3)
  }

  names_data<- names(plotly_data)
  #Add name to rows
  for(i in 1:ncol(plotly_data)){
    plotly_data[,i] <- paste(names_data[i],":" ,plotly_data[,i])
  }


  #Limit to 10 variables showing
  if(ncol(plotly_data)>10){
    plotly_data <- plotly_data[,c(1:9, ncol(plotly_data))]
  }
  #Paste all together
  Data <- plotly_data[,1]
  for(i in 2:ncol(plotly_data)){
    Data <- paste(Data, ",", plotly_data[,{i}])
  }


  # Compute the hat matrix values
  hii <- (infl <- influence(model, do.coef = FALSE))$hat

  # Determine the range of the hat matrix values
  r.hat <- range(hii, na.rm = TRUE)

  # Determine the rank of the model
  p <- model$rank

  # Create a sequence of hat values
  usr <- par("usr")
  hh <- seq.int(min(r.hat[1L], r.hat[2L]/100), usr[2L], length.out = 101)

  # Compute Cook's D values based on the sequence of hat values for two levels
  cl_h1 <- data.frame(hh = hh,
                      pos = sqrt(0.5 * p * (1 - hh) / hh),
                      neg = -sqrt(0.5 * p * (1 - hh) / hh))
  cl_h2 <- data.frame(hh = hh,
                      pos = sqrt(1 * p * (1 - hh) / hh),
                      neg = -sqrt(1 * p * (1 - hh) / hh))

  # Create the residual vs. leverage plot
  plot <- ggplot(data=model_values, aes(x = leverage, y = std_res)) +
    geom_point() +
    labs(x = "Leverage", y =   r_label) +
    expand_limits(x = 0) +
    geom_smooth(color = "red", se = FALSE, method = 'loess', size = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_x_continuous(limits = c(0, max(model_values$leverage, na.rm = TRUE))) +
    scale_y_continuous(limits = extendrange(range(model_values$std_res, na.rm = TRUE), f = 0.08)) +
    geom_line(data = data.frame(cl_h1), aes(x = hh, y = pos), linetype = "dashed", color = "red", na.rm = TRUE) +
    geom_line(data = data.frame(cl_h1), aes(x = hh, y = neg), linetype = "dashed", color = "red", na.rm = TRUE) +
    geom_line(data = data.frame(cl_h2), aes(x = hh, y = pos), linetype = "dashed", color = "red", na.rm = TRUE) +
    geom_line(data = data.frame(cl_h2), aes(x = hh, y = neg), linetype = "dashed", color = "red", na.rm = TRUE)

  Default_Title <- paste(r_label, "vs Leverage")
  # Add theme to plot
  if (theme == "bw"){
    plot <- plot + theme_bw()
  } else if (theme == "classic"){
    plot <- plot + theme_classic()
  } else if (theme == "gray" | theme == "grey"){
    plot <- plot + theme_grey()
  }

  # Set text size of title and axis lables, determine whether to include a title, and return plot
  if(title == TRUE){
    plot +
      labs(title = Default_Title) +
      theme(plot.title = element_text(size = title.text.size, face = "bold"),
            axis.title = element_text(size = axis.text.size))
  } else if (title == FALSE){
    plot + theme(axis.title = element_text(size = axis.text.size))
  }

}
