# Residual Plot.
#
# Creates a residual plot with residuals versus predicted values from a model.
#
# @param model Model fit using either lm, glm, lmer, or glmer.
# @param smoother Indicates whether or not to include a smoother on the residual plot.
# Specify TRUE or FALSE. Default is set to FALSE.
# @return A plot of the residuals versus predicted values from the \code{model}
#  with a horizontal line through 0.
# @examples
# model <- lm(Volume ~ Girth, data = trees)
# resid_plot(model)

resid_plot <- function(model, type, smoother, theme, axis.text.size, title.text.size, title){



  r_label <- resid_label(type, model)
  Default_Title <- paste(r_label,"Plot")
  # Create a data frame with the residuals
  if(is.na(type)){
    model_values <- data.frame(resid = resid_resid(type=NA, model=model),
                               pred = fitted(model))
  }else{
    model_values <- data.frame(resid = resid_resid(type=type, model=model),
                               pred = fitted(model))
  }

  #Create Data to use as labels

  if(class(model)[1]%in%c("lm", "glm")){
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
    }else if (class(model)[1]%in%c("lmerMod", "glmerMod")) {
    names_data <- names(model@frame)
    plotly_data <- data.frame(as.matrix(model@frame))

    #If binomial, the response variables are two columsn
    if(class(model)[1]=="glmerMod"){
      if(model@resp$family[[1]]=="binomial"){

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

      }
  # Create the residual plot
  plot <- ggplot(data=model_values, aes(x = pred, y = resid,label=Data)) +
    geom_point() +
    geom_abline(slope = 0, intercept = 0, color = "blue") +
    labs(x = "Predicted Values", y = r_label)


  # If smoother is set to true, add it to the plot
  if (smoother == TRUE){
   plot <- plot +
     geom_smooth(colour = "red", se = FALSE, method = "loess", size = 0.5)
  }

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
