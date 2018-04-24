# Histogram of Residuals.
#
# Creates a histogram of the residuals from a model.
#
# @param model Model fit using either lm, glm, lmer, or glmer.
# @return A histogram of the residuals from the \code{model} with a normal
# @param type The user may specify a type of residuals to use, otherwise the default residual type for each model is used.
# density curve overlaid with mean equal to the mean of the residuals and
# standard deviation equal to the standard deviation of the residuals.
# @examples
# model <- lm(Volume ~ Girth, data = trees)
# resid_hist(model)


resid_hist <- function(model, type,bins, theme, axis.text.size, title.text.size, title){

  #If bins=NA, use default
  if(is.na(bins)){
    bins <- 30
  }

  #call function to return appropriate residual label
  r_label <- resid_label(type, model)
  # Create a data frame with the residuals
  if(is.na(type)){
    model_values <- data.frame(resid = resid_resid(type=NA, model=model))
  }else{
    model_values <- data.frame(resid = resid_resid(type=type, model=model))
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


  Default_Title <- paste("Histogram of", r_label)
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
    plot <- ggplot(model_values, aes(x = resid, label=Data)) +
      geom_histogram(aes(y = ..density.., fill = ..count..),
                     color = "black", fill = "grey82", bins = bins) +
      stat_function(fun = dnorm, color = "blue",
                    args = list(mean = 0,
                                sd = sd(model_values$resid))) +
      labs(x = r_label, y = "Density") +
      theme(plot.title = element_text(size = 12, face = "bold"),
            axis.title = element_text(size = 10))
  }else{
  # Create the histogram of residuals
  plot <- ggplot(model_values, aes(x = resid, label=Data)) +
    geom_histogram(aes(y = ..density.., fill = ..count..),
                   color = "black", fill = "grey82", bins = bins) +
    stat_function(fun = dnorm, color = "blue",
                  args = list(mean = 0,
                              sd = sd(model_values$resid))) +
    labs(x = r_label, y = "Density") +
      xlim(c(min_x, max_x))+
    theme(plot.title = element_text(size = 12, face = "bold"),
          axis.title = element_text(size = 10))+
    labs(x = r_label, y = "Density")
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
      labs(title =  Default_Title) +
      theme(plot.title = element_text(size = title.text.size, face = "bold"),
            axis.title = element_text(size = axis.text.size))
  } else if (title == FALSE){
    plot + theme(axis.title = element_text(size = axis.text.size))
  }

}
