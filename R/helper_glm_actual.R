# Actual values from a glm model.

# Finds the number of successes divided by the total

helper_glm_actual <- function(model){
  ###############################################################
  #Create Data to use as labels
  #The only way to get plotly to plot the y, the x's, and the observation when
  #it is not included in aes() is to create a vector where each value of that
  #Vector contains all of the above information. The plotly option 'tooltips' can
  #then be used to select which variables to print in plotly which would be the
  #single variable 'Data' containing all the above information

  #The methods for acquiring the data are different for mixed models so separated
  #based on model type
  if(class(model)[1]%in%c("lm", "glm")){
    #Get names of variables
    names_data <- names(model$model)
    #Get data used in model from model
    plotly_data <- data.frame(as.matrix(model$model))

    #If binomial, the response variables are two columns
    if(class(model)[1]=="glm"){
      if(model$family[[1]]=="binomial"){

        #The next set of code extracts the column names from the
        #cbind statement used to create the number of successes and
        #number of failures

        #Find first parentheses
        firstp <- as.numeric(gregexpr(pattern ='\\(',names_data[1])[1])
        #Find end parentheses
        lastp <- as.numeric(gregexpr(pattern ='\\)',names_data[1])[1])
        #Find first comma
        firstc <- as.numeric(gregexpr(pattern ='\\,',names_data[1])[1])
        #Grab the name of the number of successes which is between the
        #first parentheses and the comma
        names(plotly_data)[1] <- str_sub(names_data[1], {firstp+1}, {firstc-1})

        #Check if the person gave the number of failures or calculated the number
        #of failures
        if (grepl("\\-", names_data[1])){
          #Find the '-' sign: the name of the number of trials will preceed it
          firstm <- as.numeric(gregexpr(pattern ='\\-',names_data[1])[1])
          #Set the name of the trials by extracting everything from the comma
          #to the minus sign and remove extra blanks
          names(plotly_data)[2] <- gsub(" ", "",str_sub(names_data[1], {firstc+1}, {firstm-1}))

          #Make sure are numeric (I no longer need these so commented out)
          plotly_data[,1] <- as.numeric(as.character(plotly_data[,1]))
          plotly_data[,2] <- as.numeric(as.character(plotly_data[,2]))

          #Second column needs to contain total so add first two
          plotly_data[,2] <- plotly_data[,1]+plotly_data[,2]
        }else{
          #if the second column does not contain a minus, the user ented the number
          #of failures so don't add the first two together and grab the name
          #of the failures between the comma and the end parentheses and removed extra
          #blanks
          names(plotly_data)[2] <- gsub(" ", "",str_sub(names_data[1], {firstc+1}, {lastp-1}))
        }
      }
    }

    return(plotly_data[1]/plotly_data[2])
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

          #Make sure are numeric (don't need these lines anymore)
          plotly_data[,1] <- as.numeric(as.character(plotly_data[,1]))
          plotly_data[,2] <- as.numeric(as.character(plotly_data[,2]))

          #Second column needs to contain total so add first two
          plotly_data[,2] <- plotly_data[,1]+plotly_data[,2]
        }else{
          names(plotly_data)[2] <- gsub(" ", "",str_sub(names_data[1], {firstc+1}, {lastp-1}))
        }
      }
    }

    return(as.numeric(plotly_data[,1]/plotly_data[,2]))
  }
  #################################################################

}
