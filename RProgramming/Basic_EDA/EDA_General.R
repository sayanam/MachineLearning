library(RColorBrewer)

# Function Name : featureImp_cat_Target
# Description   : Measures the significance value of binomial categorical target variable. Performs t-test
#                 between a numberical independent variable with categoriacal target variable and 
#                 chi-square test between a categorical independent variable and categorical target variable.
# Input parameters: 
#   data       - input data Dataframe
#   num_var    - numberical variable vector
#   fact_var   - factor variable vector
#   targetVar  - target variable
#   conf.level - significance
# Returns : A dataframe of test results
# Limitations : The target variable should be a binary variable.

featureImp_cat_Target <- function(data, num_var, fact_var, targetVar, conf.level = 0.05){
  cat("\n Feature importance between a numerical independent variable and binary categorical variable \n \n")
  targetFactors = unlist(unique(fram_data[targetVar]))
  if(length(targetFactors)> 2 || length(targetFactors) == 1){
    stop("The target variable does not have binary factors")
  }
  
  pvalues = c()
  for(i in 1 : length(num_var)) {
    attributeName = num_var[i]
    pval = t.test( data[attributeName][data[targetVar] == targetFactors[[1]]], 
                   data[attributeName][data[targetVar] == targetFactors[[2]]],
                   conf.level = conf.level)['p.value'][1]
    pval = as.numeric(pval)
    pvalues[i] = pval
  }
  tTestResult <- data.frame( columnName = num_var,
                             p_value = pvalues)
  print(tTestResult)
  tTestResult['Test'] = 't-student'
  cat("\n t-test- Alternate Hypothesis :- Means are not same \n \n \n")
  cat("Feature importance between categorical independent variable and categorical target variable \n \n")
  
  pvalues = c()
  for (i in 1 :length(fact_var)){
    attributeName = fact_var[i]
    pval = chisq.test(data[[attributeName]],data[[targetVar]])['p.value']
    pval = as.numeric(pval)
    pvalues[i] = pval
  }
  ChiTestResult <- data.frame( columnName = fact_var,
                               p_value = pvalues)
  print(ChiTestResult)
  ChiTestResult['Test'] = 'chi-squared'
  cat("ChiSquare-test- Alternate Hypothesis :- Variables are Independent")
  
  finalResult <- rbind(tTestResult,ChiTestResult)
  return (finalResult)
}


##############################################################################################

# Function name : plot_data
# Description   : Plots histograms and box plot for numerical data, plots bar graph and pie chart for 
#                 categorical columns for the input data provided
# Input parameters :
#   data     - Input data fram.
#   num_var  - column names that are numerical
#   fact_var - column names that are categorical
#

plot_data <- function(data, num_var, fact_var){
  for (colName in num_var){
    par(mfrow=c(2,1))
    hist(data[[colName]], main=paste("Histogram of",colName, sep = " "), xlab=colName, col="orange")
    boxplot(data[[colName]], main = paste('Box plot of',colName, sep=' '), col = "orange", border = "grey5",
            horizontal = T)
  }
  
  for (colName in  fact_var){
    barplot(table(data[[colName]]), col="orange", 
            main= paste("Bar plot for",colName, sep=' '), 
            ylab= 'Frequency', xlab = 'Values')

    noOfFactors = length(unique(data[[colName]]))
    
    if (noOfFactors <= 2){
      mycolor <- brewer.pal(3, "Set2")
    }else{
      mycolor <- brewer.pal(length(unique(data[[colName]])), "Set2")
    }
    pie(table(data[[colName]]), main=paste('Pi Chart',colName,sep=' '), col=mycolor)
  }
}

##############################################################################################

# Function name : create_unique_dir
# Description : creates a unique directory, if the input directory is already present, it appends the directory
#               with an incremetal number unitl a directory that is not already present is created.
# Input parameter:
#   dir_location - location in which the directory is created.
#   dir_name     - name of the directory to be created

create_unique_dir <- function(dir_location, dir_name){
  temp_dir = paste(dir_location,dir_name, sep='\\')
  i = 1
  if (dir.exists(temp_dir)){
    temp_dir = paste(temp_dir, i, sep="_")
  }
  temp_dir
  while(dir.exists(temp_dir)){
    i = i + 1
    temp = unlist(strsplit(temp_dir,""))
    temp[length(temp)] = i  
    temp_dir = paste0(temp, collapse = "")
  }
  dir.create(temp_dir)
  return (temp_dir)
}


##############################################################################################

# Function name : plot_data_png_Files
# Description   : Plots histograms and box plot for numberical data, plots bar graph and pie chart for 
#                 categorical columns for the input data provided and saves them as .png files
# Input parameters :
#   data     - Input data fram.
#   num_var  - column names that are numerical
#   fact_var - column names that are categorical
#   png_dir  - directory in which the files are to be stored

plot_data_png_files <- function(data, num_var, fact_var, png_dir=getwd()){
  
  if(! dir.exists(png_dir)){
    stop("Given directory location does not exist!!")
  }
  dir = create_unique_dir(png_dir,'plots')
  
  for (colName in num_var){
    png(paste(dir,'\\',colName, ".png", sep=""))
    
    par(mfrow=c(2,1))
    hist(data[[colName]], main=paste("Histogram of",colName, sep = " "), 
         xlab=colName, col="orange")
    
    boxplot(data[[colName]], main = paste('Box plot of',colName, sep=' '), col = "orange", border = "grey5",
            horizontal = T)
    dev.off() 
  }
  
  for (colName in  fact_var){
    png(paste(dir,'\\',colName, ".png", sep=""))
    
    par(mfrow=c(2,1))
    barplot(table(data[[colName]]), col="orange", 
            main= paste("Bar plot for",colName, sep=' '), 
            ylab= 'Frequency', xlab = 'Values')
    
    noOfFactors = length(unique(data[[colName]]))
    
    if (noOfFactors <= 2){
      mycolor <- brewer.pal(3, "Set2")
    }else{
      mycolor <- brewer.pal(length(unique(data[[colName]])), "Set2")
    }
    pie(table(data[[colName]]), main=paste('Pi Chart',colName,sep=' '), col=mycolor)
    
    dev.off()
  }
}

##############################################################################################

# Function name : plot_data_pdf_file
# Description   : Plots histograms and box plot for numberical data, plots bar graph and pie chart for 
#                 categorical columns for the input data provided and saves them in a single pdf file.
# Input parameters :
#   data     - Input data fram.
#   num_var  - column names that are numerical
#   fact_var - column names that are categorical
#   png_dir  - directory in which the files are to be stored

plot_data_pdf_file <- function(data, num_var, fact_var, pdf_dir=getwd()){
  if(! dir.exists(pdf_dir)){
    stop("Given directory location does not exist!!")
  }
  pdf(paste(pdf_dir,"myplot.pdf", sep='\\'))
  plot_data(data,num_var,fact_var)
  dev.off()
}

##############################################################################################

# Function name : get_factor_attributes
# Description   : gets the attribute names for which the dataframe column is factor 

get_factor_attributes <- function(data){
  factorAttributes = c()
  attributes = names(data)
  for (attribute in attributes){
    if (is.factor(data[[attribute]])){
      factorAttributes <- c(factorAttributes,attribute)
    }
  }
  return(factorAttributes)
}

##############################################################################################


# Function name : get_not_factor_attributes
# Description   : gets the attribute names for which the dataframe column is not factor 

get_not_factor_attributes <- function(data){
  nonFactorAttributes = c()
  attributes = names(data)
  for (attribute in attributes){
    if (!(is.factor(data[[attribute]]))){
      nonFactorAttributes <- c(nonFactorAttributes,attribute)
    }
  }
  return(nonFactorAttributes)
}


##############################################################################################

# Function Name : transform_data_to_factor_based_on_proportions
# Description :
# Input Parameters:
#     minprop - if the proportion of unique elements is less that 0.01(default) then that column is converted to 
#               factor variable
#     data    - input data

transform_data_to_factor_based_on_proportions <- function(data, minPorp = 0.01){
  nonFactorAttributes = get_not_factor_attributes(data)
  totalRowCount = dim(data)[1]
  for (attribute in nonFactorAttributes){
    uniqueRowCount = length(unlist(unique(data[attribute])))
    if((uniqueRowCount/totalRowCount) < minPorp){
      data[attribute] <- as.factor(data[[attribute]])
    }
  }
  return (data)
}

##############################################################################################

# Function name : univariate_plots
# Description name : Generates appropriate univariate plots for a given data
# Input parameter :
#     data : Data for which the univariate plots are to be generated, dataframe
#     minProp : Min proportion of uniqure values in an attribute, below which the column is transformed to 
#               factor. (Default = 0.01)
#     outputType : dis - display the plots, pdf - pdf format of the plots, png - png format of the plots
#                 Default - dis
#     dir_loc : Location in which the png and pdf's are to be stored. Default - current working directory    
#     tranformData : Can contain True or False. Transforms the data as per minPorp if True. Default is False.

univariate_plots <- function(data, outputType='dis', minPorp = 0.01, dir_loc = getwd(),
                             tranformData = FALSE){
  if(tranformData){
    transformedData =  transform_data_to_factor_based_on_proportions(data, minPorp)
  } else{
    transformedData = data
  }
  fact_var = get_factor_attributes(transformedData)
  num_var = get_not_factor_attributes(transformedData)
  
  if(outputType == 'dis'){
    plot_data(transformedData, num_var, fact_var)
  }else if(outputType == 'png'){
    if(! dir.exists(dir_loc)){
      stop("The entered directory location is invalid !!")  
    }
    plot_data_png_files(transformedData, num_var, fact_var, png_dir=dir_loc)
  }else if(outputType == 'pdf'){
    if(! dir.exists(dir_loc)){
      stop("The entered directory location is invalid !!")  
    }
    plot_data_pdf_file(transformedData, num_var, fact_var, pdf_dir=dir_loc)
  }else{
    cat("Entered value for output type is invalid. Displaying the plots")
    plot_data(transformedData, num_var, fact_var)
  }
}

##############################################################################################


# Function Name : featureImp_cat_Target_gen
# Description :
# Input parameters: 
#   data       - input data Dataframe
#   targetVar  - target variable
#   conf.level - significance
#   minProp : Min proportion of uniqure values in an attribute, below which the column is transformed to 
#               factor. (Default = 0.01)
#   tranformData : Can contain True or False. Transforms the data as per minPorp if True. Default is False.
# Returns : A dataframe of test results
# Limitations : The target variable should be a binary variable.
featureImp_cat_Target_gen <- function(data, targetVar, conf.level = 0.05, 
                                      tranformData=FALSE, minPorp=0.01){
  if(tranformData){
    transformedData =  transform_data_to_factor_based_on_proportions(data, minPorp)
  } else{
    transformedData = data
  }
  fact_var = get_factor_attributes(transformedData)
  num_var = get_not_factor_attributes(transformedData)
  print(fact_var)
  print(num_var)
  fact_var = fact_var[ fact_var != targetVar]
  print(fact_var)
  return (featureImp_cat_Target(transformedData, num_var, fact_var, targetVar, conf.level))
}

##############################################################################################



