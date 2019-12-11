if(!require("pacman")){
  install.packages("pacman")
} 

pacman::p_load(readr, stringr, tidyverse, mice, reshape2, lubridate, caret, grid, gridExtra,
               devtools,corrplot,factoextra, cluster, magrittr)

#### LIBRARY DESCRIPTIONS ------------------------------
# Basic libraries
library (readr)
library (stringr) 
library (tidyverse) 
library (mice)
library (reshape2)

# # Data Import ----------------------------------------
# library(rio) 
# # Makes assumptions about the file format guessing the format 
# # of the file to import, applies import functions appropriate to that format.
# 
# # All purpose ----------------------------------
library(Hmisc)
# # Functions useful for data analysis
# 
# #Correlation -----------------------------------

# library(GGally)
# # View Correlation Matrix
 
 library(corrplot)

# # Plots -----------------------------------------
# library(ggpubr) 
# # Arrange plots on page

 library(grid) 
# # Rewrite the graphics layout capabilities

 library(gridExtra) 
# # Arrange multiple grid-based plots on a page

# library(cowplot) 
# # ggplot2 addon to create publication-quality figures
# 
# # TS Libraries -----------------------------------

 library(lubridate)
# # Makes it easy to work with dates-times

# library(zoo) 
# # Aimed at irregular time series of numeric vectors/matrices and factors.

# libraries for Caret, Apriori, Arima Models
 library(caret)

# library(forecast) 
# # Methods and tools for displaying and analysing univariate time series forecasts including automatic ARIMA modelling.

# library(ggfortify) 
# # Plot directly from a timeseries

# library(tseries) 
# # Time series analysis and computational finance.

# library(imputeTS) 
# # Provides plotting and printing functions of time series missing data statistics.

# library(stats) 
# # HoltWinters model


# # CLUSTERING ANALYSIS ------------------------------
 library (vcd)
# Visualizing Categorical Data

library(devtools)
# install_github("ujjwalkarn/xda")
library (xda) # load with devtools

# # Report -------------------------------------------
# library(kableExtra)



#### VARIABLES       ---------------------------------------------------------