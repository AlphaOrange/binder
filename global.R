###########################################
##### Binder: Global Dashboard Script #####
###########################################

# Load Packages
library(magrittr)
library(jsonlite)
library(shiny)
library(shinydashboard)
library(rlist)
library(stringr)
library(dplyr)

# Load scripts
source("data_io.R")
source("functions/utils.R")

# Load content
.res <- load_resources()
.data <- load_data()
