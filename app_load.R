library(openxlsx)
library(stringr)
library(tidyr)
library(shiny)
library(data.table)
library(DT)
library(dplyr)

# to do
# get the feedback from Joe and Emanuel


source('www/src/stuff.R')
source('www/src/get_db.R')
source('www/src/functions.R')

# load the app
source('appl_ui.R')
source('app_server.R')

shinyApp(ui = ui, server = server)

