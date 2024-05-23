library(stringr)
library(tidyr)
library(shiny)
library(data.table)
library(DT)
library(dplyr)

# to do 
# nice excel output on multiple-comparison
# check if statements for determining the procedure
# add the tables for within-project comparisons (just a long table, probably)


source('www/src/stuff.R')
source('www/src/get_db.R')


# load the app
source('appl_ui.R')
source('app_server.R')

shinyApp(ui = ui, server = server)
