# UI

ui <- fluidPage(
  
  # Application title
  titlePanel("REACH Question browser"),
  tags$head(
    tags$style(HTML(
      "#table-container {
          overflow: visible !important;
        }"
    )),
    HTML(
      '<meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"/>'
    ),
    includeCSS("www/style.css"),
    HTML(
      '<a style="padding-left:10px;" class="app-title" href= "https://www.reach-initiative.org/" target="_blank"><img src="reach.jpg" height = "50"></a><span class="app-description" style="font-size: 16px; color: #FFFFFF"><strong>Database_test</strong></span>'
    ),
  ),
  
  tabsetPanel(
    tabPanel('Read me',
             # Sidebar layout with input and output definitions
             column(12, 
                    h3("General information"),
                    span(htmlOutput("text_row1"), style = "font-size:20px;")
             ),
             column(6, 
                    h3("Intra-project comparison"),
                    span(htmlOutput("text_column1"), style = "font-size:20px;")
             ),
             column(6, 
                    h3("Within-project comparison"),
                    span(htmlOutput("text_column2"), style = "font-size:20px;")
             ),
             column(12, 
                    h3("Available research cylces"),
                    DTOutput("table1")
             ),
             div(style = "height: 20px;")
    ),
    tabPanel('Intra-project comparison',
             # Sidebar layout with input and output definitions
             sidebarLayout(
               # Sidebar panel for inputs
               sidebarPanel(
                 # Select input for choosing a dataset
                 uiOutput('newNameInput'),
                 div(style = "height: 10px;"),
                 
                 selectInput("table_type",
                             "Select an option:",
                             choices = c('Single comparison', 'Multiple comparison')),
                 
                 # Empty space with CSS styling
                 div(style = "height: 10px;"),
                 actionButton("process", "Build table"),
                 div(style = "height: 10px;"),
                 downloadButton("excel", "Download Excel"),
                 width =2
               ),
               
               # Main panel for displaying outputs
               mainPanel(
                 # Output: DataTable
                 DTOutput("table"),
                 width = 10
               )
             )
    ),
    tabPanel('Within-project comparison',
             sidebarLayout(
               # Sidebar panel for inputs
               sidebarPanel(
                 # Select input for choosing a dataset
                 uiOutput('newNameInput2'),
                 # Empty space with CSS styling
                 div(style = "height: 10px;"),
                 uiOutput('survey_type_input'),
                 # Empty space with CSS styling
                 div(style = "height: 10px;"),
                 actionButton("process_time", "Build table"),
                 div(style = "height: 10px;"),
                 downloadButton("excel_time", "Download Excel"),
                 width =2
               )
               ,
               
               # Main panel for displaying outputs
               mainPanel(
                 # Output: DataTable
                 DTOutput("table_time"),
                 width = 10
               )
             )
    )
    # new tabs go here
    
  )
)
