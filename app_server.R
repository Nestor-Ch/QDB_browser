server <- function(input, output) {
  # ----------------------------readme -------------------------------------------------
  
  output$text_row1 <- renderText({
    'This app allows the user to browse the questionnaire bank of the Reach Database. The user can select
    different projects to view or view the entirety of the database. The pages also allow for different 
    types of view of the matching tables, if that was desired.
    The tables that are produced by this programme are browseable. Each table has a filter pane on the top.
    The user can filter the output table by the question of interest, variable type and type of survey that
    was conducted among others. The user can  also download the resulting table as an Excel file if they
    click the `Download Excel button`
    <br>
    <br>'
  })
  
  
  output$text_column1 <- renderText({
    '<strong>Single comparison.</strong><br>
    This page allows the user to select the project of interest and browse the database for
    all of the questions that match the questions of the selected project. For example, selecting UKR2308
    will show the user all of the questions that match between UKR2308 and the rest of our questionnaires.
    The user can select multiple projects or leave the field blank to view the entire database.
    <br>
    <strong>Multiple comparison.</strong><br>
    To use this capability of the browser the user has to select 2 or more projects in the dropdown menu.
    Selecting this type of view will build a wide table for the matching questions of the selected projects.
    Each row will represent a question with it\'s details (question type, sector, survey_type, etc.) with
    columns being grouped by the projects where the question in the row has a match.
    '
  })
  
  # Render text in the second column
  output$text_column2 <- renderText({
    '<strong>General information.</strong><br>
    This page allows the user to view all of the matching questions that match across multiple rounds
    within a project. So, if the question `a` was asked in rounds 1,3,10 of the project, the user will
    be able to see this. If there are multiple types of surveying within the project, the user will have to
    specify exactly what kind of survey they\'d like to browse (e.g. Household survey vs Key informant survey
    under the same project ID).The matching questions are color coded for easy browsing.
    '
  })
  
  # --------------------------Research cycle table
  database_research_cycle <- dbGetQuery(my_connection , "SELECT * from Research_cycle_db")
  
  project_table <- reactive({
    database_proj_dist <- dbGetQuery(my_connection , "SELECT DISTINCT project_ID, round_ID from Reach_QDB")
    database_proj_count<- dbGetQuery(my_connection , "SELECT project_ID, COUNT(*) as N_questions from Reach_QDB GROUP BY project_ID")
    if(nrow(database_proj_dist)>0){
      
      database_proj_rounds <- database_proj_dist %>% 
        mutate(round_ID = as.numeric(round_ID)) %>% 
        group_by(project_ID) %>% 
        arrange(project_ID,round_ID) %>% 
        summarise(rounds = numbers_to_string(round_ID)) %>% 
        ungroup()
      
      database_proj_count %>% 
        inner_join(database_research_cycle %>% 
                     rename(project_ID=Research_cycle_ID)) %>% 
        inner_join(database_proj_rounds) %>% 
        relocate(project_ID,Name,rounds,N_questions)
    }else{data.frame()}
    
  })
  
  
  output$table1 <- DT::renderDataTable({
    DT::datatable(project_table())
  })
  
  # ---------------------------intra project comparison --------------------------------
  
  database_project <- dbGetQuery(my_connection , "SELECT * from Reach_QDB")
  
  
  # get the list of survey IDs for the user to select
  # survey id
  assessment_value <- reactive({
    txt <- unique(database_project$project_ID)
    txt
  })
  
  # Render the text input field with the default value
  output$newNameInput <- renderUI({
    choices <- assessment_value() 
    selectInput("newName",
                "Select an option:",
                choices = c('',choices),
                multiple = TRUE,
                selected = '')
  })
  
  
  datasetInput <- reactiveVal(NULL)
  
  
  # # Reactive expression to fetch the selected project
  observeEvent(input$process,{
    if(!is.null(input$newName)){
      needed_ids <- database_project %>% 
        filter(project_ID %in% input$newName)%>% 
        pull(true_ID) %>% unique()
    }
    
    if(input$table_type == 'Single comparison' & !is.null(input$newName)){
      df <- database_project %>% 
        filter(true_ID %in% needed_ids)
    }else if (input$table_type != 'Single comparison' & !is.null(input$newName)){
      df <- database_project %>% 
        filter(project_ID %in% input$newName)
    }else if (is.null(input$newName)){
      df <- database_project
    }
    
    if(input$table_type != 'Single comparison' &length(input$newName)<2){
      removeModal()
      showModal(
        modalDialog(
          title = "Error",
          "You've selected multiple comparison but chosen <2 Research cycles for comparison.",
          footer = NULL,
          easyClose = TRUE
        )
      )
    }else{
      
      showModal(
        modalDialog(
          title = "Processing",
          "Fetching your request from the DB.",
          footer = NULL,
          easyClose = TRUE
        )
      )
      
      
      df <- df %>% 
        mutate(TABLE_ID = paste0(project_ID,'_R',round_ID,'_',survey_type)) %>% 
        filter(!grepl('\\bsettlement|\\brectangle\\b|\\brectangles\\b|geo_location|\\bpoint\\b|\\bhub\\b|raion|hromada|oblast|center_idp',list_name))
      
      table_IDs <- df %>% pull(TABLE_ID) %>% unique()
      
      
      tool_choices <- dbGetQuery(my_connection , 
                                 paste0("SELECT * from Choices_DB where TABLE_ID IN ('",
                                        paste0(table_IDs, collapse = "', '"),"')"))
      
      tool_survey <- dbGetQuery(my_connection , 
                                paste0("SELECT * from Survey_DB where TABLE_ID IN ('",
                                       paste0(table_IDs, collapse = "', '"),"')"))
      
      df_choices_added <- df %>% 
        tibble() %>% 
        group_by(project_ID) %>% 
        distinct(true_ID, .keep_all = TRUE) %>% 
        ungroup()
      
      
      
      df_choices_added <- df_choices_added %>% 
        left_join(tool_choices %>% 
                    select(-c(order,load_time)) %>% 
                    rename(choice_name =name,
                           english_choices = `Label::English` ,
                           ukrainian_choices = `Label::Ukrainian`,
                           russian_choices = `Label::Russian`
                    ) %>% 
                    group_by(TABLE_ID,list_name) %>% 
                    summarise(across(everything(), ~ paste0(.x, collapse=',\n'))) %>% 
                    ungroup() %>% 
                    mutate(list_name = str_squish(list_name))
        ) %>% 
        left_join(tool_survey %>% 
                    select(name, contains('Label'), TABLE_ID) %>% 
                    rename(                           
                      english_question = `Label::English` ,
                      ukrainian_question = `Label::Ukrainian`,
                      russian_question = `Label::Russian`
                    ) %>% 
                    mutate(name = str_squish(name))
        ) 
      
      check_n_proj <- unique(df_choices_added$project_ID)
      
      if(!is.null(input$newName)){
        df_choices_added <- df_choices_added %>% 
          filter(!is.na(english_question)) %>% 
          group_by(true_ID) %>% 
          mutate(cnt = n()) %>%
          ungroup() %>%
          filter(cnt > 1)
      }
      
      check_n_proj_2 <- unique(df_choices_added$project_ID)
      
      
      if(nrow(df_choices_added)>0){
        
        if(input$table_type == 'Single comparison'){
          df_choices_added <- df_choices_added%>% 
            arrange(true_ID)%>%
            select(true_ID,
                   sector,TABLE_ID,q.type,name,english_question, ukrainian_question, russian_question,
                   english_choices, ukrainian_choices, russian_choices
            ) %>% 
            filter(!sector %in% 'Interview component')
          
          removeModal()
          datasetInput(df_choices_added)
        }else if(input$table_type == 'Multiple comparison' & length(input$newName)>=2){
          
          data_wide <- df_choices_added %>%
            select(true_ID,sector,TABLE_ID,q.type,name,english_question, ukrainian_question, russian_question,
                   english_choices, ukrainian_choices, russian_choices
            ) %>% 
            group_by(true_ID) %>% 
            pivot_wider(
              names_from = TABLE_ID,
              values_from = c(
                sector,q.type,name,english_question, ukrainian_question, russian_question,
                english_choices, ukrainian_choices, russian_choices
              ),
              names_sep = "_set_"
            )  %>% 
            ungroup()
          
          names_order <- paste0(rep(paste0(c('sector','q.type','name','english_question', 'ukrainian_question', 'russian_question',
                                             'english_choices', 'ukrainian_choices', 'russian_choices'
          ),'_set_'),2),rep(unique(df_choices_added$TABLE_ID),each=9))
          data_wide <- data_wide[,names_order]
          
          removeModal()
          datasetInput(data_wide)
        }else{
          datasetInput(NULL)
        }
        
        if(length(check_n_proj)>length(check_n_proj_2)){
          
          showModal(
            modalDialog(
              title = "Warning",
              paste0(paste0(setdiff(check_n_proj,check_n_proj_2),collapse=', '),
                     ' Has no match with other projects and will be omitted from the final table'),
              footer = NULL,
              easyClose = TRUE
            )
          )
        }
        
        
        
      }else{
        removeModal()
        showModal(
          modalDialog(
            title = "No data",
            "The chosen Project_ID doesn't have any matching questions with the rest of data in the DB",
            footer = NULL,
            easyClose = TRUE
          )
        )
        datasetInput(NULL)
      }
    }
    
  })
  
  
  # # Render the DataTable
  output$table <- renderDT({
    
    df <- datasetInput()
    if(is.null(df)){
      return(NULL)
    }else{
      
      if(input$table_type == 'Single comparison'){
        n_groups <- length(unique(df$true_ID))
        
        colors <- rep_len(c('#D3D3D3','white'),n_groups)
        
        names(colors) <- unique(df$true_ID)
        if(nrow(df)>0){
          df <- df %>% 
            mutate(across(any_of(c('sector','q.type', 'TABLE_ID')), ~ as.factor(.x)))
          
          datatable(df,
                    filter = "top",
                    class = list(stripe = FALSE),
                    options = list(
                      dom = 'lfrtipB',
                      pageLength = 100,
                      scrollX=TRUE,
                      autoWidth = TRUE,
                      columnDefs = list(
                        list(targets = 'true_ID', visible = FALSE)
                      )
                    )
          ) %>% 
            formatStyle(
              'true_ID',
              target = 'row',
              backgroundColor = styleEqual(names(colors), colors)
            )
        }
      }else if(input$table_type == 'Multiple comparison' & length(input$newName)>=2 &
               !is.null(input$newName)){
        
        if(all(unique(gsub('(.*\\_set_)|(\\_R.*)','',names(df))) %in% input$newName)){
          if(nrow(df)>0){
            relevant_cols <- which(grepl('sector',names(df)))
            relevant_cols <- relevant_cols[relevant_cols>8]
            
            
            tbl <- datatable(
              df,
              filter = "top",
              class = list(stripe = FALSE),
              options = list(
                dom = 'lfrtipB',
                pageLength = 100,
                columnDefs = list(
                  list(targets = "_all", width = '15px')  # Adjust the width as needed
                ),
                rownames = FALSE,
                initComplete = JS(
                  "function(settings, json) {",
                  "  var header = '<tr><th></th>';",
                  "  var columns = settings.aoColumns.map(col => col.sTitle);",
                  "  var projectIDs = [];",
                  "  columns.forEach(function(col) {",
                  "    var parts = col.split('_set_');",
                  "    if (parts.length > 1 && !projectIDs.includes(parts[1])) {",
                  "      projectIDs.push(parts[1]);",
                  "    }",
                  "  });",
                  "  projectIDs.forEach(function(id) {",
                  "    var colspan = columns.filter(col => col.includes(id)).length;",
                  "    header += '<th colspan=\"' + colspan + '\">' + id + '</th>';",
                  "  });",
                  "  header += '</tr>';",
                  "  $(settings.nTHead).prepend(header);",
                  "  // Modify the second row headers",
                  "  $(settings.nTHead).find('tr:eq(1) th').each(function() {",
                  "    var text = $(this).text().replace(/_set_.*/, '');",
                  "    $(this).text(text);",
                  "  });",
                  "}"
                )
              )
            )%>%
              formatStyle(relevant_cols, `border-left` = "solid 2px #000") %>% 
              formatStyle(0:ncol(df), `border-bottom` = "solid 2px #000") %>% 
              formatStyle(0:ncol(df),target = 'cell',
                          backgroundColor = styleEqual(NA, '#ededed')
              )
            
            
            return(tbl)
            
          }
        }
      }
    }
  })
  
  output$excel <- downloadHandler(
    filename = function() {
      paste("data", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      excel_frame <-datasetInput()
      # Create a workbook
      wb <- createWorkbook()
      
      # Add a worksheet
      addWorksheet(wb, "Data")
      
      if(input$table_type == 'Multiple comparison'){
        old_names <- names(excel_frame)
        new_row <- gsub("\\_set_.*","",old_names)
        excel_frame <- rbind(new_row,excel_frame)
        
        new_names <- gsub(".*\\_set_","",old_names)
        names(excel_frame) <- new_names
      }else{
        excel_frame <- excel_frame %>% select(-any_of(c('true_ID')))
      }
      
      # Write data to the worksheet
      writeData(wb, "Data", x = excel_frame, startCol = 1, startRow = 1, rowNames = FALSE)
      
      # create a new style for this thing
      custom_style <- createStyle(wrapText =TRUE)
      grey_rows <- createStyle(fgFill = 'lightgrey',wrapText =TRUE,border ='TopBottom')
      white_rows <- createStyle(fgFill = 'white',wrapText =TRUE,border ='TopBottom')
      
      
      addStyle(wb, sheet = "Data", custom_style, rows = 1:(nrow(excel_frame)+1), 
               cols = 1:ncol(excel_frame), gridExpand = TRUE)
      
      if(input$table_type == 'Single comparison'){
        true_id_list <- unique(excel_frame$true_ID)
        true_id_even <- true_id_list[seq(1, length(true_id_list), 2)]
        rows_to_format <- which(excel_frame$true_ID %in% true_id_even)+1
        
        addStyle(wb, sheet = "Data", grey_rows, rows = rows_to_format, 
                 cols = 1:ncol(excel_frame), gridExpand = TRUE)
      }else{
        ind_beginnings <- which(grepl('sector',excel_frame[1,]))
        ind_endings <- which(grepl('russian_choices',excel_frame[1,]))
        
        ind_beginnings <- ind_beginnings[seq(1,length(ind_beginnings),2)]
        ind_endings <- ind_endings[seq(1,length(ind_endings),2)]
        
        columns_to_format <- lapply(1:length(ind_beginnings),function(x){ind_beginnings[x]:ind_endings[x]}) %>% 
          unlist()
        
        columns_to_format2 <- setdiff(1:ncol(excel_frame),columns_to_format)
        
        addStyle(wb, sheet = "Data", grey_rows, rows = 1:(nrow(excel_frame)+1), 
                 cols = columns_to_format, gridExpand = TRUE)
        
        addStyle(wb, sheet = "Data", white_rows, rows = 1:(nrow(excel_frame)+1), 
                 cols = columns_to_format2, gridExpand = TRUE)
        
        # merge the cells with identical TABLE_ID
        ls_names <- unique(names(excel_frame))
        
        centered_style_grey <- createStyle(halign = "center", valign = "center",fgFill = 'lightgrey')
        centered_style_white <- createStyle(halign = "center", valign = "center",fgFill = 'white')
        
        for(name in ls_names){
          range <- which(names(excel_frame) %in% name)
          mergeCells(wb, "Data", cols = range, rows = 1)
          if(all(range %in% columns_to_format)){
            addStyle(wb, sheet = "Data", style = centered_style_grey,
                     cols = range, rows = 1, gridExpand = TRUE)
          }else{
            addStyle(wb, sheet = "Data", style = centered_style_white,
                     cols = range, rows = 1, gridExpand = TRUE)
          }
        }
      }
      
      border_style <- createStyle(border ='Left')
      
      addStyle(wb, sheet = "Data", border_style, rows = 1:(nrow(excel_frame)+1), 
               cols = ncol(excel_frame)+1, gridExpand = TRUE)
      
      setColWidths(wb,'Data',1:ncol(excel_frame),widths =30)
      
      # Write merged data to the worksheet
      writeData(wb, "Data", excel_frame,
                startCol = 1, startRow = 1, rowNames = FALSE)
      
      # Save the workbook
      saveWorkbook(wb, file)
    }
  )
  
  
  # ---------------------------within project comparison --------------------------------
  
  
  assessment_value_time <- reactive({
    txt <- database_project %>% 
      distinct(project_ID,round_ID) %>% 
      group_by(project_ID) %>%  
      summarise(cnt = n()) %>% 
      ungroup() %>% 
      filter(cnt>1) %>% 
      pull(project_ID)
    txt
  })
  
  output$newNameInput2 <- renderUI({
    choices <- assessment_value_time() 
    selectInput("newName_time",
                "Select an option:",
                choices = c('',choices),
                multiple = FALSE,
                selected = '')
  })
  
  
  surveytype <- reactive({
    req(input$newName_time)
    
    txt <- database_project %>% filter(project_ID %in% input$newName_time) %>% pull(survey_type) %>% unique()
    txt
  })
  
  output$survey_type_input <- renderUI({
    req(surveytype())
    
    choices <- surveytype() 
    selectInput("survey_type",
                "Select an option:",
                choices = choices,
                multiple = FALSE)
  })
  
  
  datasetInput_time <- reactiveVal(NULL)
  
  # # Reactive expression to fetch the selected project
  observeEvent(input$process_time,{
    req(input$survey_type)
    
    df <- database_project %>% 
      filter(project_ID %in% input$newName_time,
             survey_type %in% input$survey_type)
    
    
    showModal(
      modalDialog(
        title = "Processing",
        "Fetching your request from the DB.",
        footer = NULL,
        easyClose = TRUE
      )
    )
    
    
    
    
    
    df <- df %>% 
      mutate(TABLE_ID = paste0(project_ID,'_R',round_ID,'_',survey_type)) %>% 
      filter(!grepl('\\bsettlement|\\brectangle\\b|\\brectangles\\b|geo_location|\\bpoint\\b|\\bhub\\b|raion|hromada|oblast|center_idp',list_name))
    
    table_IDs <- df %>% pull(TABLE_ID) %>% unique()
    
    
    tool_choices <- dbGetQuery(my_connection , 
                               paste0("SELECT * from Choices_DB where TABLE_ID IN ('",
                                      paste0(table_IDs, collapse = "', '"),"')"))
    
    tool_survey <- dbGetQuery(my_connection , 
                              paste0("SELECT * from Survey_DB where TABLE_ID IN ('",
                                     paste0(table_IDs, collapse = "', '"),"')"))
    
    
    
    df_choices_added <- df %>% 
      left_join(tool_choices %>% 
                  select(-c(order,load_time)) %>% 
                  rename(choice_name =name,
                         english_choices = `Label::English` ,
                         ukrainian_choices = `Label::Ukrainian`,
                         russian_choices = `Label::Russian`
                  ) %>% 
                  group_by(TABLE_ID,list_name) %>% 
                  summarise(across(everything(), ~ paste0(.x, collapse=',\n'))) %>% 
                  ungroup() %>% 
                  mutate(list_name = str_squish(list_name))
      ) %>% 
      left_join(tool_survey %>% 
                  select(name, contains('Label'), TABLE_ID) %>% 
                  rename(                           
                    english_question = `Label::English` ,
                    ukrainian_question = `Label::Ukrainian`,
                    russian_question = `Label::Russian`
                  ) %>% 
                  mutate(name = str_squish(name))
      ) 
    
    
    df_choices_added <- df_choices_added %>% 
      filter(!is.na(english_question)) %>% 
      group_by(true_ID, project_ID) %>% 
      mutate(cnt = n()) %>%
      ungroup() %>%
      filter(cnt > 1)
    
    
    if(nrow(df_choices_added)>0){
      
      df_choices_added <- df_choices_added%>% 
        mutate(round_ID = as.numeric(round_ID)) %>% 
        arrange(true_ID, round_ID)%>%
        select(true_ID,
               sector,TABLE_ID,q.type,name,english_question, ukrainian_question, russian_question,
               english_choices, ukrainian_choices, russian_choices
        ) %>% 
        filter(!sector %in% 'Interview component')
      
      datasetInput_time(df_choices_added)
      removeModal()
    }else{
      removeModal()
      showModal(
        modalDialog(
          title = "No data",
          "The chosen Project_ID doesn't have any matching questions within it's rounds",
          footer = NULL,
          easyClose = TRUE
        )
      )
      datasetInput_time(NULL)
      
    }
    
    
  })
  
  output$table_time <- renderDT({
    
    df <- datasetInput_time()
    if(is.null(df)){
      return(NULL)
    }
    
    n_groups <- length(unique(df$true_ID))
    
    colors <- rep_len(c('#D3D3D3','white'),n_groups)
    
    names(colors) <- unique(df$true_ID)
    
    df <- df %>% 
      mutate(across(c(sector,q.type, TABLE_ID), ~ as.factor(.x)))
    
    datatable(df,
              filter = "top",
              class = list(stripe = FALSE),
              options = list(
                dom = 'lfrtipB',
                pageLength = 100,
                scrollX=TRUE,
                autoWidth = TRUE,
                columnDefs = list(
                  list(targets = 'true_ID', visible = FALSE)
                )
              )
    ) %>% 
      formatStyle(
        'true_ID',
        target = 'row',
        backgroundColor = styleEqual(names(colors), colors)
      )
    
  })
  
  
  output$excel_time <- downloadHandler(
    filename = function() {
      paste("data_timeline", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      excel_frame <-datasetInput_time()
      # Create a workbook
      wb <- createWorkbook()
      
      # Add a worksheet
      addWorksheet(wb, "Data")
      
      # Write data to the worksheet
      writeData(wb, "Data", x = excel_frame, startCol = 1, startRow = 1, rowNames = FALSE)
      
      # create a new style for this thing
      custom_style <- createStyle(wrapText =TRUE)
      grey_rows <- createStyle(fgFill = 'lightgrey',wrapText =TRUE)
      
      
      addStyle(wb, sheet = "Data", custom_style, rows = 1:(nrow(excel_frame)+1), 
               cols = 1:ncol(excel_frame), gridExpand = TRUE)
      
      true_id_list <- unique(excel_frame$true_ID)
      true_id_even <- true_id_list[seq(1, length(true_id_list), 2)]
      rows_to_format <- which(excel_frame$true_ID %in% true_id_even)+1
      
      addStyle(wb, sheet = "Data", grey_rows, rows = rows_to_format, 
               cols = 1:ncol(excel_frame), gridExpand = TRUE)
      
      
      setColWidths(wb,'Data',1:ncol(excel_frame),widths =30)
      
      # Write merged data to the worksheet
      writeData(wb, "Data", excel_frame %>% select(-any_of(c('true_ID'))),
                startCol = 1, startRow = 1, rowNames = FALSE)
      
      # Save the workbook
      saveWorkbook(wb, file)
    }
  )
  
  
  
  
}
