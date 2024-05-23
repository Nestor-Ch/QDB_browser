server <- function(input, output) {
  
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
      }else{

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
    }
    
    
    if(input$table_type == 'Single comparison'|
       (input$table_type != 'Single comparison' & is.null(input$newName))){
      n_groups <- length(unique(df$true_ID))
      
      colors <- rep_len(c('#D3D3D3','white'),n_groups)
      
      names(colors) <- unique(df$true_ID)
      
      df <- df %>% 
        mutate(across(c(sector,q.type, TABLE_ID), ~ as.factor(.x)))
      
      datatable(df,
                filter = "top",
                extensions = 'Buttons',
                class = list(stripe = FALSE),
                options = list(
                  dom = 'lfrtipB',
                  buttons = c("copy", "csv", "pdf"),
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
    }else{
      
      relevant_cols <- which(grepl('sector',names(df)))
      relevant_cols <- relevant_cols[relevant_cols>8]
      testo <<- df
      datatable(
        df,
        filter = "top",
        extensions = 'Buttons',
        class = list(stripe = FALSE),
        options = list(
          dom = 'lfrtipB',
          buttons = c("copy", "excel", "pdf"),
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

    }
  })
}
