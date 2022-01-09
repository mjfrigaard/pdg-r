# packages
library(tidyverse)
library(shiny)
library(shinydashboard)
library(reactable)
library(vroom)
library(haven)


# UI ----------------------------------------------------------------------

ui <- dashboardPage(
  
  dashboardHeader(title = tags$pre('shiny meta (test)')),
  
  dashboardSidebar(
    collapsed = FALSE, 
    br(), br(),
    textInput(inputId = "sas_path", label = "SAS folder"), 
    fileInput("sas_file", "Choose SAS File", accept = ".sas7bdat"),
    textInput(inputId = "sas_file_name", label = "Enter SAS data name")
  ),
  
  dashboardBody(
    # Boxes need to be put in a row (or column)
    br(),
    br(),
    tabBox(width = 12, title = code("sas folder"), 
           tabPanel(
             title = tags$p('sas folder', tags$code('tree')),
             fluidRow(column(12,
                shiny::verbatimTextOutput(outputId = "sas_dir_tree")
             )))
        ),
    tabBox(width = 12, title = code("file info"),

           tabPanel(
             title = tags$p('sas folder', tags$code('metadata')),
             fluidRow(column(12,
                reactable::reactableOutput(outputId = "sas_dir_info")
             ))),
           tabPanel(
             title = tags$p('sas file', tags$code('metadata')),
             fluidRow(column(12,
                    reactable::reactableOutput(outputId = "sas_metadata")
             ))),
           tabPanel(
             title = tags$p('sas file', tags$code('contents')),
             fluidRow(column(12,
                    reactable::reactableOutput(outputId = "sas_contents")
             )))
  ))
)


# SERVER ------------------------------------------------------------------

server <- function(input, output) {
  
  output$sas_dir_tree <- shiny::renderPrint({
    sas_dir <- input$sas_path
    # ext <- tools::file_ext(file$datapath)
    req(sas_dir)
    validate(need(!is.null(sas_dir), "Please provide the folder to the sas file"))
    # csv_import <- vroom::vroom(file = file$datapath, delim = ",")
    fs::dir_tree(path = sas_dir, recurse = TRUE)
  })
  
  output$sas_dir_info <- reactable::renderReactable({
    sas_dir <- input$sas_path
    # ext <- tools::file_ext(file$datapath)
    req(sas_dir)
    validate(need(!is.null(sas_dir), "Please provide the folder to the sas file"))
    # csv_import <- vroom::vroom(file = file$datapath, delim = ",")
    sas_dirinfo_data <- fs::dir_info(path = sas_dir, recurse = TRUE, all = TRUE) %>% 
      select(path, type, contains("time")) %>% 
      # mutate(across(where()))
      mutate(path = as.character(path)) %>% 
      rename(mod_time = modification_time)
    reactable(sas_dirinfo_data, defaultPageSize = 2)
  })
  
  output$sas_metadata <- reactable::renderReactable({
    file <- input$sas_file
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "sas7bdat", "Please upload a sas file"))
    sas_meta <- fs::file_info(path = file$datapath, follow = TRUE) %>% 
      select(path, contains("time")) %>% 
      mutate(path = as.character(path))
    reactable(sas_meta, defaultPageSize = 2)
  })
  
  sas_data <- reactive({
    file <- input$sas_file
    ext <- tools::file_ext(file$datapath)
    req(file)
    
    validate(need(ext == "sas7bdat", "Please upload a sas file"))
    sas_import <- haven::read_sas(data_file = file$datapath)
    return(sas_import)
  })
  
  output$sas_contents <- reactable::renderReactable({
    req(input$sas_file_name)
    sas_contents <- sas_data()
    reactable(sas_contents, defaultPageSize = 2)
  })
  
  output$input_values <- shiny::renderPrint({
      
      input_values <- reactiveValuesToList(x = input, all.names = FALSE)
      # input_value_names <- names(input_values)
      print(input_values[str_detect(string = names(input), pattern = "sas_file")])
      
  })
  
  
  
  
}

shinyApp(ui, server)