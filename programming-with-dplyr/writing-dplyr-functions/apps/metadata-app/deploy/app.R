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
      br(),
    textInput(inputId = "csv_path", label = "CSV folder"),
    fileInput(inputId = "csv_file", label = "Import CSV File", 
        accept = c(".csv", ".txt")),
    br()
  ),
  
  dashboardBody(
    # Boxes need to be put in a row (or column)
    br(),
    br(),
    tabBox(width = 6, height = 300, title = code("SAS"),
           tabPanel(
             title = tags$p('sas folder', tags$code('tree')),
             fluidRow(column(12,
                             shiny::verbatimTextOutput(outputId = "sas_dir_tree")
             ))),
           tabPanel(
             title = tags$p('sas folder', tags$code('metadata')),
             fluidRow(column(12,
                             reactable::reactableOutput(outputId = "sas_dir_info")
             )))),
      tabBox(width = 6, height = 300, title = code("SAS"),
           tabPanel(
             title = tags$p('sas file', tags$code('metadata')),
             fluidRow(column(12,
                             reactable::reactableOutput(outputId = "sas_metadata")
             ))),
           tabPanel(
             title = tags$p('sas file', tags$code('contents')),
             fluidRow(column(12,
                             reactable::reactableOutput(outputId = "sas_contents")
             )))),
    tabBox(width = 6, height = 300, title = tags$p(tags$code('CSV')),
            tabPanel(title = tags$p('csv path', tags$code('tree')),
                    fluidRow(column(12,
                             shiny::verbatimTextOutput(outputId = "csv_dir_tree")
                    ))),
           tabPanel(title = p("csv path", code("metadata")),
                    fluidRow(column(12,
                                    reactable::reactableOutput(outputId = "csv_dir_info")
                    )))),
      tabBox(width = 6, height = 300, title = tags$p(tags$code('CSV')),
           tabPanel(title = p("csv file", code("metadata")),
                    fluidRow(column(12,
                                    reactable::reactableOutput(outputId = "csv_metadata")
                    ))),
           tabPanel(title = p("csv file", code("contents")),
                    fluidRow(column(12,
                                    reactable::reactableOutput(outputId = "csv_contents")
                    )))
    )
  )
)


# SERVER ------------------------------------------------------------------

server <- function(input, output) {
    
    output$csv_dir_tree <- shiny::renderPrint({
        csv_dir <- input$csv_path
        # ext <- tools::file_ext(file$datapath)
        req(csv_dir)
        validate(need(!is.null(csv_dir), "Please provide the folder to the csv file"))
        # csv_import <- vroom::vroom(file = file$datapath, delim = ",")
        fs::dir_tree(path = csv_dir, recurse = TRUE)
      })
    
  output$csv_dir_info <- reactable::renderReactable({
    csv_dir <- input$csv_path
    # ext <- tools::file_ext(file$datapath)
    req(csv_dir)
    validate(need(!is.null(csv_dir), "Please provide the folder to the csv file"))
    # csv_import <- vroom::vroom(file = file$datapath, delim = ",")
    csv_dirinfo_data <- fs::dir_info(path = csv_dir, recurse = TRUE, all = TRUE) %>% 
        select(path, type, contains("time")) %>% 
        mutate(path = as.character(path))
    reactable(csv_dirinfo_data, defaultPageSize = 5)
  })
  
    output$csv_metadata <- reactable::renderReactable({
    file <- input$csv_file
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    csv_meta <- fs::file_info(path = file$datapath, follow = TRUE) %>% 
        select(path, contains("time")) %>% 
        mutate(path = as.character(path))
    reactable(csv_meta, defaultPageSize = 5)
  })

  output$csv_contents <- reactable::renderReactable({
    file <- input$csv_file
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    csv_import <- vroom::vroom(file = file$datapath, delim = ",")
    reactable(csv_import, defaultPageSize = 5)
  })
  

    
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
            mutate(path = as.character(path))
        reactable(sas_dirinfo_data, defaultPageSize = 5)
      })
  
    output$sas_metadata <- reactable::renderReactable({
        file <- input$sas_file
        ext <- tools::file_ext(file$datapath)
        req(file)
        validate(need(ext == "sas7bdat", "Please upload a sas file"))
        sas_meta <- fs::file_info(path = file$datapath, follow = TRUE) %>% 
            select(path, contains("time")) %>% 
            mutate(path = as.character(path))
        reactable(sas_meta, defaultPageSize = 5)
      })
      
    output$sas_contents <- reactable::renderReactable({
        file <- input$sas_file
        ext <- tools::file_ext(file$datapath)
        req(file)
        
        validate(need(ext == "sas7bdat", "Please upload a sas file"))
        sas_import <- haven::read_sas(data_file = file$datapath)
        
        reactable(sas_import, defaultPageSize = 5)
      })
    
    

  
}

shinyApp(ui, server)
