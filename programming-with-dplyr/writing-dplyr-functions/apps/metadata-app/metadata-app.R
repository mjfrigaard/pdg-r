library(shinydashboard)
library(reactable)
library(vroom)
library(haven)
library(shiny)


ui <- dashboardPage(
    
  dashboardHeader(title = tags$pre('shiny meta (test)')),
  
  dashboardSidebar(
      collapsed = FALSE, 
        br(), br(),
    fileInput(inputId = "csv_file", 
              label = "Import CSV File", accept = c(".csv", ".txt")),
        br(), br(),
    fileInput("sas_file", "Choose SAS File", accept = ".sas7bdat"),
        br(),
        br()
  ),
  
  dashboardBody(
    # Boxes need to be put in a row (or column)
    br(),
    br(),
    tabBox(width = 12,
        title = tags$strong(tags$code('CSV')), 
        tabPanel(title = strong("file", code("contents")),
            fluidRow(column(12,
                reactable::reactableOutput(outputId = "csv_contents")
                    ))),
        tabPanel(title = strong("file", code("metadata")),
            fluidRow(column(12,
                reactable::reactableOutput(outputId = "csv_metadata")
                    )))
        ),
    tabBox(width = 12,
        title = tags$strong(tags$code('SAS')),
        tabPanel(
            title = tags$strong('file', tags$code('contents')),
        fluidRow(column(12,
          reactable::reactableOutput(outputId = "sas_contents")
          ))),
        tabPanel(
            title = tags$strong('file', tags$code('metadata')),
        fluidRow(column(12,
          reactable::reactableOutput(outputId = "sas_metadata")
          )))
        )
    )
)

server <- function(input, output) {

  output$csv_contents <- reactable::renderReactable({
    file <- input$csv_file
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    csv_import <- vroom::vroom(file = file$datapath, delim = ",")
    reactable(csv_import, defaultPageSize = 5)
  })
  
    output$csv_metadata <- reactable::renderReactable({
    file <- input$csv_file
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    csv_meta <- fs::file_info(path = file$datapath, follow = TRUE) %>% 
        select(path, contains("time"))
    reactable(csv_meta, defaultPageSize = 5)
  })
  
  
  
    output$sas_contents <- reactable::renderReactable({
    file <- input$sas_file
    ext <- tools::file_ext(file$datapath)
    req(file)
    
    validate(need(ext == "sas7bdat", "Please upload a sas file"))
    sas_import <- haven::read_sas(data_file = file$datapath)
    
    reactable(sas_import, defaultPageSize = 5)
  })
    
    
    output$sas_metadata <- reactable::renderReactable({
    file <- input$sas_file
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "sas7bdat", "Please upload a sas file"))
    sas_meta <- fs::file_info(path = file$datapath, follow = TRUE) %>% 
        select(path, contains("time"))
    reactable(sas_meta, defaultPageSize = 5)
  })
  
}

shinyApp(ui, server)
