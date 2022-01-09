# packages
library(tidyverse)
library(shiny)
library(shinydashboard)
library(reactable)
library(vroom)
library(haven)


# UI ----------------------------------------------------------------------

ui <- dashboardPage(
  
  dashboardHeader(title = 'shiny file names'),
  
  dashboardSidebar(
    collapsed = FALSE, 
    br(), br(),
    fileInput(inputId = "csv_file", label = p("Upload ", code("csv_file")), accept = ".csv"),
    textInput(inputId = "csv_app_name", label = p("Enter new ", code("csv_app_name")))
  ),
  
  dashboardBody(
    # Boxes need to be put in a row (or column)
    br(),
    br(),
    tabBox(width = 12, title = code("SHINY VALUES"), 
        tabPanel(
             title = tags$code('input$csv_file'),
             fluidRow(column(12,
                p(em("Import a .csv file and the ", 
                     code("input"), " values will be printed below:")),
                shiny::verbatimTextOutput(outputId = "input_values")
             ))),
           tabPanel(
             title = tags$code('input$csv_file$name'),
             fluidRow(column(12,
              p(em("The file ", 
                code("name"), " is from the imported file:")),
                shiny::verbatimTextOutput(outputId = "file_name")
             )))
        ),
      tabBox(width = 12, title = code("CSV FILE"),
        tabPanel(
             title = tags$code('input$csv_app_name'),
             fluidRow(column(12,
              p(em("Enter a new ", 
                code("csv_app_name"), 
                " and the file contents will appear on the next tab:")),
                shiny::verbatimTextOutput(outputId = "new_csv_file_name")
             ))),
           tabPanel(
             title = tags$p('csv file', tags$code('contents')),
             fluidRow(column(12,
              p(em("The contents contains both ", 
                code("orig_name"), " and ", code("app_name"), 
                " columns (for filtering):")),
                 reactable::reactableOutput(outputId = "csv_contents")
             )))
          )
      )
    )


# SERVER ------------------------------------------------------------------

server <- function(input, output) {
    
input_values_reactive <- reactive({
    req(input$csv_file)
    input_values <- reactiveValuesToList(x = input, all.names = FALSE)
    csv_input_values <- input_values[str_detect(string = names(input), pattern = "csv")]
    return(csv_input_values)
})
    
 output$input_values <- shiny::renderPrint({
      print(input_values_reactive()$csv_file)
  })
 
 output$new_csv_file_name <- shiny::renderPrint({
     req(input$csv_app_name)
     print(input_values_reactive()$csv_app_name)
  })
 
  output$file_name <- shiny::renderPrint({
      req(input$csv_file)
      input_values <- reactiveValuesToList(x = input, all.names = FALSE)
      # input_value_names <- names(input_values)
      print(input_values$csv_file$name)
      
  })
  
  csv_data <- reactive({
    file <- input$csv_file
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    csv_import <- vroom::vroom(file = file$datapath)
    return(csv_import)
  })
  
  new_csv_file <- reactive({
      req(input$csv_file)
      req(input$csv_app_name)
      # add orginal name column
      csv_orig_name <- add_column(.data = csv_data(), 
                                 orig_name = input$csv_file$name)
      # add app name column
      csv_app_name <- add_column(.data = csv_orig_name, 
                                 app_name = input$csv_app_name)
      
      new_csv <- select(csv_app_name, orig_name, app_name, everything())
      # return
      return(new_csv)
      
  })
  
  output$csv_contents <- reactable::renderReactable({
    req(input$csv_app_name)
    csv_contents <- new_csv_file()
    reactable(csv_contents, defaultPageSize = 4)
    
  })
  
  
  
  
}

shinyApp(ui, server)