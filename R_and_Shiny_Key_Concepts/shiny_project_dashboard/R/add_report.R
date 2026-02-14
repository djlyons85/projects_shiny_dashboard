library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(officer)
library(shinyjs)

#Fix for Shinylive download bug
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}


add_reportUI <- function(id, 
                         proj_list) {
  ns <- NS(id)
  div(fluidRow(
    box(title = "Word Document Setup",
        status = "primary",
        solidHeader = TRUE,
        textInput(ns("report_title"),
                  "Enter Report Title:",
                  value = proj_list$title),
        textInput(ns("report_subtitle"),
                  "Enter Report Subtitle:",
                  value = proj_list$subtitle),
        actionButton(ns("generate_report"), "Create/Update Word Report"),
        downloadButton(ns("report_doc_download"), "Download Word Report"),
        downloadButton(ns("excel_download_mod"), "Excel Data Download")),
    box(title = "Format Document Titles",
        numericInput(ns("title_size"), "Enter Title Size", value = proj_list$title_size, min = 1),
        numericInput(ns("subtitle_size"), "Enter Subtitle Size", value = proj_list$subtitle_size, min = 1),
        collapsible = TRUE, collapsed = TRUE)
  )
  )

}

add_reportServer <- function(id, project_name, .data) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$report_doc_download <- downloadHandler(
        filename = function() {
          glue::glue("{project_name} {Sys.Date()}.docx")
        },
        content = function(file){
          Sys.sleep(.3)
          print(.data$download_report(), 
                target = file)
        },
        contentType = "docx"
      )
      output$excel_download_mod <- downloadHandler(
        filename = function(){
          glue::glue("{project_name} {Sys.Date()}.xlsx")
        },
        content = function(con){
          writexl::write_xlsx(.data$excel_output(), con)
        }
      )
      
      # Word Doc Template params
      doc_template_list <- reactiveValues()
      
      observe({
        if(isTruthy(input$report_title) &
           !is_empty(input$report_title)){
          doc_template_list$document_title <- input$report_title
        } else doc_template_list$document_title <- "Placeholder Title"
      })
      
      observe({
        if(isTruthy(input$report_subtitle) &
           !is_empty(input$report_subtitle)){
          doc_template_list$document_sub_titles <- input$report_subtitle
        }
      })
      
      observe({
        if(isTruthy(input$title_size)){
          doc_template_list$doc_title_size <- input$title_size
        } else doc_template_list$doc_title_size <- 24
      })
      
      observe({
        if(isTruthy(input$subtitle_size)){
          doc_template_list$doc_sub_title_size <- input$subtitle_size
        } else doc_template_list$doc_sub_title_size <- 16
      })
      
      .data$doc_template <- eventReactive(input$generate_report, {
        doc_template_params <- reactiveValuesToList(doc_template_list)   
        
        rlang::exec(create_doc_template, !!!doc_template_params)
      })
      

      
      

    }
  )
}
