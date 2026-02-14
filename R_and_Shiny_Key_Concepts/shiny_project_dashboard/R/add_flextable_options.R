
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(purrr)
library(flextable)
library(rlang)
library(glue)
library(tidyr)



add_flextable_optionsUI <- function(id, 
                                    flextable_info) {
  ns <- NS(id)
  div(
    numericInput(ns("left_highlight_cols"), "How Many ID Columns are there?", 
                 value = flextable_info$flextable_highlighted_cols, min = 0),
    radioButtons(ns("total_rows"), "Format Last Row as Totals", choices = c("Yes", "No"), selected = flextable_info$flextable_has_row),
    radioButtons(ns("hierarchical_headers"), "Headers Are Hierarchical? Use '_' to Distinguish Levels", 
                 choices = c("Yes", "No"), selected = flextable_info$flextable_hierarchical_header),
  )
  
}

add_flextable_optionsServer <- function(id, table_data, notes) {
  moduleServer(
    id,
    function(input, output, session) {

      table_properties <- reactiveValues()
      
      observe({
        if(isTruthy(notes) & !is_empty(notes)){
          table_properties$add_notes <- notes
        }
      })
      
      observe({
        if(isTruthy(input$left_highlight_cols)) {
          table_properties$highlight_n_cols <- input$left_highlight_cols
        } else {table_properties$highlight_n_cols <- 0}
      })
      
      observe({
        if(isTruthy(input$total_rows) &
           !is_empty(input$total_rows)){
          if(input$total_rows == "Yes"){
            table_properties$format_totals_row <- TRUE
          } else table_properties$format_totals_row <- FALSE
        }
      })
      
      observe({
        if(isTruthy(input$hierarchical_headers) & 
           !is_empty(input$hierarchical_headers)){
          if(input$hierarchical_headers == "Yes"){
            table_properties$hierarchal_header_add_row <- TRUE
          } else {
            table_properties$hierarchal_header_add_row <- NULL
          }
        }
      })
      
      
      output_flextable <- reactive({
        output_table_params <- c(reactiveValuesToList(table_properties),
                                 list("x" = table_data))
        
        rlang::exec(create_standard_flex_tab, !!!output_table_params)
      })
      
      return(output_flextable)
      
    }
  )
}

