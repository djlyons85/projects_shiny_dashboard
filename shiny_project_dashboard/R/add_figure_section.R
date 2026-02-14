library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(purrr)
library(flextable)
library(rlang)
library(glue)
library(tidyr)
library(ggplot2)
library(stringr)
library(officer)


add_figure_sectionUI <- function(id, 
                                 figure_list) {
  ns <- NS(id)
  
  
  tagList(
    #sendValueToServer(ns("param_id"), figure_type),
    if(figure_list$figure_type == "flextable") {fluidRow(uiOutput(ns("flextable_display")))},
    if(figure_list$figure_type == "gg") {fluidRow(plotOutput(ns("gg_display")))}, 
    fluidRow(
      box(
        title = "Figure Labeling and Text",
        status = "primary",
        solidHeader = TRUE,
        textInput(ns("fig_label"),
                  "Enter Figure Label:",
                  value = figure_list$fig_labels),
        textAreaInput(ns("fig_pre_paragraph"),
                  "Enter Paragraph Before Figure:",
                  value = figure_list$fig_pre_paragraph),
        textAreaInput(ns("fig_post_paragraph"),
                     "Enter Paragraph After Figure:",
                     value = figure_list$fig_post_paragraph),
        textInput(ns("fig_notes"),
                  "Enter Figure Notes:",
                  value = figure_list$fig_notes)
      ),
      box(title = "Figure Formatting",
          numericInput(ns("figure_label_size"), "Enter Figure Label Size", value = figure_list$fig_label_size, min = 1),
          numericInput(ns("figure_text_size"), "Enter Paragraph Text Size", value = figure_list$fig_paragraph_text_size, min = 1),
          if(figure_list$figure_type == "flextable") {add_flextable_optionsUI(ns("flextable"), 
                                                                  flextable_info = figure_list)},
          numericInput(ns("line_break_size"), 
                       "When no Page Break, Then Size of Ending Linebreak (Most Relevant to Multiple Figure Document)", 
                       min = 1, value = figure_list$post_linebreak_size),
          checkboxInput(ns("page_break_check"), "Check to Add Page Break after This Section", 
                        value = figure_list$end_with_pagebreak),
          collapsible = TRUE, collapsed = TRUE)
    )
  )
}



add_figure_sectionServer <- function(id, 
                                     .data, 
                                     fig_output_id,
                                     fig_type, 
                                     ggplot_func = function(x, y){NULL}) {
  moduleServer(
    id,
    function(input, output, session) {
      
      
      ready_figure <- reactiveValues()
      observe({
        if(fig_type == "flextable"){
          ready_figure$figure <- add_flextable_optionsServer(id = "flextable", table_data = .data$summary(), notes = input$fig_notes)}
        if(fig_type == "gg"){
          ready_figure$figure <- ggplot_func(.data$summary(), input$fig_notes)
          }
      })

  
      ### Adding Fig Word Params
      word_properties <- reactiveValues()
      
      observe({
        if(isTruthy(input$fig_pre_paragraph) &
           !is_empty(input$fig_pre_paragraph)){
          word_properties$fig_preamble <- input$fig_pre_paragraph
          }
        
      })
      
      observe({
        if(isTruthy(input$fig_post_paragraph) &
           !is_empty(input$fig_post_paragraph)){
          word_properties$fig_postscript <- input$fig_post_paragraph
        }
      })
      
      observe({
        if(isTruthy(input$figure_text_size)){
          word_properties$fig_preamble_font_size <- input$figure_text_size
          word_properties$fig_postscript_font_size <- input$figure_text_size
        }
      })
      
      
      observe({
        if(isTruthy(input$fig_label) & !is_empty(input$fig_label)){
          word_properties$figure_labels <- input$fig_label
        }
      })
      
      observe({
        if(isTruthy(input$figure_label_size)){
          word_properties$figure_label_size <- input$figure_label_size
        }
      })
      
      observe({
        if(isTruthy(input$line_break_size)){
          if(!is_empty(input$page_break_check)){
            if(!input$page_break_check){
              word_properties$line_space_after_size <- input$line_break_size
              }
          }
        }
      })

      output$gg_display <- renderPlot({
        ready_figure$figure
      })
    
      output$flextable_display <- renderUI({
        ready_figure$figure() |>
            htmltools_value()
      })
      
      observe({
      .data[[fig_output_id]] <- reactive({
        if(fig_type == "flextable"){
          output_list <- c(reactiveValuesToList(word_properties),
                        list("object" = ready_figure$figure()))
          }
        if(fig_type == "gg"){
          output_list <- c(reactiveValuesToList(word_properties),
                        list("object" = ready_figure$figure))
        }
        return(output_list)
      })
      })
      
    }
  )
}


