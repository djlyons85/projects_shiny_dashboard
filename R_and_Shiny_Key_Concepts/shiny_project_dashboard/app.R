#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/

library(shiny)
library(shinydashboard)
library(dplyr)
library(purrr)
library(readr)
library(officer)
library(flextable)
library(docstring)
library(rlang)
library(glue)
library(tidyr)
library(lubridate)
library(janitor)
library(tidyselect)
library(datamods)
library(stringr)
library(shinyWidgets)
library(DT)
library(shinyalert)
library(ggplot2)
library(shiny)
library(thematic)
library(palmerpenguins)
library(writexl)

### NOTE ON SHINYLIVE
# Easy to implement, run one line of code in the console:
#export(app_directory, output_directory)
###Then run a static server pointing at that output directory


#Fix for Shinylive download excel bug
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "IR Projects"),
  dashboardSidebar(
    sidebarMenu(id = "project_tabs", 
                menuItem("IR Projects Table", tabName = "proj_table"),
                menuItem("IR Project List", tabName = "menu_id",
                         menuSubItem("Penguin Project", tabName = "penguin_proj"),
                         menuSubItem("MT Cars Project", tabName = "mtcars_proj"),
                         menuSubItem("Star Wars Project", tabName = "starwars_proj")
                )
    )
    
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "proj_table",
              h2("Project Menu"),
              fluidRow(
                box(
                  DTOutput("projects_table"),
                  width = 12),
              )
      ),
      tabItem(tabName = "penguin_proj",
              h2("Penguin Project"),
              fluidRow(box(title = glue::glue("Choose {penguin_proj_info$title} Filters"),
                           status = "primary",
                           filter_data_ui(id = "penguin_filter"), 
                           solidHeader = TRUE,
                           width = 10, collapsible = TRUE)),
              add_reportUI("penguin_report", 
                           proj_list = penguin_proj_info),
              add_figure_sectionUI(id = penguin_proj_info$figure_ids, 
                                   figure_list = penguin_proj_info)
      ),
      tabItem(tabName = "mtcars_proj",
              h2("MT Cars Project"),
              fluidRow(box(title = glue::glue("Choose {mtcars_proj_info$title} Filters"),
                           status = "primary",
                           filter_data_ui(id = "mtcars_filter"), 
                           solidHeader = TRUE,
                           width = 10, collapsible = TRUE)),
              add_reportUI("mtcars_report", 
                           proj_list = mtcars_proj_info),
              add_figure_sectionUI(id = mtcars_proj_info$figure_ids, 
                                   figure_list = mtcars_proj_info)
      ),
      tabItem(tabName = "starwars_proj",
              h2("Star Wars Project"),
              fluidRow(box(title = glue::glue("Choose {starwars_fig_1_info$title} Filters"),
                           status = "primary",
                           filter_data_ui(id = "starwars_filter"), 
                           solidHeader = TRUE,
                           width = 10, collapsible = TRUE)),
              add_reportUI("starwars_report", 
                           proj_list = starwars_fig_1_info),
              add_figure_sectionUI(id = starwars_fig_1_info$figure_ids, 
                                   figure_list = starwars_fig_1_info),
              add_figure_sectionUI(id = starwars_fig_2_info$figure_ids, 
                                   figure_list = starwars_fig_2_info)
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  ## Palmer Penguin Example
  #Most simple example, 1 flextable
  
  #Connect to data
  penguin_df <- reactive({
    palmerpenguins::penguins_raw
  })
  
  #Define Filtering Columns
  penguin_filter_on <- reactive({
    c("Species", "Island", "Date Egg", "Sex")
  })
  
  #from datamods generates listed filters by data type
  #see function description for other selection widget types
  #Note that data type determines selection. It may
  #be helpful to add character versions of numeric columns just for filtering.
  #Requires filter_data_ui function in UI
  penguin_filtered <- datamods::filter_data_server(
    id = "penguin_filter", data = penguin_df,
    vars = penguin_filter_on,
    widget_char = "picker", widget_num = "range",
    widget_date = "range")
  
  #Apply filter
  penguin_filtered_data <- reactive({
    penguin_df() |>
      filter(!!!penguin_filtered$expr())
  })
  
  #Create project reactiveValues. Pass and save data to and from Modules
  penguin_data <- reactiveValues()
  #Apply project function
  penguin_data$summary <- reactive({
    penguin_filtered_data() |>
      get_penguin_summary()
  })
  
  #Add data for excel download
  penguin_data$excel_output <- reactive({
    penguin_filtered_data() |>
      get_penguin_summary()
  })
  
  
  #add report generating modules
  #add_reportServer provides the document setup
  add_reportServer("penguin_report", 
                   project_name = penguin_proj_info$title, 
                   .data = penguin_data)
  #add_figure_sectionServer adds figure customization
  add_figure_sectionServer(id = penguin_proj_info$figure_ids,
                           .data = penguin_data, 
                           fig_type = penguin_proj_info$figure_type,
                           fig_output_id = "penguin_fig_output")
  #add figure with formatting to downloaded report
  penguin_data$download_report <- reactive({
    add_object_iterator(doc = penguin_data$doc_template(), params_list = penguin_data$penguin_fig_output())
  })
  
  
  
  
  
  ############# MT CARS Example #################
  
  mtcars_df <- reactive({
    mtcars
  })
  
  mtcars_filter_on <- reactive({
    c("cyl", "gear", "wt", "carb")
  })
  
  #from datamods generates listed filters by data type
  mtcars_filtered <- datamods::filter_data_server(
    id = "mtcars_filter", data = mtcars_df,
    vars = mtcars_filter_on,
    widget_char = "picker", widget_num = "range",
    widget_date = "range")
  
  mtcars_filtered_data <- reactive({
    mtcars_df() |>
      filter(!!!mtcars_filtered$expr())
  })
  
  mtcars_data <- reactiveValues()
  mtcars_data$summary <- reactive({
    mtcars_filtered_data() |>
      get_mtcars_summary()
  })
  
  mtcars_data$excel_output <- reactive({
    mtcars_filtered_data() |>
      get_mtcars_summary()
  })
  
  add_reportServer("mtcars_report", 
                   project_name = mtcars_proj_info$title, 
                   .data = mtcars_data)
  add_figure_sectionServer(id = mtcars_proj_info$figure_ids,
                           .data = mtcars_data, 
                           fig_type = mtcars_proj_info$figure_type,
                           ggplot_func = convert_mtcars_ggplot,
                           fig_output_id = "mtcars_fig_1")
  
  mtcars_data$download_report <- reactive({
    add_object_iterator(mtcars_data$doc_template(), mtcars_data$mtcars_fig_1())
  })
  
  
  
  
  ############ Star Wars Example
  
  starwars_df <- reactive({
    starwars |>
      unnest_longer(col = films)
  })
  
  starwars_filter_on <- reactive({
    c("films")
  })
  
  starwars_filtered <- datamods::filter_data_server(
    id = "starwars_filter", 
    data = starwars_df,
    vars = starwars_filter_on,
    widget_char = "picker", widget_num = "range",
    widget_date = "range")
  
  starwars_filtered_df <- reactive({
    starwars_df() |>
      filter(!!!starwars_filtered$expr())
  })
  
  starwars_fig_1_data <- reactiveValues()
  starwars_fig_1_data$summary <- reactive({
    df <- starwars_filtered_df() |> 
      get_starwars_summary()
    df[[1]]
  })
  
  starwars_fig_1_data$excel_output <- reactive({
    df <- starwars_filtered_df() |> 
      get_starwars_summary()
  })
  
  starwars_fig_2_data <- reactiveValues()
  starwars_fig_2_data$summary <- reactive({
    df <- starwars_filtered_df() |> 
      get_starwars_summary()
    df[[2]]
  })
  
  add_reportServer(id = "starwars_report", 
                   project_name = starwars_fig_1_info$title, 
                   .data = starwars_fig_1_data)
  add_figure_sectionServer(id = starwars_fig_1_info$figure_ids,
                           .data = starwars_fig_1_data, 
                           fig_type = starwars_fig_1_info$figure_type,
                           fig_output_id = "starwars_fig_1")
  add_figure_sectionServer(id = starwars_fig_2_info$figure_ids,
                           .data = starwars_fig_2_data, 
                           fig_type = starwars_fig_2_info$figure_type,
                           fig_output_id = "starwars_fig_2")
  
  starwars_fig_1_data$download_report <- reactive({
    add_object_iterator(starwars_fig_1_data$doc_template(), starwars_fig_1_data$starwars_fig_1()) |>
      add_object_iterator(starwars_fig_2_data$starwars_fig_2())
  })
  
  
  
  
  
  
  ########## Landing Page ##########################
  
  project_data_list <- reactive({
    proj_data <- list(penguin_proj_info, mtcars_proj_info, starwars_fig_1_info,
                      starwars_fig_2_info) |>
      map(as_tibble) |>
      bind_rows() |>
      add_count(project_id_name, name = "Figure Count") |>
      distinct(project_id_name, .keep_all = TRUE) |>
      mutate(next_due = case_when(
        frequency == "annually" ~ last_completed + years(1),
        frequency == "monthly" ~ last_completed + months(1),
        TRUE ~ NA
      ),
      actionable = glue('<button id="custom_btn" onclick="Shiny.onInputChange(\'button_id\', \'{project_id_name}\')">Click</button>'),
      description = glue('<button id="another_btn" onclick="Shiny.onInputChange(\'description_popup\', \'{project_id_name}\')">Click</button>'),
      .before = frequency)
  })
  
  observeEvent(input$button_id, {
    updateTabItems(session, "project_tabs", selected = input$button_id)
  })
  
  observeEvent(input$description_popup, {
    message <- project_data_list() |> 
      distinct(project_id_name, .keep_all = TRUE) |> 
      filter(project_id_name == input$description_popup) |>
      pull(long_description)
    showModal(modalDialog(title = "Long Description", message))
  })
  
  output$projects_table <- renderDT({
    project_data_list() |>
      select(-c(subtitle, long_description, contains("fig", ignore.case = FALSE)), everything()) |>
      distinct() |>
      arrange(next_due) |>
      datatable(class = 'cell-border stripe', extensions = 'Responsive',
                escape = FALSE, selection = 'none',editable = TRUE)
  })
  
  
}

# Run the application 
thematic_shiny()
shinyApp(ui = ui, server = server)
