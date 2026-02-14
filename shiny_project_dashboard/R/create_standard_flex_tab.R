library(officer)
library(flextable)
library(docstring)
library(tidyselect)
library(dplyr)
library(purrr)
library(rlang)
library(glue)
library(tidyr)
library(janitor)
library(tidyselect)



create_standard_flex_tab <- function(x,
                                     highlight_n_cols = 1, 
                                     format_totals_row = TRUE,
                                     hierarchal_header_add_row = NULL,
                                     add_notes = NULL,
                                     add_cited_footnotes = NULL,
                                     rename_cols = NULL,
                                     dimension_func = fit_to_width,
                                     dimension_args = list("max_width" = 8.5)) {
  
  #' @title create_standard_flex_tab
  #' 
  #' @description Convert a dataframe/tibble into a flextable
  #' 
  #' @details Function takes a dataframe/tibble and converts to a flextable with
  #' @details standardized branded coloring and styles. Allows for some custimization options.
  #' 
  #' 
  #' @param x A dataframe or tibble
  #' @param highlit_n_cols An integer indicating the number of rows from the 
  #' left size to color a darker branded brown. Allows zero for uniform column colors.
  #' @param format_totals_row A boolean determines if the last row should be 
  #' formatted the same as the header. Typically used for a Totals row.
  #' @param figure_label_size numeric sets size and corresponding line spacing of label
  #' @param hierarchal_header_add_row A non null value, FALSE, or list. If a non-null/non-FALSE
  #' then standard flextable::separate_header is applied with "_" used as the 
  #' separation operator. If a list then the list is passed as variables to 
  #' flextable::separate_header. For example, list("split" = "[\\\\.\\.]") 
  #' would make "." the separator.
  #' @param add_notes A character or character vector to be added as formatted notes. Vector items will introduce line breaks to the notes.
  #' @param add_cited_footnotes A named list passed as variables to flextable::footnote. The default value is adjusted to inline = TRUE.
  #' To add formatting the notes should be submitted as a paragraph. Here is the paragraph formatting for the add_notes parameter, where add_notes is the character vector:\n 
  #' value = as_paragraph(as_chunk(add_notes, props = fp_text_default(font.size = 8, italic = TRUE), as.character))\n The above would be a named list item 
  #' the user wants passed, along with other named list items, as parameters to flextable::footnote. 
  #' @param rename_cols Named list, see flextable::set_header_labels
  #' @param dimension_func A function that is executed to establish the width of the table. The default value is flextable::fit_to_width. Other functions for 
  #' flextable size management: autofit(), dim.flextable(), dim_pretty(), flextable_dim(), height(), hrule(), ncol_keys(), nrow_part(), width() 
  #' @param dimension_args A named list of arguements to be passed in the dimension_func
  #' 
  #' 
  #' @return A docx type from officer package with the object and accompanying text sections added
  #' 
  

  big_border = fp_border(color= "black", width = 2)
  small_border = fp_border(color="gray", width = 1)
  highlight_n_cols <- min(highlight_n_cols, ncol(x))
  ft <- x |>
    flextable() |>
    bg(j = (highlight_n_cols + 1):ncol(x), part = "body", bg = "#DFDCCE") |>
    bg(part = "header", bg = "#00529B") |>
    bold(part = "header") |>
    color(part = "header", color = "white") |>
    border_inner_h(part="all", border = small_border ) |>
    border_inner_v(part="all", border = small_border )
  
  ft <- ft |> autofit() |> fit_to_width(7.5)
  
  if(!is.null(rename_cols)){
    if(class(rename_cols) != "list") {
      rlang::abort(glue::glue(
        "rename_cols must be a list type, you submitted a type {class(rename_cols)}"))}
    ft <-  rlang::exec(set_header_labels, x = ft, !!!rename_cols)
  }
  if(!is.null(hierarchal_header_add_row)){
    if(class(hierarchal_header_add_row) != "list") {
      ft <- ft |>
        separate_header() |> 
        align(align = "center", part = "header") |>
        border_inner_h(part="header", border = small_border) |>
        border_inner_v(part="header", border = small_border)
    } else {
      ft <- rlang::exec(separate_header, x = ft, !!!hierarchal_header_add_row) |> 
        align(align = "center", part = "header") |>
        border_inner_h(part="header", border = small_border) |>
        border_inner_v(part="header", border = small_border)
    }
  }
  
  if(highlight_n_cols > 0) ft <- ft |> bg(j = 1:highlight_n_cols, 
                                           part = "body", bg = "#AFAB89")
  if(format_totals_row){
    ft <- ft |>
      bg(i = nrow(x), bg = "#00529B") |>
      bold(i = nrow(x)) |>
      color(i = nrow(x), color = "white")
  }
  
  ft <- ft |>
    border_outer(part="all", border = big_border)
  
  if(!is.null(add_cited_footnotes)){
    footnote_default <- purrr::partial(footnote, inline = TRUE)
    ft <- rlang::exec(footnote_default, x = ft, !!!add_header_footnote) 
  }
  
  if(!is.null(add_notes) & !is_empty(add_notes)){
    ft <- ft |> add_footer_lines(values = as_paragraph(
      as_chunk(add_notes, 
               props = fp_text_default(font.size = 8, italic = TRUE), as.character)), top = FALSE) |>
      padding(padding = 1, part = "footer")
  }
  
  ft <- rlang::exec(dimension_func, x = ft, !!!dimension_args)
  
  return(ft)
}
