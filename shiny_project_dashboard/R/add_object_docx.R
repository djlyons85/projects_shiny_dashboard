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


add_object_docx <- function(doc, 
                               object, 
                               figure_labels = NULL,
                               figure_label_size = 16, 
                               fig_preamble = NULL,
                               fig_preamble_font_size = 12, 
                               #fig_preamble_para_style = list(NA), #using pf_p in fp_par always ends up displaying the contents of the paragraph formatting object
                               fig_postscript = NULL,
                               fig_postscript_font_size = 12, 
                               #fig_postscript_para_style = list(NA), #Following above note, updates are required to allow full paragraph formatting control for pre and post text.
                               page_break_after = TRUE,
                               line_space_after_size = NA
){
  #' @title add_object_docx
  #' 
  #' @description Add a flextable or ggplot figure to an officer package docx object
  #' 
  #' @details Allows for object label, and paragraph text chunks before or after object (preamble, postscript)
  #' @details Object notes should be added via the flextable or ggplot. See function create_standard_flex_tab()
  #' 
  #' 
  #' @param doc docx type from officer package 
  #' @param object dataframe/tibble, flextable, or ggplot to add to doc. Dataframe/tibbles will be automatically converted to a flextable
  #' @param figure_labels Bolded Text immediately preceding object, string or character vector
  #' @param figure_label_size numeric sets size and corresponding line spacing of label
  #' @param fig_preamble list, vector, convertable to character Adds text before figure. Lines break between list or vector items. 
  #' @param fig_preamble_font_style Sets font style of preamble, Named list, see ?officer::fp_text for valid items and values
  #' @param fig_preamble_para_style Sets paragraph formatting of preamble, Named list, see officer::fp_par for valid items and values
  #' @param fig_postscript = list, vector, convertable to character Adds text after figure. Lines break between list or vector items. 
  #' @param fig_postscript_font_style Sets font style of postscript, Named list, see ?officer::fp_text for valid items and values
  #' @param fig_postscript_para_style Sets paragraph formatting of postscript, Named list, see officer::fp_par for valid items and values
  #' @param page_break_after TRUE/FALSE Ignored if line_space_after_size is not NA. Choose to put a page break after object/postscript if applicable
  #' @param line_space_after_size Numeric, if not NA will add a blank line after object/postscript. Size corresponds to font sizing.
  #' 
  #' @return A docx type from officer package with the object and accompanying text sections added
  #' 
  
  create_standard_flex_tab <- function(x,
                                       highlight_n_cols = 1, 
                                       format_totals_row = TRUE,
                                       hierarchal_header_add_row = NULL,
                                       add_notes = NULL,
                                       add_cited_footnotes = NULL,
                                       rename_cols = NULL,
                                       dimension_func = fit_to_width,
                                       dimension_args = list("max_width" = 8.5)) {
    
    
    big_border = fp_border(color= "black", width = 2)
    small_border = fp_border(color="gray", width = 1)
    ft <- x |>
      flextable() |>
      bg(j = (highlight_n_cols + 1):ncol_keys(.), part = "body", bg = "#DFDCCE") |>
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
        bg(i = nrow_part(.), bg = "#00529B") |>
        bold(i = nrow_part(.)) |>
        color(i = nrow_part(.), color = "white")
    }
    
    ft <- ft |>
      border_outer(part="all", border = big_border)
    
    if(!is.null(add_cited_footnotes)){
      footnote_default <- purrr::partial(footnote, inline = TRUE)
      ft <- rlang::exec(footnote_default, x = ft, !!!add_header_footnote) 
    }
    
    ft <- rlang::exec(dimension_func, x = ft, !!!dimension_args)
    
    return(ft)
  }
  
  

  
  if(!is.null(fig_preamble)){
    doc <- doc |>
      body_add_blocks(block_list(
        fpar(
          ftext(fig_preamble, fp_text(font.size = fig_preamble_font_size)),
          fp_p = fp_par(line_spacing = fig_preamble_font_size/16)
        )))      
    
  }
  
  if(!is.null(figure_labels)){
    doc <- doc |>
      body_add_blocks(block_list(
        fpar(
          run_linebreak(),
          ftext(figure_labels, fp_text(font.size = figure_label_size, bold = TRUE)),
          fp_p = fp_par(line_spacing = figure_label_size/16)
        )))
  }
  
  if("tbl" %in% class(object) | "data.frame" %in% class(object)){
    object <- create_standard_flex_tab(object)
  }
  
  if("flextable" %in% class(object)){
    doc <- doc |>
      body_add_flextable(object)
  }
  
  if("gg" %in% class(object)){
    doc <- doc |>
      body_add_gg(object)
  }
  

  
  if(!is.null(fig_postscript) & !is_empty(fig_postscript)){
    doc <- doc |>
      body_add_blocks(block_list(
        fpar(
          run_linebreak(),
          ftext(fig_postscript, fp_text(font.size = fig_postscript_font_size)),
          fp_p = fp_par(line_spacing = fig_postscript_font_size/16)
        )))     
    
  }

  if(!is.na(line_space_after_size)){
    doc <- doc |>
      body_add_blocks(block_list(
        fpar(
          ftext(" ", fp_text(font.size = line_space_after_size)),
          run_linebreak()
        ))) 
      
  }
  
  if(page_break_after & is.na(line_space_after_size)){
    doc <- doc |>
      body_add_break()
  }
  
  return(doc)
  
  
}

