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



create_doc_template <- function(document_title, 
                                document_sub_titles = NULL,
                                doc_title_size = 24,
                                doc_sub_title_size = 16,
                                logo_path = "www\\sctcs_header_logo.jpg"){
  create_map_block <- function(text, text_font_style = NA, text_para_style = NA){
    
    text_style <- rlang::exec(fp_text, !!!text_font_style)
    paragraph_style <- rlang::exec(fp_par, !!!text_para_style)
    
    if(class(paragraph_style) == "list"){
      if(any(unlist(map(paragraph_style, \(x) length(x) > 1), 
                    use.names = FALSE))) paragraph_style <- unlist(paragraph_style)
      }
    
    fpar_list <- map(text, fpar, fp_t = text_style, fp_p = paragraph_style)

    rlang::exec(block_list, !!!fpar_list)
    
    
  }

  img_block <- block_list(
    fpar(fp_p = fp_par(text.align = "center"),
         external_img(
           logo_path, width = 3.67, height = .88)))
  
  x <- read_docx() |>
    body_set_default_section(prop_section(
      header_default = img_block,
      page_margins = page_mar(bottom = 1, top = 1.75, right = .75, left = .75))) |>
    body_add_blocks(block_list(
      fpar(
        ftext(document_title, fp_text(font.size = doc_title_size, bold = TRUE)),
        fp_p = fp_par(line_spacing = doc_title_size/19, text.align = "center")
      )))
  if(!is.null(document_sub_titles) & !is_empty(document_sub_titles)){
    font_style = list(font.size = doc_sub_title_size, bold = TRUE)
    para_style = list(padding.top = doc_sub_title_size %/% 5, text.align = "center")
    x <- x |> body_add_blocks(create_map_block(document_sub_titles,
                                                   text_font_style = font_style,
                                                   text_para_style = para_style))
  }
  
  x <- x |> body_add_blocks(block_list(
    fpar(run_linebreak())))
  
  return(x)
}

