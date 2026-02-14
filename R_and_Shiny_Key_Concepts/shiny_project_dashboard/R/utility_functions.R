add_object_iterator <- function(doc, params_list){
  params <- c(list("doc" = doc), params_list) 
  
  rlang::exec(add_object_docx, !!!params)
}