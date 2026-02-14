rm(list = ls())
library(tidyverse)

df <- iris |>
  group_by(Species) |>
  summarise(across(everything(), mean))




iris_proj_info <- list(
  "project_id_name" = "iris_proj",
  "title" = "Palmer Iris Example", 
  "subtitle" = "My Favorite Example",
  "figure_ids" = "figure_iris",
  "figure_type" = "flextable",
  "fig_labels" = "Iris Mean Flower Measurements",
  "frequency" = "monthly",
  "last_completed" = lubridate::ydm(20251306),
  "short_description" = "Summary of iris measurements by species, island, and sex.",
  "long_description" = 
    paste0("Mean measurements by species. This famous (Fisher's or Anderson's) iris data set gives the",
    "measurements in centimeters of the variables sepal length and width and petal",
    "length and width, respectively, for 50 flowers from each of 3 species of iris.",
    " The species are Iris setosa, versicolor, and virginica."),
  "title_size" = 24,
  "subtitle_size" = 16,
  "fig_pre_paragraph" = "Some flowery analysis before.",
  "fig_post_paragraph" = "Some flowery analysis after.",
  "fig_label_size" = 14,
  "fig_paragraph_text_size" = 12,
  "post_linebreak_size" = 20,
  "end_with_pagebreak" = FALSE,
  "flextable_highlighted_cols" = 2,
  "flextable_has_row" = "No",
  "flextable_hierarchical_header" = "No"
)

