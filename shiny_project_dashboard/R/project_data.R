

###Project information is located in an app R file for demonstration purposes only. 
###This should be located in a database with some users having writing privileges
###so information can be dynamically updated through the app.

penguin_proj_info <- list(
  "project_id_name" = "penguin_proj",
  "title" = "Palmer Penguin Example", 
  "subtitle" = "My Favorite Example",
  "figure_ids" = "figure_penguin",
  "figure_type" = "flextable",
  "fig_labels" = "Penguin Size Measures by Species, Island, and Sex",
  "frequency" = "monthly",
  "last_completed" = lubridate::ydm(20262301),
  "short_description" = "Summary of penguin measurements by species, island, and sex.",
  "long_description" = 
    paste0("Penguine measurements are collected on various islands. The species",
           " sex, culmen length, culmen depth, flipper length, and body mass are ",
           "recorded in the dataset. I had no idea what a culmen is and that ",
           "is a great example of what would be included in the long descrition. ",
           "A culmen is the upper ridge of a bird's bill or beak. The simple example ",
           "groups by species, island, and sex, then summarises the mean of the ",
           "respective data points. My issue with this data is where are the pictures?!"),
  "fig_notes" = "Source: Base R example data",
  "title_size" = 24,
  "subtitle_size" = 16,
  "fig_pre_paragraph" = "Some analysis before.",
  "fig_post_paragraph" = "Some analysis after.",
  "fig_label_size" = 14,
  "fig_paragraph_text_size" = 12,
  "post_linebreak_size" = 20,
  "end_with_pagebreak" = FALSE,
  "flextable_highlighted_cols" = 2,
  "flextable_has_row" = "No",
  "flextable_hierarchical_header" = "No"
)

mtcars_proj_info <- list(
  "project_id_name" = "mtcars_proj",
  "title" = "MT (Empty) Cars Example", 
  "subtitle" = "My Least Favorite Example",
  "figure_ids" = "figure_mtcars",
  "figure_type" = "gg",
  "fig_labels" = "Engine and Performance by # of Cylinders",
  "frequency" = "annually",
  "last_completed" = lubridate::ydm(20251601),
  "short_description" = "Summary of engine and performance measures by number of cylinders.",
  "long_description" = 
    paste0(
      "Car engine and performance measurements are recorded. I am not a 'car ",
      "guy' and had limited understanding of what this data represents. According",
      "to the dataset description it was extracted from the 1974 Motor Trend US",
      " magazine and covers 32 automobiles from 1973-74. The summary data includes",
      " miles per gallon, diplacement, weight, and quarter mile time."),
  "fig_notes" = "Source: Base R example data",
  "title_size" = 24,
  "subtitle_size" = 16,
  "fig_pre_paragraph" = "Some analysis before.",
  "fig_post_paragraph" = "Some analysis after.",
  "fig_label_size" = 14,
  "fig_paragraph_text_size" = 12,
  "post_linebreak_size" = 20,
  "end_with_pagebreak" = FALSE,
  "flextable_highlighted_cols" = 2,
  "flextable_has_row" = "No",
  "flextable_hierarchical_header" = "No"
)


starwars_fig_1_info <- list(
  "project_id_name" = "starwars_proj",
  "title" = "Star Wars Multiple Outputs Example", 
  "subtitle" = "Two Flextables",
  "figure_ids" = "figure_1_starwars",
  "figure_type" = "flextable",
  "fig_labels" = "Character Home World Counts",
  "frequency" = "annually",
  "last_completed" = lubridate::ydm(20250506),
  "short_description" = "Summary of Starwars character home worlds and species.",
  "long_description" = 
    paste0(
      "Star Wars character information taken from the SWAPI, the Star Wars",
      " API available at https://swapi.py4e.com/ This project provides ",
      "summarized character counts of character home worlds and species. ",
      "dropping those with low single digit counts."),
  "fig_notes" = "Source: Tidyverse example data",
  "title_size" = 24,
  "subtitle_size" = 16,
  "fig_pre_paragraph" = "Some analysis before.",
  "fig_post_paragraph" = "Some analysis after.",
  "fig_label_size" = 14,
  "fig_paragraph_text_size" = 12,
  "post_linebreak_size" = 20,
  "end_with_pagebreak" = FALSE,
  "flextable_highlighted_cols" = 2,
  "flextable_has_row" = "No",
  "flextable_hierarchical_header" = "No"
)

starwars_fig_2_info <- list(
  "project_id_name" = "starwars_proj",
  "title" = "Star Wars Multiple Outputs Example", 
  "subtitle" = "Two Flextables",
  "figure_ids" = "figure_2_starwars",
  "figure_type" = "flextable",
  "fig_labels" = "Character Species Counts",
  "frequency" = "annually",
  "last_completed" = lubridate::ydm(20251601),
  "short_description" = "Summary of Starwars character home worlds and species.",
  "long_description" = 
    paste0(
      "Star Wars character information taken from the SWAPI, the Star Wars",
      " API available at https://swapi.py4e.com/ This project provides ",
      "summarized character counts of character home worlds and species. ",
      "dropping those with low single digit counts."),
  "fig_notes" = "Source: Tidyverse example data",
  "title_size" = 24,
  "subtitle_size" = 16,
  "fig_pre_paragraph" = "Some analysis before.",
  "fig_post_paragraph" = "Some analysis after.",
  "fig_label_size" = 14,
  "fig_paragraph_text_size" = 12,
  "post_linebreak_size" = 20,
  "end_with_pagebreak" = FALSE,
  "flextable_highlighted_cols" = 2,
  "flextable_has_row" = "No",
  "flextable_hierarchical_header" = "No"
)



