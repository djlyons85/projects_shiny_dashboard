library(dplyr)
library(tidyr)

get_starwars_summary <- function(df, homeworld_cutoff = 6, species_cutoff = 4) {

  starwars_df <- df |>
    mutate(homeworld = replace_na(homeworld, "Homeworld_Unknown"),
           species = replace_na(species, "Species_Unknown")) |>
    add_count(films, homeworld, name = "Homeworld") |>
    add_count(films, species, name = "Species") |>
    distinct(films, species, homeworld, Homeworld, Species)
  
  homeworld_df <- starwars_df |>
    group_by(homeworld) |>
    filter(sum(Homeworld) >= homeworld_cutoff) |>
    pivot_wider(id_cols = films,
                names_from = homeworld, 
                values_from = Homeworld,
                values_fn = first) |>
    mutate(across(where(is.numeric), function(x) replace_na(x, 0))) |>
    janitor::adorn_totals("row")
  
  species_df <- starwars_df |>
    group_by(species) |>
    filter(sum(Species) >= species_cutoff) |>
    pivot_wider(id_cols = films,
                names_from = species, 
                values_from = Species,
                values_fn = first) |>
    mutate(across(where(is.numeric), function(x) replace_na(x, 0))) |>
    janitor::adorn_totals("row")
  return(list("movie_homeworld_count" = homeworld_df,
              "movie_species_count" = species_df))
}


