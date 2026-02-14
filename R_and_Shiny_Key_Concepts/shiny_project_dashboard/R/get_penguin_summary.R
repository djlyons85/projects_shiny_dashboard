

library(palmerpenguins)
library(dplyr)


get_penguin_summary <- function(penguin_df){
  penguin_df |>
    na.omit(Sex) |>
    group_by(Species, Island, Sex) |>
    summarise(
      Sample_Size = n(),
      Culmen_Length = round(mean(`Culmen Length (mm)`, na.rm = TRUE), 1),
      Culmen_Depth = round(mean(`Culmen Depth (mm)`, na.rm = TRUE), 1),
      Flipper_Length = round(mean(`Flipper Length (mm)`, na.rm = TRUE), 1),
      Body_Mass = round(mean(`Body Mass (g)`, na.rm = TRUE), 1),
      .groups = 'keep'
    )
  
}
