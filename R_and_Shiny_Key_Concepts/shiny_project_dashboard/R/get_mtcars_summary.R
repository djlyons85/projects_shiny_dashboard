library(dplyr)

get_mtcars_summary <- function(df){
  df |>
    mutate(cyl = paste0(cyl, "_cylander")) |>
    group_by(cyl) |>
    summarise(
      sample_size = n(),
      ave_mpg = round(mean(mpg), 1),
      ave_disp = round(mean(disp), 1),
      ave_wt = round(mean(wt), 1),
      ave_qsec = round(mean(qsec), 1)
    )
  
}



