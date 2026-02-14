library(ggplot2)
library(dplyr)

convert_mtcars_ggplot <- function(.data, notes){
  .data |>
    ggplot(aes(x = cyl, y = ave_mpg)) +
    geom_bar(stat = "identity") +
    theme(plot.title    = element_text(family = "Times New Roman"),
          plot.subtitle = element_text(family = "Times New Roman"),
          axis.title.x  = element_text(family = "Times New Roman"),
          axis.title.y  = element_text(family = "Times New Roman"),
          axis.text.x   = element_text(family = "Times New Roman"),
          axis.text.y   = element_text(family = "Times New Roman"),
          plot.caption = element_text(family = "Times New Roman")) +
    labs(caption = notes)
  }