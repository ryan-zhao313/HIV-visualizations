# Load libraries
library("dplyr")
library("plotly")
library("ggplot2")

# Make function that returns the plot
art_treated_vs_sick <- function(dataframe) {
  dataframe <- hiv_df %>%
    select(
      Reported.number.of.people.receiving.ART,
      Estimated.number.of.people.living.with.HIV_median
    ) %>%
    filter(Reported.number.of.people.receiving.ART != "Nodata") %>%
    filter(!is.na(Estimated.number.of.people.living.with.HIV_median)) %>%
    mutate(
      living_with_HIV =
        as.numeric(Estimated.number.of.people.living.with.HIV_median),
      receiving_ART =
        as.numeric(Reported.number.of.people.receiving.ART)
    )
  treated_vs_sick_plot <- plot_ly(
    data = dataframe,
    x = ~living_with_HIV,
    y = ~receiving_ART,
    type = "scatter",
    mode = "markers"
  ) %>%
    layout(
      xaxis = list(title = "Estimated People Receiving Art"),
      yaxis = list(title = "Estimated People Living With HIV"),
      title =
        "Estimated People Receiving Art vs. Estimated People Living With HIV"
    )
  return(treated_vs_sick_plot)
}
