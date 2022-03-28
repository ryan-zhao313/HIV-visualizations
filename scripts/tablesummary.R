# Loading Libraries
library("dplyr")
library("plotly")
library("ggplot2")
library("leaflet")
library("tidyverse")

summary_table <- function(dataset) {
  dataset %>%
    drop_na() %>%
    select(
      Country, Estimated.number.of.people.living.with.HIV_median,
      Reported.number.of.people.receiving.ART, WHO.Region
    ) %>%
    filter(Reported.number.of.people.receiving.ART != "Nodata") %>%
    mutate(Reported_receiving_art = strtoi(
      Reported.number.of.people.receiving.ART
    )) %>%
    group_by(WHO.Region) %>%
    summarize(
      total_cases = sum(
        Estimated.number.of.people.living.with.HIV_median,
        na.rm = TRUE
      ),
      total_coverage = sum(Reported_receiving_art, na.rm = TRUE),
      coverage_prop = sum(Reported_receiving_art, na.rm = TRUE) /
        sum(Estimated.number.of.people.living.with.HIV_median,
          na.rm = TRUE
        )
    ) %>%
    arrange(-coverage_prop) %>% 
    rename("WHO Region" = WHO.Region, "Total Cases" = total_cases, 
          "Total ART Coverage" = total_coverage, 
          "ART Coverage Proportion" = coverage_prop)
}
