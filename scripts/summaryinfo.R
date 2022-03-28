library("dplyr")
library("tidyverse")

get_summary_info <- function(dataset) {
  result <- list()
  # number of countries with estimated cases of HIV
  result$num_countries_hiv <- dataset %>%
    select(Estimated.number.of.people.living.with.HIV_median) %>%
    filter(!is.na(Estimated.number.of.people.living.with.HIV_median)) %>%
    summarize(num_hiv = n()) %>%
    pull(num_hiv)
  # number of countries with estimated amount of ART coverage
  result$num_countries_art <- dataset %>%
    select(Estimated.ART.coverage.among.people.living.with.HIV...._median) %>%
    filter(!is.na(
      Estimated.ART.coverage.among.people.living.with.HIV...._median
    )) %>%
    summarize(num_art = n()) %>%
    pull(num_art)
  # Country with the highest median estimate of HIV cases
  result$max_cases_country <- dataset %>%
    select(Country, Estimated.number.of.people.living.with.HIV_median) %>%
    filter(!is.na(Estimated.number.of.people.living.with.HIV_median)) %>%
    filter(Estimated.number.of.people.living.with.HIV_median == max(
      Estimated.number.of.people.living.with.HIV_median,
      na.rm = TRUE
    )) %>%
    pull(Country)
  # Estimated cases in highest country
  result$max_cases <- dataset %>%
    select(Country, Estimated.number.of.people.living.with.HIV_median) %>%
    filter(!is.na(Estimated.number.of.people.living.with.HIV_median)) %>%
    filter(Estimated.number.of.people.living.with.HIV_median == max(
      Estimated.number.of.people.living.with.HIV_median,
      na.rm = TRUE
    )) %>%
    pull(Estimated.number.of.people.living.with.HIV_median)
  # Country with lowest proportion of reported number of people receiving ART
  # coverage to estimated number of people living with HIV
  result$min_coverage_prop_country <- dataset %>%
    select(
      Country, Estimated.number.of.people.living.with.HIV_median,
      Reported.number.of.people.receiving.ART
    ) %>%
    filter(Reported.number.of.people.receiving.ART != "Nodata") %>%
    mutate(Reported_receiving_art = strtoi(
      Reported.number.of.people.receiving.ART
    )) %>%
    mutate(
      coverage_prop =
        Reported_receiving_art /
          Estimated.number.of.people.living.with.HIV_median
    ) %>%
    filter(
      coverage_prop == min(
        coverage_prop,
        na.rm = TRUE
      )
    ) %>%
    pull(Country)
  # Min coverage proportion of country with lowest proportion
  result$min_coverage_prop <- dataset %>%
    select(
      Country, Estimated.number.of.people.living.with.HIV_median,
      Reported.number.of.people.receiving.ART
    ) %>%
    filter(Reported.number.of.people.receiving.ART != "Nodata") %>%
    mutate(Reported_receiving_art = strtoi(
      Reported.number.of.people.receiving.ART
    )) %>%
    mutate(
      coverage_prop =
        Reported_receiving_art /
          Estimated.number.of.people.living.with.HIV_median
    ) %>%
    filter(
      coverage_prop == min(
        coverage_prop,
        na.rm = TRUE
      )
    ) %>%
    pull(coverage_prop)
  # World Health Org Region with the highest total coverage proportion
  result$max_region_cov_prop_region <- dataset %>%
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
      coverage_prop =
        sum(Reported_receiving_art, na.rm = TRUE) /
          sum(Estimated.number.of.people.living.with.HIV_median,
            na.rm = TRUE
          )
    ) %>%
    filter(
      coverage_prop == max(
        coverage_prop,
        na.rm = TRUE
      )
    ) %>%
    pull(WHO.Region)
  # Proportion of highest region
  result$max_region_cov_prop <- dataset %>%
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
      coverage_prop =
        sum(Reported_receiving_art, na.rm = TRUE) /
          sum(Estimated.number.of.people.living.with.HIV_median,
            na.rm = TRUE
          )
    ) %>%
    filter(
      coverage_prop == max(
        coverage_prop,
        na.rm = TRUE
      )
    ) %>%
    pull(coverage_prop)
  # World Health Org Region with the lowest total coverage proportion
  result$min_region_cov_prop_region <- dataset %>%
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
      coverage_prop =
        sum(Reported_receiving_art, na.rm = TRUE) /
          sum(Estimated.number.of.people.living.with.HIV_median,
            na.rm = TRUE
          )
    ) %>%
    filter(
      coverage_prop == min(
        coverage_prop,
        na.rm = TRUE
      )
    ) %>%
    pull(WHO.Region)
  # Proportion of lowest region
  result$min_region_cov_prop <- dataset %>%
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
      coverage_prop =
        sum(Reported_receiving_art, na.rm = TRUE) /
          sum(Estimated.number.of.people.living.with.HIV_median,
            na.rm = TRUE
          )
    ) %>%
    filter(
      coverage_prop == min(
        coverage_prop,
        na.rm = TRUE
      )
    ) %>%
    pull(coverage_prop)
  return(result)
}
