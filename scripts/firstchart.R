# Loading Libraries
library("dplyr")
library("plotly")
library("ggplot2")
library("leaflet")

# Function for creating geographical chart
geographical_chart <- function(df1, df2) {
  combined_data <- merge(df1, df2,
    by = "Country",
    all.x = TRUE
  )
  # Turning column in numeric values
  combined_data$art_received <-
    as.numeric(combined_data$Reported.number.of.people.receiving.ART)
  # Filtering Columns to show only countries with data in reported number of
  # people receiving ART.
  filtered_hiv <- filter(combined_data, art_received != "Nodata")
  # Combining columns for information
  content <- paste(
    sep = "<br/>",
    filtered_hiv$Country,
    paste0("Art Received: ", filtered_hiv$art_received)
  )
  # Color
  pal <- colorNumeric(
    palette = "RdYlBu",
    domain = filtered_hiv$art_received
  )
  # Interactive Geographical Plot for Reported number of People Receiving Art
  geographic_plot_art <- leaflet(data = filtered_hiv) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addCircles(
      lng = ~Longitude,
      lat = ~Latitude,
      stroke = TRUE,
      weight = 1,
      fillOpacity = 0.3,
      color = ~ pal(art_received),
      radius = ~ sqrt(art_received) * 600,
      popup = content
    ) %>%
    addLegend(
      "bottomright",
      pal = pal,
      values = ~art_received,
      title = "Number of People Receiving ART"
    )
  return(geographic_plot_art)
}

# Description
# This is an interactive geographical visualization for the reported number
# of people receiving ART. You can see which country has the most number of
# people receiving the treatment based on the size of the circles. When
# the circle is clicked, it will show the user the country and the exact
# number of people who received the treatment.
