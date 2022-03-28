# Libraries
library("shiny")
library("leaflet")
library("dplyr")
library("ggplot2")
library("tidyverse")


# load data
hiv_df <- read.csv("../data/art_coverage_by_country_clean.csv",
                   stringsAsFactors = FALSE)

# Data for country coordinates
country_coordinates <- read.csv("../data/world_country_coordinate.csv",
                                stringsAsFactors = FALSE)

# combine data
hiv_country <- merge(hiv_df, country_coordinates,
                     by = "Country",
                     all.x = TRUE)

# group_by region
hiv_region <- hiv_country %>%
  drop_na() %>%
  select(
    Country, Estimated.number.of.people.living.with.HIV_median,
    Reported.number.of.people.receiving.ART, WHO.Region, Latitude,
    Longitude
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
          na.rm = TRUE),
    avg_lat = mean(Latitude, na.rm = TRUE),
    avg_long = mean(Longitude, na.rm = TRUE)
  )
  

map_page <- tabPanel(
  "Map",
  fluidPage(
    h2("View Country and Region data on the map")
    )
  )

region_selection <- selectInput(
  "region",
  label = h3("Choose a WHO Region"),
  choices = hiv_region$WHO.Region
)


ui <- fluidPage(
  map_page,
  region_selection,
  h5("Click the marker that appears to view data about your selected region"),
  leafletOutput("region_map"),
  h4("Map Summary"),
  textOutput("map_explanation")
)


server <- function(input, output) {
  output$region_map <- renderLeaflet({
    
    selected_region <- hiv_region %>%
      filter(WHO.Region == input$region)
    
    pal <- colorFactor(
      palette = "Dark2",
      domain = selected_region$WHO.Region
    )
    
    leaflet(data = selected_region) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = selected_region$avg_long,
              lat = selected_region$avg_lat,
              zoom = 3) %>%
      addCircles(
        lng = ~selected_region$avg_long,
        lat = ~selected_region$avg_lat,
        color = ~pal(selected_region$WHO.Region),
        radius = 2000000,
        stroke = TRUE,
        weight = 1,
        fillOpacity = 0.3,
        popup = paste("Region: ", selected_region$WHO.Region, "<br>",
                      "Total HIV Cases: ", selected_region$total_cases, "<br>",
                      "People Recieving ART: ", selected_region$total_coverage, "<br>",
                      "Coverage Proportion: ", selected_region$coverage_prop, "<br>")
      )
  })
  output$map_explanation <- renderText({
    paste("This map allows for the selection of a World Health Organization
    region and then places a marker in the general area of that region.
    Clicking that marker will show summary HIV and ART data for that entire
    region with the data points: Total HIV Cases, number of People Receiving
    ART, and Coverage Proportion. This is meant to give the viewer quick
    insights about different areas of the world and specifically which regions
    need the most help in terms of more ART coverage. Furthermore, it allows
    for equitable comparisons directly between regions because it gives the
          proportion of art coverage in a region to the total HIV cases.")
  })
}


shinyApp(ui = ui, server = server)
