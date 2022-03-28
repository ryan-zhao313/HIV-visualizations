server <- function(input, output) {
  output$region_chart <- renderPlotly({
    chart <- plot_ly(
      data = bar_chart_df,
      x = ~ bar_chart_df$Country[bar_chart_df$WHO.Region == input$regionselect],
      y = ~ bar_chart_df$M_ART_coverage_among_living_HIV[bar_chart_df$WHO.Region
      == input$regionselect],
      type = "bar"
    ) %>%
      layout(
        xaxis = list(title = "Country", automargin = TRUE),
        yaxis = list(title = "ART Coverage among People with HIV")
      )
    return(chart)
  })
  output$chartexplanation <- renderText({
    message <- paste("This page allows for users to select a specific
                     region, then outputs a bar chart which displays the
                     Median ART coverage among people living with HIV in the
                     countries within that selected region. This visual allows
                     for easy comparison of ART coverage to be made between
                     countries within certain regions, giving viewers
                     a better understanding of which countries and regions
                     have good coverage, and which ones need more.")
  })
  output$region_map <- renderLeaflet({
    selected_region <- hiv_region %>%
      filter(WHO.Region == input$region)

    pal <- colorFactor(
      palette = "Dark2",
      domain = selected_region$WHO.Region
    )

    leaflet(data = selected_region) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(
        lng = selected_region$avg_long,
        lat = selected_region$avg_lat,
        zoom = 3
      ) %>%
      addCircles(
        lng = ~ selected_region$avg_long,
        lat = ~ selected_region$avg_lat,
        color = ~ pal(selected_region$WHO.Region),
        radius = 2000000,
        stroke = TRUE,
        weight = 1,
        fillOpacity = 0.3,
        popup = paste(
          "Region: ", selected_region$WHO.Region, "<br>",
          "Total HIV Cases: ", selected_region$total_cases, "<br>",
          "People Recieving ART: ", selected_region$total_coverage, "<br>",
          "Coverage Proportion: ", selected_region$coverage_prop, "<br>"
        )
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
  output$linechart <- renderPlot({
    title <- paste0(input$country, " between ", input$years)

    isolated_data <- death_rate_df %>%
      group_by(Entity) %>%
      rename(
        Deaths.HIV.AIDS =
          Deaths...HIV.AIDS...Sex..Both...Age..All.Ages..Number.,
        Incidence.of.HIV.AIDS =
          Incidence...HIV.AIDS...Sex..Both...Age..All.Ages..Number.,
        Prevalance.of.HIV.AIDS =
          Prevalence...HIV.AIDS...Sex..Both...Age..All.Ages..Number.
      ) %>%
      filter(Entity == input$country) %>%
      filter(Year >= input$years[1] & Year <= input$years[2])

    country_plot <- ggplot(isolated_data, aes(x = isolated_data$Year)) +
      geom_line(aes(
        y = isolated_data$Deaths.HIV.AIDS, color =
          "Deaths.HIV.AIDS"
      )) +
      geom_line(aes(
        y = isolated_data$Incidence.of.HIV.AIDS, color =
          "Incidence.HIV.AIDS"
      )) +
      geom_line(aes(
        y = isolated_data$Prevalance.of.HIV.AIDS, color =
          "Prevalance.HIV.AIDS"
      )) +
      scale_color_manual("",
        breaks = c(
          "Deaths.HIV.AIDS", "Incidence.HIV.AIDS",
          "Prevalance.HIV.AIDS"
        ),
        values = c(
          "Deaths.HIV.AIDS" = "green",
          "Incidence.HIV.AIDS" = "red",
          "Prevalance.HIV.AIDS" = "blue"
        )
      ) +
      labs(
        x = paste0("Data Between ", input$years[1], "-", input$years[2]),
        y = "Total Number", title =
          paste0("Deaths, Incidence, and Prevalance in ", input$country)
      )

    return(country_plot)
  })
  output$linechartexplanation <- renderText({
    linechartsummary <- paste("This chart allows you to see death rate,
                              incidence, and prevalance of HIV/AIDS over the 
                              years for chosen countries. It allows the user to 
                              have a general understanding of how individual
                              countries have been handling the HIV crisis over
                              time and provides valuable information on which
                              countries are struggling the most with
                              the disease. It will provide answers
                              to whether the HIV/AIDS epidemic is getting
                              better or worse in whichever country the user
                              is interested in knowing about. There is also
                              a \"world\" option for users to see the general
                              trend for the entire world and not just a single
                              country.")
  })
}
