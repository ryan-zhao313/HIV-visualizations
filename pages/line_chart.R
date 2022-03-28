# Loading Libraries
library("dplyr")
library("shiny")

# Reading Data
data <- read.csv("../data/deaths-and-new-cases-of-hiv.csv")
# data <- data %>% 
#   group_by(data$Entity)


country_selection <- selectInput(
  "country",
  label = h3("Choose a Country"),
  choices = unique(data$Entity)
)

years_selection <- sliderInput(
  "years", 
  label = h3("Year Range"), 
  min = min(data$Year, na.rm = TRUE), 
  max = max(data$Year, na.rm = TRUE), 
  value = c(min(data$Year, na.rm = TRUE), max(data$Year, na.rm = TRUE)),
  sep = ""
)

ui <- fluidPage(
  h1("See How Selected Countries Have Been Dealing with HIV/AIDS"),
  country_selection,
  years_selection,
  plotOutput("linechart")
)


server <- function(input, output) {
  output$linechart <- renderPlot({
    title <- paste0(input$country, " between ", input$years)
    
    isolatedData <- data %>% 
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
    
    country_plot <- ggplot(isolatedData, aes(x = isolatedData$Year)) +
      geom_line(aes(y = isolatedData$Deaths.HIV.AIDS, color = 
                      "Deaths.HIV.AIDS")) + 
      geom_line(aes(y = isolatedData$Incidence.of.HIV.AIDS, color = 
                      "Incidence.HIV.AIDS")) +
      geom_line(aes(y = isolatedData$Prevalance.of.HIV.AIDS, color = 
                      "Prevalance.HIV.AIDS")) +
      scale_color_manual("", breaks = c("Deaths.HIV.AIDS", "Incidence.HIV.AIDS",
                                        "Prevalance.HIV.AIDS"),
                         values = c("Deaths.HIV.AIDS" = "green", 
                                    "Incidence.HIV.AIDS" = "red",
                                    "Prevalance.HIV.AIDS" = "blue")) +
      labs(x = paste0("Data Between ", input$years[1], "-", input$years[2]),
           y = "Total Number", title = 
             paste0("Deaths, Incidence, and Prevalance in ", input$country))
    
    return(country_plot)
  })
}

chart <- tabPanel(
  "Line Chart",
  fluidPage(
    h2("See How Selected Countries Have Been Dealing with HIV"),
    country_selection,
    years_selection
  )
)


shinyApp(ui, server)

