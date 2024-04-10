options(shiny.port = 8050, shiny.autoreload = TRUE)

library(shiny)
# library(ggplot2)
library(tidyverse)
library(plotly)
library(rjson)
library(dplyr)


df <- read_csv('../data/preprocessed/all_data.csv')
df <- df[220000:221000, ]




ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = 'darkly'),
  
  titlePanel(
    "Snow in Switzerland"),
  
  sidebarLayout(
    
    sidebarPanel(
    selectInput(
      'station_filter',
      'Select station',
      choices = unique(df$Name)
    )
    ),
    
    mainPanel(
      plot_geo(df, locationmode = 'europe') |>
        layout(geo = list(scope = 'europe'),
               mapbox = list(
                 zoom = 5,
                 center = list(lon = 46, lat = 8))) |>
        add_markers(x = ~Longitude, y = ~Latitude),
      
      
      plotOutput('plot', width = '400px')
      )
    )
  )


# Server side callbacks/reactivity
server <- function(input, output, session) {
  
  # lineplot per selection
  output$plot <- renderPlot({
    return(
      df |> 
        filter(Name %in% c(input$station_filter)) |> 
           ggplot(aes(x = month, y = HSmean, color = factor(year))) +
           geom_line() +
           labs(title = 'HSmean values over 12 months', 
                x = 'Month', 
                y = 'HSmean', 
                color = 'Year') +
          scale_color_discrete(name = 'Year') +
          theme_minimal()
      )
  })
  
  # test map
  output$plot_deaths <- renderPlotly({
    return(
      ggplotly(
        ggplot(df,
               aes(x = year, y = deaths, color = Country)
        ) +
          geom_line() +
          geom_line() +
          labs(
            x = "Date",
            y = "Number of Deaths",
            title = "Cumulative no. of confirmed, probable, and suspected deaths"
          )
      )
    )
  })
  
  
  
}


# Run the app/dashboard
shinyApp(ui, server)

































