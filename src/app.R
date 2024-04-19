options(shiny.port = 8050, shiny.autoreload = TRUE)

library(shiny)
library(tidyverse)
library(plotly)
library(rjson)
library(dplyr)
library(glue)


df <- read_csv('../data/preprocessed/all_data.csv')
month_order <- c('July', 'August', 'September', 'October', 
                 'November', 'December', 'January', 'February', 
                 'March', 'April', 'Mai', 'June')

df$Month_fct <- factor(df$Month, levels = month_order, ordered=TRUE)

colors <- c(rep('grey', each = 50))


ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = 'darkly'),
  
  titlePanel(
    "Snow in Switzerland"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(
        'station_filter',
        'Select a Station',
        choices = unique(df$Name)
      ),
      selectInput(
        'metric_filter',
        'Select a Metric',
        choices = colnames(df)[4:6]
      ),
      selectInput(
        'month_filter',
        'Select a Month',
        choices = unique(df$Month)
      ),
      selectInput(
        'year_filter',
        'Select a Year',
        choices = unique(df$Year)
      )
    ),
    
    mainPanel(
      
      plotOutput('map_plot', width = '300px'),
      plotOutput('lineplot', width = '300px'),
      plotOutput('barplot', width = '300px'),
      textOutput('selected_var')
      )
    )
  )


# Server side callbacks/reactivity
server <- function(input, output, session) {
  
  # map
  output$map_plot <- renderPlot({
    return(
      df |> 
      plot_geo(locationmode = 'europe') |>
        layout(geo = list(scope = 'europe'),
               mapbox = list(
                 zoom = 5,
                 center = list(lon = 46, lat = 8))) |>
        add_markers(x = ~Longitude, y = ~Latitude) 
        # add_markers(data = filter(df, Name %in% c(input$station_filter) & 
        #                             Year %in% c(input$year_filter)), 
        #             x = ~Longitude, 
        #             y = ~Latitude,
        #             color = 'red')
    )
  })
  
  # lineplot per selection
  output$lineplot <- renderPlot({
    return(
      df |> 
        filter(Name %in% c(input$station_filter)) |> 
        ggplot() +
        geom_line(aes(x = Month_help, 
                      y = !!sym(input$metric_filter),
                      color = factor(Year)
        )) +
        labs(title = glue('{input$metric_filter} cm of Snow in {input$station_filter}'), 
             x = 'Month', 
             y = glue('{input$metric_filter} cm of Snow'), 
             color = 'Year') +
        scale_color_manual(values = colors, guide = 'none') +
        scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                           labels = c("July", "August", "September", "October", "November", 
                                      "December", "January", "February", "March", "April", 
                                      "May", "June")) +
        theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
        geom_line(data = filter(df, Name == 'Altdorf' & Year == 2010),
                  aes(x = Month_help, 
                      y = !!sym(input$metric_filter)),
                  color = 'red')
      )
  })
  
  # debug
  output$selected_var <- renderText({
    paste("you have selected", input$metric_filter)
  })
  
  # barchart
  output$barplot <- renderPlot({
    return(
      df |> 
        filter(Name %in% c(input$station_filter)) |>
        filter(Month %in% c(input$month_filter)) |> 
        ggplot(aes(x=!!sym(input$metric_filter))) +
        geom_histogram(stat="density") +
        labs(title = glue('{input$metric_filter} cm of Snow in {input$month_filter} in {input$station_filter}'), 
             x = glue('{input$metric_filter} cm of Snow')
             #y = 'Count'
             ) +
        theme_minimal()
    )
  })
  
  
  
}


# Run the app/dashboard
shinyApp(ui, server)

