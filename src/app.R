options(shiny.port = 8050, shiny.autoreload = TRUE)

library(shiny)
library(tidyverse)
library(plotly)
library(rjson)
library(dplyr)
library(glue)
library(ggplot2)
library(sf)
library(ggswissmaps)


# prepare map data
ch <- shp_df[["g1l15"]]
sf_data <- st_as_sf(ch, coords = c("long", "lat"), crs = "+proj=somerc +lat_0=46.9524056 +lon_0=7.43958333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs")
sf_data_wgs84 <- st_transform(sf_data, "+proj=longlat +datum=WGS84")
ch$latitude <- st_coordinates(sf_data_wgs84)[, "Y"]
ch$longitude <- st_coordinates(sf_data_wgs84)[, "X"]

# prepare snow data
df <- read_csv('../data/preprocessed/all_data.csv')
month_order <- c('July', 'August', 'September', 'October', 
                 'November', 'December', 'January', 'February', 
                 'March', 'April', 'Mai', 'June')

df$Month_fct <- factor(df$Month, levels = month_order, ordered=TRUE)
colors <- c(rep('grey', each = 50))

theme <- bslib::bs_theme(
  bg = "white",
  fg = "black", 
  primary = "#B8BCC2", 
  secondary = "#B8BCC2",
  base_font = c("Grandstander", "sans-serif"),
  code_font = c("Courier", "monospace"),
  heading_font = "'Helvetica Neue', Helvetica, sans-serif",
  "input-border-color" = "#B8BCC2"
)


ui <- fluidPage(
  theme = theme,
  
  tags$head(
    tags$style(
      HTML(
        ".title-panel {
                    padding: 10px;
                }",
        ".title-panel h1 {
                    font-size: 24pt;
                    margin-top: 15px; 
                    margin-bottom: 0px;
                }",
        "hr {
          margin-top: 0px;
          margin-bottom: 0px;
        }"
      )
    )
  ),
  title = "SnoWatch - Snow Depth in Switzerland",
  tags$header(
    class = "col-sm-12 title-panel",
    tags$h1("SnoWatch - Snow Depth in Switzerland"),
  ),
  
  hr(),
  div(style = "height:20px"),
  
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
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
      fluidRow(
        column(12, align='center', plotlyOutput('map_plot', width = '100%'))
        ),
      
      div(style = "height:50px"),
      
      fluidRow(
        column(6, plotOutput('lineplot', width = '100%')),
        column(6, plotOutput('barplot', width = '100%'))
      ),
      
      div(style = "height:50px"),
      
      ),
    )
  )


# Server side callbacks/reactivity
server <- function(input, output, session) {

  # map
  output$map_plot <- renderPlotly({
    p <- ggplot(ch, aes(x = longitude, y = latitude)) +
      geom_path() + 
      coord_equal() +
      theme_white_f() + 
      geom_point(data = filter(df, !(Name %in% c(input$station_filter)) &
                                 !(Year %in% c(input$year_filter))), 
                 aes(x = Longitude, 
                     y = Latitude,
                     group = Longitude, 
                     text = Name),
                 color = 'lightblue', size = 2) +
      geom_point(data = filter(df, Name %in% c(input$station_filter) & 
                                 Year %in% c(input$year_filter)),
                 aes(x = Longitude, 
                     y = Latitude,
                     group = Longitude, 
                     text = Name),
                 color = 'red', size = 3)
    
    return(ggplotly(p, tooltip = c("text")) |> 
             config(displaylogo = FALSE,
                    scrollZoom = FALSE,
                    modeBarButtonsToRemove = c('pan', 'toImage', 'select2d', 'lasso2d')
                    )
           )
  })
  
    
    
    
#    renderPlotly({
#    return(
#      df |> 
#        plot_geo(locationmode = 'europe') |>
#        layout(geo = list(scope = 'europe'),
#               mapbox = list(
#                 zoom = 10,
#                 center = list(lon = 46, lat = 8))) |>
#        add_markers(x = ~Longitude, 
#                    y = ~Latitude,
#                    text = ~paste(Name),
#                    hoverinfo = "text",
#                    color = 'Stations',
#                    marker = list(color = "lightblue"),
#                    ) |> 
#        add_markers(data = filter(df, Name %in% c(input$station_filter) & 
#                                   Year %in% c(input$year_filter)), 
#                   x = ~Longitude, 
#                   y = ~Latitude,
#                   color = 'Selected',
#                   marker = list(color = "red"),
#                   text = ~paste(Name),
#                   hoverinfo = "text") |> 
#        config(displaylogo = FALSE,
#               scrollZoom= FALSE,
#               modeBarButtonsToRemove = c('pan', 'toImage', 'select2d', 'lasso2d')
#               )
#    )
#  })
  
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
        labs(title = glue('{input$metric_filter} cm of snow per month in {input$station_filter} in {input$year_filter} (red)\n compared to all other years (grey)'), 
             x = 'Month', 
             y = glue('{input$metric_filter} cm of Snow'), 
             color = 'Year') +
        scale_color_manual(values = colors, guide = 'none') +
        scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                           labels = c("July", "August", "September", "October", "November", 
                                      "December", "January", "February", "March", "April", 
                                      "May", "June")) +
        theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1),
              plot.title = element_text(size = 17),
              axis.text = element_text(size = 15),
              axis.title = element_text(size = 15)) +
        geom_line(data = filter(df, Name %in% c(input$station_filter) & Year %in% c(input$year_filter)),
                  aes(x = Month_help, 
                      y = !!sym(input$metric_filter)),
                  color = 'red')
      )
  })
  
  # barchart
  output$barplot <- renderPlot({
    return(
      df |> 
        filter(Name %in% c(input$station_filter)) |>
        filter(Month %in% c(input$month_filter)) |> 
        ggplot(aes(x=!!sym(input$metric_filter))) +
        geom_histogram(stat="density") +
        theme(axis.text.x = element_text(angle=0, vjust=1, hjust=1),
              plot.title = element_text(size = 17),
              axis.text = element_text(size = 15),
              axis.title = element_text(size = 15)) +
        labs(title = glue('{input$metric_filter} cm of snow in {input$station_filter} in {input$month_filter} {input$year_filter} (red)\n compared to distribution of {input$metric_filter} cm of snow\n in {input$month_filter} in {input$station_filter} between {min(df$Year)} and {max(df$Year)}'), 
             x = glue('{input$metric_filter} cm of Snow'),
             y = 'Density'
             ) +
        geom_vline(
          xintercept = first(pull(filter(df, 
                                         Name %in% c(input$station_filter) & 
                                           Year %in% c(input$year_filter) & 
                                           Month %in% c(input$month_filter)), 
                                  !!sym(input$metric_filter))),
          linetype = 'dashed', 
          color = 'red'
        )
    )
    })
}


# Run the app/dashboard
shinyApp(ui, server)

