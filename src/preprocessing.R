library(tidyverse)
library(plotly)
library(rjson)
library(lubridate)

daily_CH_METEO <- read_csv('../data/raw/data_monthly_CH_METEOSWISS.csv')
daily_CH_SLF <- read_csv('../data/raw/data_monthly_CH_SLF.csv')
meta <- read_csv('../data/raw/meta_all.csv')

df <- left_join(daily_CH_METEO, meta, by = "Name") 

write_csv(df, file = "../data/preprocessed/all_data.csv")

data <- daily_CH_METEO
year <- 1980
name <- 'Aadorf_Tanikon'

daily_CH_METEO |> 
  filter(Name == name) |> 
  # filter(year == year) |> 
  ggplot(aes(x=month, 
             y=HSmean, 
             color=year)) +
  geom_line(stat = 'summary', 
            fun = mean)




p <- plot_geo() |> 
  add_markers(
    data = df, x = ~Longitude, y = ~Latitude, alpha = 0.5)|> 
  layout(
    geo = list(
      scope = 'europe',
      lakecolor = toRGB('white')
    ),
    mapbox = list(
      zoom = 200,
      center = list(lon = 46, lat = 8)
    ))

p



df <- read_csv('../data/preprocessed/all_data.csv')
df <- df[220000:221000, ]

df |> 
  filter(Name == 'Zurich_Fluntern') |>
  ggplot(aes(x = month, y = HSmean, color = factor(year))) +
  geom_line() +
  labs(title = 'TEST HSmean values over 12 months', x = 'Month', y = 'HSmean', color = 'Year') +
  scale_color_discrete(name = 'Year') +
  theme_minimal()


















options(shiny.port = 8050, shiny.autoreload = TRUE)

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

data <- read.csv("../data/raw/ebola_2014_2016_clean.csv") |>
  mutate(
    cases = Cumulative.no..of.confirmed..probable.and.suspected.cases,
    deaths = Cumulative.no..of.confirmed..probable.and.suspected.deaths,
    date = as.Date(Date)
  )

data$Date <- as.Date(data$Date)


# Layout
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = 'darkly'),
  
  titlePanel(
    h1("2014-2016 Ebola Outbreak in Western Africa"),
    h2("Exploring cumulative cases and deaths")
  ),
  
  fluidRow(
    column(3,
           selectizeInput(
             "country_multi",
             "Select country:",
             choices = unique(data$Country),
             multi=TRUE,
             options = list(
               placeholder = c("Select countries..."),
               onInitialize = I('function() { this.setValue(["Guinea", "Liberia", "Sierra Leone"]); }'))
           ),
           
           sliderInput(
             'x_range',
             'Time period:',
             min = data$Date |> min(),
             max = data$Date |> max(),
             value = c(data$Date |> min(), data$Date |> max())
           ),
    ),
    
    column(7,
           plotlyOutput("plot_cases", width = "800px"),
           plotlyOutput("plot_deaths", width = "800px")
    )
    
  ),
  fluidRow(
    column(5,
           plot_ly(data, 
                   type='choropleth', 
                   locationmode = 'country names',
                   locations=~Country, z=~cases, text=~Country, color=~cases,
                   colorscale='Purples')
    ),
    column(5,
           plot_ly(data, type='choropleth', locationmode = 'country names',
                   locations=~Country, z=~deaths, text=~Country, color=~deaths,
                   colorscale='Reds')
    )
  )
)

# Server side callbacks/reactivity
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    data |>
      filter(Country %in% c(input$country_multi)) |>
      filter(between(date, input$x_range[1], input$x_range[2]))
    
  })
  
  output$plot_cases <- renderPlotly({
    return(
      ggplotly(
        ggplot(filtered_data(),
               aes(x = date, y = cases, color = Country)
        ) +
          geom_line() +
          labs(
            x = "Date",
            y = "Number of Cases",
            title = "Cumulative no. of confirmed, probable, and suspected cases"
          )
      )
    )
  })
  
  output$plot_deaths <- renderPlotly({
    return(
      ggplotly(
        ggplot(filtered_data(),
               aes(x = date, y = deaths, color = Country)
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


  
  


         

  