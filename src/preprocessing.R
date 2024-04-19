library(tidyverse)

daily_CH_METEO <- read_csv('../data/raw/data_monthly_CH_METEOSWISS.csv')
meta <- read_csv('../data/raw/meta_all.csv')

start_year <- 1970
end_year <- 2015

df <- left_join(daily_CH_METEO, meta, by = "Name") |> 
  filter(HN_year_start <= start_year, HS_year_start <= start_year) |> 
  filter(HN_year_end >= end_year, HS_year_end >= end_year) |> 
  filter(year >= start_year, year <= end_year)

df <- df[!(df$Name %in% unique(df[!complete.cases(df), ]$Name)),]

df <- df |> 
  mutate(Name = case_when(
    Name == 'Andermatt_CH_METEOSWISS' ~ 'Andermatt',
    Name == 'Arosa_CH_METEOSWISS' ~ 'Arosa',
    Name == 'Elm_CH_METEOSWISS' ~ 'Elm',
    Name == 'Samedan_CH_METEOSWISS' ~ 'Samedan',
    TRUE ~ Name
  ))

df$month <- factor(month.name[df$month], levels = c(
  "July", "August", "September", "October", "November", "December", 
  "January", "February", "March", "April", "May", "June"))
  

df$Month_help <- df$month

df <- df |> 
  mutate(Month_help = case_when(
    Month_help == 'July' ~1, 
    Month_help == 'August' ~2, 
    Month_help == 'September' ~3, 
    Month_help == 'October' ~4, 
    Month_help == 'November' ~5, 
    Month_help == 'December' ~6, 
    Month_help == 'January' ~7, 
    Month_help == 'February' ~8, 
    Month_help == 'March' ~9, 
    Month_help == 'April' ~10, 
    Month_help == 'May' ~11, 
    Month_help == 'June' ~12
  )) |> 
  select(Name, year, month, HNsum, HSmean, HSmax, Longitude, Latitude, Elevation, Month_help)

names(df) <- c("Name", "Year", "Month", "Total", "Average", 
               "Maximum", "Longitude", "Latitude", "Elevation", "Month_help")

write_csv(df, file = "../data/preprocessed/all_data.csv")