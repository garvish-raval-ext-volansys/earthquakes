# NOAA earthquakes

The goal of earthquakes package is to visualize some of the information in the NOAA earthquakes dataset. In particular, we would like to visualize the times at which earthquakes occur within certain countries. In addition to showing the dates on which the earthquakes occur, we can also show the magnitudes (i.e. Richter scale value) and the number of deaths associated with each earthquake.

## Example

We first need to set Working directory where data files are available in local compter or just copy all data files into working directory. We provided datafile as external data in this packageto explore data. We also initialize variables for filter of Countries and timeframe.

```R
library(earthquakes)

#Setting Working directory
setwd(system.file("extdata", package = "earthquakes"))

#Assigning raw dataset file path
raw_data_file <- "signif.txt"

#Assigning start date
START_DATE <- "2000-01-01"

#Assigning end date
END_DATE <- "2017-01-01"

#Assigning Max Number
N_MAX <- 5

#Select Contry from Dataset
#COUNTRIES <-c("USA","CHINA","INDIA")
#COUNTRIES <- c("USA", "CHINA")
COUNTRIES <- c("USA")
#COUNTRIES <- c("MEXICO")

eq_data <- eq_clean_data(raw_data_file)
eq_data$LOCATION_NAME <- eq_location_clean(eq_data)
eq_data <- eq_data %>% dplyr::filter(COUNTRY %in% COUNTRIES)
```

## Example 1: Sample code to visaulize timeline of earthquakes.
```R
ggplot(
  data = eq_data,
   aes(
     x = DATE,
     y = COUNTRY,
     colour = eq_data$TOTAL_DEATHS,
     size = eq_data$EQ_PRIMARY,
     date = eq_data$DATE,
     countries = eq_data$COUNTRY,
     location_name = eq_data$LOCATION_NAME
   )
 ) +
geom_timeLine(xmin = START_DATE, xmax = END_DATE) +
geom_timeline_label(xmin = START_DATE,
                     xmax = END_DATE,
                     n_max = N_MAX) +
them_timeline() +
labs(
     y = "",
     x = "Date",
     size = "Richter Scale Value",
     colour = "# deaths"
)
```
## Example 2: Sample code to visaulize earthquakes on leaflet Map.
```R
eq_data %>%
  dplyr::filter(COUNTRY %in% COUNTRIES & 
                  lubridate::year(DATE) >= lubridate::year(START_DATE)) %>%
  dplyr::mutate(popup_text = eq_create_label(.)) %>%
  eq_map(annot_col = "popup_text")
```
