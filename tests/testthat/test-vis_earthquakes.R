context("Test the basic functionality of the package\n\n")
library(earthquakes)
library(testthat)
library(ggplot2)

setwd(system.file("extdata", package = "earthquakes"))

test_that("Clean Data", {
  print("Testing Data Clean Functon")
  eq_data <- eq_clean_data("signif.txt")
  expect_that(eq_data, is_a("data.frame"))
  expect_that(eq_data$DATE, is_a("Date"))
})

test_that("Location Name", {
  print("Testing Location Name Functon")
  eq_data <- eq_clean_data("signif.txt")
  expect_that(eq_data, is_a("data.frame"))
  expect_that(eq_data$DATE, is_a("Date"))

  eq_data$LOCATION_NAME <- eq_data %>% eq_location_clean()
  expect_that(eq_data$LOCATION_NAME, is_a("character"))
})

test_that("Geom Timeline", {
  print("Testing Geom Timeline Functon")
  expect_is({x <- eq_clean_data("signif.txt") %>%
    dplyr::filter(lubridate::year(DATE) %in% 2000:2017 & COUNTRY == "USA") %>%
    ggplot(aes( x = DATE,
                y = COUNTRY,
                colour = TOTAL_DEATHS,
                size = EQ_PRIMARY,
                date = DATE,
                countries = COUNTRY,
                location_name = LOCATION_NAME)) +
    geom_timeLine()
    x$layers[[1]]$geom}, "GeomTimeLine")
})

test_that("Geom TimelineLabel", {
  print("Testing Geom TimelineLabel Functon")
  expect_is({x <- eq_clean_data("signif.txt") %>%
    dplyr::filter(lubridate::year(DATE) %in% 2000:2017 & COUNTRY == "USA") %>%
    ggplot(aes( x = DATE,
                y = COUNTRY,
                colour = TOTAL_DEATHS,
                size = EQ_PRIMARY,
                date = DATE,
                countries = COUNTRY,
                location_name = LOCATION_NAME)) +
    geom_timeLine() +
    geom_timeline_label(n_max = 5)
      x$layers[[1]]$geom}, "GeomTimeLine")

  expect_that({x <- eq_clean_data("signif.txt") %>%
    dplyr::filter(lubridate::year(DATE) %in% 2000:2017 & COUNTRY == "USA") %>%
    ggplot(aes( x = DATE,
                y = COUNTRY,
                colour = TOTAL_DEATHS,
                size = EQ_PRIMARY,
                date = DATE,
                countries = COUNTRY,
                location_name = LOCATION_NAME)) +
    geom_timeLine() +
    geom_timeline_label(n_max = 5)
    x$labels$location_name}, equals("LOCATION_NAME"))
})

test_that("Geom Timeline Them", {
  print("Testing Geom Timeline Them Functon")
  expect_is(them_timeline(), "theme")
})

test_that("Earthquakes Map", {
  print("Testing Earthquakes Map Functon")
  expect_is(eq_clean_data("signif.txt") %>%
              dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
              eq_map(annot_col = "DATE"), "leaflet")
})

test_that("Earthquakes Map with Popup Label", {
  print("Testing Earthquakes Map with Popup Label Functon")
  eq_data <- eq_clean_data("signif.txt")
  expect_match(eq_create_label(eq_data[1,]), "<b>Location:</b> IRAN-IRAQ:  DINAVAR,BAGHDAD, TIGRIS-KTESIPHON")
})
