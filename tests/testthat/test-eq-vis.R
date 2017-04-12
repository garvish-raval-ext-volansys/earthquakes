context("Test the basic functionality of the package\n\n")
library(earthquakes)
library(testthat)
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

})

test_that("Geom TimelineLabel", {
  print("Testing Geom TimelineLabel Functon")

})

test_that("Geom Timeline Them", {
  print("Testing Geom Timeline Them Functon")

})

test_that("Earthquakes Map", {
  print("Testing Earthquakes Map Functon")

})

test_that("Earthquakes Map with Popup Label", {
  print("Testing Earthquakes Map with Popup Label Functon")

})
