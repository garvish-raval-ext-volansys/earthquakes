library(testthat)
library(earthquakes)

test_check("earthquakes")
test_file("testthat/test-vis_earthquakes.R")
