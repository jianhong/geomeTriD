require("geomeTriD") || stop("unable to load Package:geomeTriD")
require("testthat") || stop("unable to load testthat")

x <- readRDS(system.file("extdata", "4DNFI1UEG1HD.chr21.FLAMINGO.res.rds",
  package = "geomeTriD"
))
gi <- readRDS(system.file("extdata", "gi.rds", package = "trackViewer"))
test_check("geomeTriD")
