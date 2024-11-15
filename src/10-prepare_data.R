# Prepare data

# Init ------------------------------------------------------------

library(yaml)
library(readr)
library(dplyr)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  config = './config.yaml',
  global = './src/00-global_objects.R',
  # deaths and exposures
  raw_data = './dat'
)
paths$output <- list(
  model_input = './out/model_input.rds'
)

# global configuration
#config <- read_yaml(paths$input$config)

# constants specific to this analysis
cnst <- within(list(), {
  fitting_ages = 0:100
  J = length(fitting_ages)
  fitting_years = 1871:2000
  T = length(fitting_years)
})

# Load data -------------------------------------------------------

countries <- list.dirs(paths$input$raw_data, full.names = FALSE)[-1]
input_data <- lapply(countries, function (l) {
  list(
    country = l,
    path_Dxt = paste0(paths$input$raw_data, '/', l, '/Deaths_1x1.txt'),
    path_Ext = paste0(paths$input$raw_data, '/', l, '/Exposures_1x1.txt')
  )
})

model_data <- lapply(input_data, function (l) {

  Dxt_input <- read_table(l$path_Dxt, skip = 2, na = '.')
  Ext_input <- read_table(l$path_Ext, skip = 2, na = '.')

  DxEx <-
    left_join(Dxt_input, Ext_input, by = c('Year', 'Age')) |>
    select(Year, Age, Dx = Male.x, Ex = Male.y) |>
    mutate(Age = as.integer(Age),
           Age = ifelse(is.na(Age), 110, Age),
           Dx = floor(Dx)) |>
    filter(Age %in% cnst$fitting_ages) |>
    filter(Year %in% cnst$fitting_years)

  Dxt <- matrix(
    DxEx$Dx, nrow = cnst$J, ncol = cnst$T,
    dimnames = list(Age = cnst$fitting_ages, Year = cnst$fitting_years)
  )
  Ext <- matrix(
    DxEx$Ex, nrow = cnst$J, ncol = cnst$T,
    dimnames = list(Age = cnst$fitting_ages, Year = cnst$fitting_years)
  )

  list(
    country = l$country,
    Dxt = Dxt, Ext = Ext
  )
})

names(model_data) <- countries

# Export ----------------------------------------------------------

saveRDS(model_data, file = paths$output$model_input)
