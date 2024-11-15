# Fit Lee-Carter Model

# Init ------------------------------------------------------------

library(yaml)
library(readr)
library(dplyr)
library(rstan)

# use all available cores
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  config = './config.yaml',
  global = './src/00-global_objects.R',
  # deaths and exposures
  model_input = './out/model_input.rds',
  stan_model = './src/poisson_lee_carter.stan'
)
paths$output <- list(
  out = './out'
)

# global configuration
#config <- read_yaml(paths$input$config)

# constants specific to this analysis
cnst <- within(list(), {
  nsim = 1000
  seed = 1987
})

# Functions -------------------------------------------------------

# global objects and functions
source(paths$input$global)

# Load data -------------------------------------------------------

model_input <- readRDS(paths$input$model_input)

# illustrate
PlotMatrix(log(model_input$englandwales$Dxt/model_input$englandwales$Ext), N = 10)

# Compile and fit model -------------------------------------------

# prepare data for stan
standata <- list(
  J = nrow(model_input$englandwales$Dxt),
  T = ncol(model_input$englandwales$Dxt),
  Y = as.vector(model_input$englandwales$Dxt),
  E = as.vector(model_input$englandwales$Ext),
  H = 10
)

# compile model
lcmodel <- stan_model(file = paths$input$stan_model)

# 0.0019s
lcfit <- sampling(
  object = lcmodel,
  data = standata,
  chains = 1,
  warmup = 1e3,
  iter = 1e3+cnst$nsim,
  #output_samples = cnst$nsim,
  #seed = cnst$seed,
  #importance_resampling = TRUE
  verbose = TRUE
)

# Diagnostics -----------------------------------------------------

sims <- extract(lcfit)

# life expectancy forecasts
X_prd <- apply(
  sims$mu_h,
  c(1,3),
  LifeExpectancyFromMortality
)
X_fit <- apply(
  exp(sims$eta),
  c(1,3),
  LifeExpectancyFromMortality
)
plot.new()
plot.window(xlim = c(0, 150), y = c(30, 80))
axis(1); axis(2)
points(X_fit[1,1,])
for (i in 1:100) {
  lines(X_fit[1,i,], col = rgb(0,0,0,0.1))
}
for (i in 1:100) {
  lines(x = 130+1:standata$H, y= X_prd[1,i,], col = rgb(0,0,0,0.1))
}

# lee carter fit diagnostics and parameters
diagnostics <- within(list(), {
  eta_avg = apply(sims$eta, 2:3, mean)
  logrates_obs =
    log(model_input$englandwales$Dxt) -
    log(model_input$englandwales$Ext)
  epsilon = logrates_obs - eta_avg
  ax_avg = colMeans(sims$a_x)
  bx_avg = colMeans(sims$b_x)
  kt_avg = colMeans(sims$k_t)
  theta = list(ax = ax_avg, bx = bx_avg, kt = kt_avg)
})
PLCplotFitDiagnostics(
  dev = c(100,1),
  maxit = 1,
  epsilon = diagnostics$epsilon,
  eta = diagnostics$eta_avg,
  theta = diagnostics$theta,
  outlier = FALSE
)
