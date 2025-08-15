library(R6)
library(plumber)
library(later)
library(jsonlite)
library(yaml)
library(httpuv)

setwd("~/Library/CloudStorage/GoogleDrive-basar.erdivanli@erdogan.edu.tr/My Drive/R/Deneme")

`%||%` <- function(a, b) if (is.null(a)) b else a

engine$finalize()
engine <- NULL

model_files  <- list.files("models", pattern="\\.R$", full.names=TRUE)
for (f in model_files) {
  source(f)
}

engine <- SimulationEngine$new(
  patient_config = 'patients/healthy_adult.yml',
  machine_config = 'machines/datex_ohmeda_s5_avance.yml',  
  step_interval = 1.0,
  http_port = 8080
)
