library(R6)
library(plumber)
library(later)
library(jsonlite)
library(yaml)
library(httpuv)

reset <- function() {
  engine$stop_simulation()
  engine$shutdown()
  engine <- NULL
}

setwd("~/Library/CloudStorage/GoogleDrive-basar.erdivanli@erdogan.edu.tr/My Drive/R/Deneme")
`%||%` <- function(a, b) if (is.null(a)) b else a

reset()

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

engine$start_simulation()

engine$disconnect_patient_to_room_air()                     # breathing room air
engine$machine$vaporizer_bank$get_current_fi_agents()
engine$patient$systems$respiratory$get_fi("sevoflurane")
engine$patient$systems$respiratory$get_fA("sevoflurane")
engine$patient$systems$respiratory$get_Pa("sevoflurane")
engine$patient$systems$respiratory$PaCO2
engine$patient$systems$respiratory$arterial_o2
engine$machine$get_fresh_gas()

engine$connect_patient_to_machine_controlled()
engine$machine$vaporizer_bank$open_vaporizer("sevoflurane")
engine$machine$set_vaporizer_setting("sevoflurane", 3)
engine$machine$vaporizer_bank$get_current_fi_agents()
engine$patient$systems$respiratory$get_fi("sevoflurane")
engine$patient$systems$respiratory$get_fA("sevoflurane")
engine$patient$systems$respiratory$get_Pa("sevoflurane")
engine$patient$systems$respiratory$PaCO2
engine$patient$systems$respiratory$arterial_o2
engine$machine$get_fresh_gas()

engine$machine$close_vaporizer("sevoflurane")
engine$connect_patient_to_machine_manual_mask(mask_seal=0.2) # preoxygenation, leaky mask
engine$machine$set_o2_flow(6)                                # 6 L/min O2
engine$machine$vaporizer_bank$get_current_fi_agents()
engine$patient$systems$respiratory$get_fi("sevoflurane")
engine$patient$systems$respiratory$get_fA("sevoflurane")
engine$patient$systems$respiratory$get_Pa("sevoflurane")
engine$patient$systems$respiratory$PaCO2
engine$patient$systems$respiratory$arterial_o2
engine$machine$get_fresh_gas()

engine$machine$o2_flow <- 0.5
engine$machine$air_flow <- 0
engine$machine$frequency <- 8
engine$machine$get_fresh_gas()

engine$stop_simulation()


