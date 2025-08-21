ModeManualMask <- R6::R6Class(
  "ModeManualMask",
  inherit = Mode,
  public = list(
    label = "manual_mask",
    mask_seal = 0.3,
    patient_minute_vent_L_min = 6,
    extra_leak_L_min = 0,
    initialize = function(mask_seal = 0.3, patient_minute_vent_L_min = 6, extra_leak_L_min = 0) {
      self$mask_seal <- max(0, min(1, mask_seal))
      self$patient_minute_vent_L_min <- as.numeric(patient_minute_vent_L_min)
      self$extra_leak_L_min <- as.numeric(extra_leak_L_min)
    },
    .eff = function(machine) {
      # fraction of patient demand met by circuit gas (rest is room air)
      fgf <- machine$total_fresh_gas_flow()
      demand <- max(1e-6, self$patient_minute_vent_L_min)
      supplied <- max(0, fgf * self$mask_seal - self$extra_leak_L_min)
      max(0, min(1, supplied / demand))
    },
    get_fio2 = function(machine) {
      eff <- self$.eff(machine)
      eff * machine$current_fio2 + (1 - eff) * 0.21
    },
    get_current_fi_agents = function(machine) {
      eff <- self$.eff(machine)
      vol <- machine$vaporizer_bank$get_volatile_agent_composition()
      lapply(vol, function(x) eff * as.numeric(x))
    },
    
    get_minute_vent_L_min = function(machine) {
      # what you already store as 'patient_minute_vent_L_min'
      as.numeric(self$patient_minute_vent_L_min %||% 6)
    }
  )
)