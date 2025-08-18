DatexManualMask <- R6::R6Class(
  "DatexManualMask",
  public = list(
    machine = NULL,                # your DatexOhmedaS5Avance instance
    mask_seal = 0.3,               # 0..1 (1 = perfect seal)
    patient_minute_vent_L_min = 6, # rough inspiratory demand
    extra_leak_L_min = 0,          # fixed leak (optional)
    initialize = function(machine, mask_seal = 0.3) {
      self$machine <- machine
      self$mask_seal <- max(0, min(1, mask_seal))
    },
    # (1) Inspired volatiles at the patient
    get_current_fi_agents = function() {
      # If mask is leaky, a portion of patient demand is room air (no volatiles)
      vol <- self$machine$vaporizer_bank$get_volatile_agent_composition()
      vol <- lapply(vol, as.numeric)
      eff <- self$._effective_delivery_fraction()  # fraction of demand met by circuit gas
      # Dilute agent fractions by entrained room air (0 agent in room air)
      lapply(vol, function(x) eff * x)
    },
    # (2) Inspired O2 at the patient (dilution by entrainment)
    get_fio2 = function() {
      circuit_fio2 <- self$machine$current_fio2
      eff <- self$._effective_delivery_fraction()
      eff * circuit_fio2 + (1 - eff) * 0.21
    },
    get_total_flow_L_min = function() self$machine$total_fresh_gas_flow(),
    # --- helpers ---
    ._effective_delivery_fraction = function() {
      # portion of patient inspiratory demand met by the circuit (rest is room air)
      fgf <- self$machine$total_fresh_gas_flow()   # L/min from machine
      demand <- max(1e-6, self$patient_minute_vent_L_min)
      supplied <- max(0, fgf * self$mask_seal - self$extra_leak_L_min)
      frac <- supplied / demand
      max(0, min(1, frac))
    }
  )
)