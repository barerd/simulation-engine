CO2Absorber <- R6::R6Class(
  "CO2Absorber",
  inherit = DevicePart,
  public = list(
    canister_volume_L = 1.0,         # physical void volume contributing to internal volume
    position = "post",               # "pre" or "post" (relative to FGF entry into the circle)
    material = "SodaLime_G2",
    capacity_mmol = 50000,           # total stoichiometric capacity
    remaining_mmol = 50000,          # dynamic
    efficiency = 0.95,               # nominal removal fraction at nominal flow
    channeling = 0.10,               # 0..1; higher -> worse removal
    water_content = 0.15,            # affects activity a bit
    pressure_drop_cmH2O_per_Lps = 1.0,
    agent_adsorb_tau_s = 2.0,        # small lag for volatile agents (first-order)
    
    # internal memory for agent lag
    .last_agent_out = list(),
    
    initialize = function(name = "CO2Absorber",
                          parent_device = NULL,
                          node_id = NULL,
                          bus = NULL,
                          canister_volume_L = 1.0,
                          position = c("pre","post"),
                          material = "SodaLime_G2",
                          capacity_mmol = 50000,
                          efficiency = 0.95,
                          channeling = 0.10,
                          water_content = 0.15,
                          pressure_drop_cmH2O_per_Lps = 1.0,
                          agent_adsorb_tau_s = 2.0) {
      
      # Fix the match.arg call by providing choices explicitly
      position <- match.arg(tolower(position), choices = c("pre", "post"))
      
      super$initialize(name = name, parent_device = parent_device, node_id = node_id, bus = bus)
      self$canister_volume_L <- as.numeric(canister_volume_L)
      self$position <- position
      self$material <- material
      self$capacity_mmol <- as.numeric(capacity_mmol)
      self$remaining_mmol <- self$capacity_mmol
      self$efficiency <- as.numeric(efficiency)
      self$channeling <- as.numeric(channeling)
      self$water_content <- as.numeric(water_content)
      self$pressure_drop_cmH2O_per_Lps <- as.numeric(pressure_drop_cmH2O_per_Lps)
      self$agent_adsorb_tau_s <- as.numeric(agent_adsorb_tau_s)
    },
    
    # ---- glue methods expected by the base ---------------------------------
    # In a classic soda lime canister we do not change FGF composition; we act on
    # the circle gas when it passes the absorber (via scrub()).
    mix_fresh_gas = function(fgf) fgf,
    
    # Absorber is not a reflector; it doesn’t return agent by design.
    rebreathing_fraction = function(mode_label) 0,
    
    effective_internal_volume_L = function(mode_label) {
      # The canister void is part of the circle volume regardless of mode.
      self$canister_volume_L
    },
    
    # Expose a τ multiplier so other components (e.g., VaporizerBank) can
    # immediately respect absorber-related lags for volatile transport.
    # We only slow "response" (rise) and slightly speed "decay" (washout).
    tau_multiplier = function(agent, kind = c("response","decay")) {
      kind <- match.arg(kind)
      # small, bounded effect based on agent_adsorb_tau_s
      base <- max(0.5, min(3.0, 1.0 + self$agent_adsorb_tau_s / 6.0))
      if (kind == "response") return(base)           # slower rise
      return(1.0 / base)                             # slightly faster washout
    },
    
    summary_state = function() {
      list(
        type = "CO2Absorber",
        enabled = self$enabled,
        position = self$position,
        volume_L = self$canister_volume_L,
        material = self$material,
        remaining_mmol = self$remaining_mmol,
        capacity_mmol = self$capacity_mmol,
        effective_efficiency = self$efficiency * (1 - self$channeling),
        agent_adsorb_tau_s = self$agent_adsorb_tau_s
      )
    },
    
    # ---- Core operation on circle gas --------------------------------------
    # gas_in: named list of fractions (sum≈1), with a "CO2" (or "co2") entry and
    # possibly agent names like "sevoflurane".
    # flow_L_min: circle flow through the absorber (L/min).
    # dt: step (s).
    scrub = function(flow_L_min, gas_in, dt) {
      # defensiveness
      if (is.null(gas_in) || !length(gas_in)) return(list(
        gas_out = gas_in, dP = 0, removed_mmol = 0, remaining_mmol = self$remaining_mmol
      ))
      
      # locate CO2 key
      co2_key <- if ("CO2" %in% names(gas_in)) "CO2" else if ("co2" %in% names(gas_in)) "co2" else NULL
      co2_in  <- if (is.null(co2_key)) 0 else as.numeric(gas_in[[co2_key]])
      
      # molar flow approximation: 24 L ≈ 1 mol at room temp
      MOL_L <- 24.0
      total_L_this_step <- as.numeric(flow_L_min) * as.numeric(dt) / 60.0
      total_mol_this_step <- total_L_this_step / MOL_L
      co2_mmol_in <- 1000.0 * total_mol_this_step * max(0, co2_in)
      
      # effective removal fraction (bounded)
      water_boost <- 0.9 + 0.4 * max(0, min(1, self$water_content))  # 0.9..1.3
      eff <- max(0, min(1, self$efficiency * (1 - self$channeling) * water_boost))
      
      # candidate removal limited by remaining capacity
      removable_mmol <- eff * co2_mmol_in
      removed_mmol <- min(self$remaining_mmol, removable_mmol)
      self$remaining_mmol <- max(0, self$remaining_mmol - removed_mmol)
      
      # convert removed back to fraction at the outlet:
      co2_out <- if (total_mol_this_step > 0) {
        co2_in - (removed_mmol / 1000.0) / total_mol_this_step
      } else co2_in
      co2_out <- max(0, co2_out)
      
      # Start with pass-through, then modify CO2
      gas_out <- gas_in
      if (!is.null(co2_key)) gas_out[[co2_key]] <- co2_out
      
      # adsorption lag for volatile agents (low-pass)
      if (self$agent_adsorb_tau_s > 1e-6) {
        alpha <- 1 - exp(-as.numeric(dt) / self$agent_adsorb_tau_s)
        for (nm in names(gas_in)) {
          if (nm %in% c("O2","N2","AIR","CO2","co2","N2O","He","Ar")) next
          vin <- as.numeric(gas_in[[nm]])
          vprev <- as.numeric(self$.last_agent_out[[nm]] %||% vin)
          vout <- vprev + (vin - vprev) * alpha
          self$.last_agent_out[[nm]] <- vout
          gas_out[[nm]] <- vout
        }
      }
      
      # Renormalize other fractions when CO2 is removed/changed
      sum_in_wo_co2  <- max(1e-12, 1 - co2_in)
      sum_out_wo_co2 <- max(1e-12, 1 - co2_out)
      if (!is.null(co2_key)) {
        for (nm in names(gas_out)) {
          if (nm == co2_key) next
          gas_out[[nm]] <- as.numeric(gas_out[[nm]]) * (sum_out_wo_co2 / sum_in_wo_co2)
        }
      }
      
      # pressure drop across bed
      dP <- self$pressure_drop_cmH2O_per_Lps * (as.numeric(flow_L_min) / 60.0)
      
      list(
        gas_out = gas_out,
        dP = dP,
        removed_mmol = removed_mmol,
        remaining_mmol = self$remaining_mmol
      )
    }
  )
)