DatexControlledMode <- R6Class(
  "DatexControlledMode", 
  inherit = DatexWithVaporizers,
  public = list(
    # Keep gas mixing in main class (simplified approach)
    o2_flow = 2.0,
    air_flow = 2.0,
    n2o_flow = 0.0,
    current_fio2 = 0.21,
    current_fin2o = 0.0,
    fio2_response_tau = 5.0,
    
    # Other device parameters...
    frequency = 12,
    tidal_volume = 500,
    
    initialize = function(name, settings = list(), node_id = NULL, bus = NULL) {
      super$initialize(name = name, node_id = node_id, bus = bus)
      
      # Apply gas settings directly (no separate component)
      self$o2_flow <- settings$o2_flow %||% self$o2_flow
      self$air_flow <- settings$air_flow %||% self$air_flow
      self$n2o_flow <- settings$n2o_flow %||% self$n2o_flow
      self$fio2_response_tau <- settings$datex_tau %||% self$fio2_response_tau
      
      # Initialize gas fractions
      self$current_fio2 <- self$compute_fio2_from_flows()
      self$current_fin2o <- self$compute_fin2o_from_flows()
      
      cat("DatexOhmedaS5Avance initialized (using parent vaporizer bank)\n")
      if (!is.null(bus)) self$publish_snapshot()
    },
    
    # === GAS METHODS (kept in main class) ===
    set_o2_flow = function(v) { self$o2_flow <- max(0, as.numeric(v)); invisible(TRUE) },
    set_air_flow = function(v) { self$air_flow <- max(0, as.numeric(v)); invisible(TRUE) },
    set_n2o_flow = function(v) { self$n2o_flow <- max(0, as.numeric(v)); invisible(TRUE) },
    
    total_fresh_gas_flow = function() { self$o2_flow + self$air_flow + self$n2o_flow },
    
    compute_fio2_from_flows = function() {
      tot <- self$total_fresh_gas_flow()
      if (tot <= 0) return(0.21)
      (self$o2_flow + 0.21 * self$air_flow) / tot
    },
    
    compute_fin2o_from_flows = function() {
      tot <- self$total_fresh_gas_flow()
      if (tot <= 0) return(0.0)
      self$n2o_flow / tot
    },
    
    # === UPDATE ===
    update = function(dt) {
      # Update gas fractions (kept in main class)
      alpha <- 1 - exp(-dt / max(1e-6, self$fio2_response_tau))
      self$current_fio2 <- self$current_fio2 + (self$compute_fio2_from_flows() - self$current_fio2) * alpha
      self$current_fin2o <- self$current_fin2o + (self$compute_fin2o_from_flows() - self$current_fin2o) * alpha
      
      # Update vaporizer bank component
      self$vaporizer_bank$update(dt)
      
      # Publish composite snapshot
      self$publish_snapshot()
      invisible(TRUE)
    },
    
    # === COMPOSITE API ===
    get_fresh_gas = function() {
      volatile_agents <- self$vaporizer_bank$get_volatile_agent_composition()
      
      list(
        flow_rate = self$total_fresh_gas_flow(),
        fio2 = self$current_fio2,
        fin2o = self$current_fin2o,
        fi_agents = volatile_agents
      )
    },
    
    publish_snapshot = function() {
      if (is.null(self$bus)) return(invisible())
      
      volatile_agents <- self$vaporizer_bank$get_volatile_agent_composition()
      
      self$publish_params(list(
        device = self$name,
        flows = list(o2 = self$o2_flow, air = self$air_flow, n2o = self$n2o_flow),
        total_fresh_gas_flow = self$total_fresh_gas_flow(),
        fio2 = self$current_fio2,
        fin2o = self$current_fin2o,
        fi_agents = volatile_agents,  # This will be picked up by lungs
        vaporizer_bank_status = self$vaporizer_bank$get_bank_status(),
        frequency = self$frequency,
        tidal_volume = self$tidal_volume
      ))
    }
  )
)