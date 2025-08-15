DatexOhmedaS5Avance <- R6::R6Class(
  "DatexOhmedaS5Avance",
  inherit = Device,
  public = list(
    # ----------------- Smoothed “current/displayed” fractions -----------------
    current_fio2 = 0.21,              # fraction 0..1
    current_fin2o = 0.00,             # fraction 0..1
    current_fi_sevoflurane = 0.00,    # fraction 0..1 (vol % / 100)
    current_fi_desflurane = 0.00,
    current_fi_isoflurane = 0.00,
    
    # ----------------- Flow & ventilation knobs -----------------
    o2_flow  = 2.0,                   # L/min
    air_flow = 2.0,                   # L/min
    n2o_flow = 0.0,                   # L/min
    frequency = 12,                   # breaths/min
    tidal_volume = 500,               # mL
    
    # ----------------- Vaporizer bank (with interlock) -----------------
    # vaporizers is a named list keyed by agent name
    # Each item: list(vaporizer_setting=0..8 (vol%), is_open=FALSE, residual_decay_tau=60, response_tau=5)
    vaporizers = list(
      sevoflurane = list(vaporizer_setting = 0.0, is_open = FALSE, residual_decay_tau = 60, response_tau = 5),
      desflurane  = list(vaporizer_setting = 0.0, is_open = FALSE, residual_decay_tau = 60, response_tau = 5),
      isoflurane  = list(vaporizer_setting = 0.0, is_open = FALSE, residual_decay_tau = 60, response_tau = 5)
    ),
    
    # ----------------- Global smoothing for FiO2/FiN2O display -----------------
    fio2_response_tau = 5.0,          # seconds
    
    # ----------------- Cached deriveds -----------------
    cached_fresh_gas_flow = 4.0,      # L/min
    cached_minute_ventilation = 6.0,  # L/min
    
    initialize = function(name, settings = list(), node_id = NULL, bus = NULL) {
      # Device$initialize(name, topology_config = NULL, ...)
      super$initialize(name = name, topology_config = NULL, node_id = node_id, bus = bus)
      
      # Helper
      num <- function(k, d) if (!is.null(settings[[k]]) && is.finite(settings[[k]])) settings[[k]] else d
      
      # Apply flows & ventilation
      self$o2_flow        <- num("o2_flow",        self$o2_flow)
      self$air_flow       <- num("air_flow",       self$air_flow)
      self$n2o_flow       <- num("n2o_flow",       self$n2o_flow)
      self$frequency      <- num("frequency",      self$frequency)
      self$tidal_volume   <- num("tidal_volume",   self$tidal_volume)
      self$fio2_response_tau <- num("datex_tau",   self$fio2_response_tau)
      
      # Vaporizers from YAML (friendly keys)
      if (!is.null(settings$vaporizers) && is.list(settings$vaporizers)) {
        # merge provided fields into defaults agent-by-agent
        for (agent in names(settings$vaporizers)) {
          if (!(agent %in% names(self$vaporizers))) {
            # add unknown agent as-is
            self$vaporizers[[agent]] <- list(
              vaporizer_setting   = 0.0,
              is_open             = FALSE,
              residual_decay_tau  = 60,
              response_tau        = 5
            )
          }
          v <- self$vaporizers[[agent]]
          s <- settings$vaporizers[[agent]]
          v$vaporizer_setting  <- if (!is.null(s$vaporizer_setting)) as.numeric(s$vaporizer_setting) else v$vaporizer_setting
          v$is_open            <- if (!is.null(s$is_open)) as.logical(s$is_open) else v$is_open
          v$residual_decay_tau <- if (!is.null(s$residual_decay_tau)) as.numeric(s$residual_decay_tau) else v$residual_decay_tau
          v$response_tau       <- if (!is.null(s$response_tau)) as.numeric(s$response_tau) else v$response_tau
          self$vaporizers[[agent]] <- v
        }
      } else {
        # Back-compat: single initial setting + active_vaporizer at device root
        if (!is.null(settings$volatile_agent) && !is.null(settings$vaporizer_setting)) {
          agent <- as.character(settings$volatile_agent)
          if (!(agent %in% names(self$vaporizers))) {
            self$vaporizers[[agent]] <- list(vaporizer_setting=0,is_open=FALSE,residual_decay_tau=60,response_tau=5)
          }
          self$vaporizers[[agent]]$vaporizer_setting <- as.numeric(settings$vaporizer_setting)
          if (!is.null(settings$active_vaporizer)) {
            for (ag in names(self$vaporizers)) self$vaporizers[[ag]]$is_open <- (ag == settings$active_vaporizer)
          }
        }
      }
      
      # Prime display values from flows
      self$current_fio2  <- self$compute_fio2_from_flows()
      self$current_fin2o <- self$compute_fin2o_from_flows()
      # Prime agent Fi’s from their settings (if open) else 0
      self$current_fi_sevoflurane <- if (self$is_vaporizer_open("sevoflurane")) self$vaporizers$sevoflurane$vaporizer_setting/100 else 0
      self$current_fi_desflurane  <- if (self$is_vaporizer_open("desflurane"))  self$vaporizers$desflurane$vaporizer_setting/100  else 0
      self$current_fi_isoflurane  <- if (self$is_vaporizer_open("isoflurane"))  self$vaporizers$isoflurane$vaporizer_setting/100  else 0
      
      # Deriveds
      self$cached_fresh_gas_flow     <- self$total_fresh_gas_flow()
      self$cached_minute_ventilation <- self$minute_ventilation()
      
      if (!is.null(bus)) self$publish_snapshot()
    },
    
    # ----------------- Setters you can POST to -----------------
    set_o2_flow        = function(v) { self$o2_flow  <- max(0, as.numeric(v)); invisible(TRUE) },
    set_air_flow       = function(v) { self$air_flow <- max(0, as.numeric(v)); invisible(TRUE) },
    set_n2o_flow       = function(v) { self$n2o_flow <- max(0, as.numeric(v)); invisible(TRUE) },
    set_vaporizer_setting = function(agent, value) {
      agent <- tolower(as.character(agent))
      if (!(agent %in% names(self$vaporizers))) stop("Unknown vaporizer: ", agent)
      self$vaporizers[[agent]]$vaporizer_setting <- max(0, min(8, as.numeric(value)))
      invisible(TRUE)
    },
    open_vaporizer = function(agent) {
      agent <- tolower(as.character(agent))
      if (!(agent %in% names(self$vaporizers))) stop("Unknown vaporizer: ", agent)
      # interlock: close all others
      for (ag in names(self$vaporizers)) self$vaporizers[[ag]]$is_open <- FALSE
      self$vaporizers[[agent]]$is_open <- TRUE
      invisible(TRUE)
    },
    close_vaporizer = function(agent) {
      agent <- tolower(as.character(agent))
      if (!(agent %in% names(self$vaporizers))) stop("Unknown vaporizer: ", agent)
      self$vaporizers[[agent]]$is_open <- FALSE
      invisible(TRUE)
    },
    
    # ----------------- Per-tick update -----------------
    update = function(dt) {
      # Validate numeric fields first
      self$validate_numeric_fields()
      
      # Gas fractions from flows
      a_fio2 <- self$alpha(dt, self$fio2_response_tau)
      self$current_fio2  <- self$current_fio2  + (self$compute_fio2_from_flows()  - self$current_fio2)  * a_fio2
      self$current_fin2o <- self$current_fin2o + (self$compute_fin2o_from_flows() - self$current_fin2o) * a_fio2
      
      # Volatile agents: move toward setting if open; decay toward 0 if closed
      self$current_fi_sevoflurane <- self$update_agent_fraction(
        cur = self$current_fi_sevoflurane,
        agent = "sevoflurane",
        dt = dt
      )
      self$current_fi_desflurane <- self$update_agent_fraction(
        cur = self$current_fi_desflurane,
        agent = "desflurane",
        dt = dt
      )
      self$current_fi_isoflurane <- self$update_agent_fraction(
        cur = self$current_fi_isoflurane,
        agent = "isoflurane",
        dt = dt
      )
      
      # Deriveds for convenience
      self$cached_fresh_gas_flow     <- self$total_fresh_gas_flow()
      self$cached_minute_ventilation <- self$minute_ventilation()
      
      self$publish_snapshot()
      invisible(TRUE)
    },
    
    # ----------------- Pull APIs for the Patient -----------------
    get_fresh_gas = function() {
      list(
        flow_rate = self$total_fresh_gas_flow(),     # L/min
        fio2      = self$current_fio2,
        fin2o     = self$current_fin2o,
        fi_agents = list(
          sevoflurane = self$current_fi_sevoflurane,
          desflurane  = self$current_fi_desflurane,
          isoflurane  = self$current_fi_isoflurane
        )
      )
    },
    
    # ----------------- Helpers -----------------
    total_fresh_gas_flow = function() self$o2_flow + self$air_flow + self$n2o_flow,
    minute_ventilation   = function() (self$frequency * self$tidal_volume) / 1000,
    
    compute_fio2_from_flows = function() {
      tot <- self$total_fresh_gas_flow()
      if (tot <= 0) return(0.21)                     # default to room air
      (self$o2_flow + 0.21 * self$air_flow) / tot
    },
    compute_fin2o_from_flows = function() {
      tot <- self$total_fresh_gas_flow()
      if (tot <= 0) return(0.0)
      self$n2o_flow / tot
    },
    
    is_vaporizer_open = function(agent) {
      agent <- tolower(agent)
      !is.null(self$vaporizers[[agent]]) && isTRUE(self$vaporizers[[agent]]$is_open)
    },
    
    update_agent_fraction = function(cur, agent, dt) {
      agent <- tolower(agent)
      v <- self$vaporizers[[agent]]
      if (is.null(v)) return(0)
      if (isTRUE(v$is_open)) {
        # approach dialed setting (% → fraction)
        target <- (v$vaporizer_setting %||% 0) / 100
        a <- self$alpha(dt, v$response_tau %||% 5)
        return(cur + (target - cur) * a)
      } else {
        # residual washout toward 0
        a <- self$alpha(dt, v$residual_decay_tau %||% 60)
        return(cur + (0 - cur) * a)
      }
    },
    
    alpha = function(dt, tau) 1 - exp(-as.numeric(dt) / max(1e-6, as.numeric(tau))),
    
    publish_snapshot = function() {
      if (is.null(self$bus)) return(invisible())
      self$publish_params(list(
        device = self$name,
        flows  = list(o2 = self$o2_flow, air = self$air_flow, n2o = self$n2o_flow),
        fresh_gas_flow = self$cached_fresh_gas_flow,
        minute_ventilation = self$cached_minute_ventilation,
        fio2 = self$current_fio2,
        fin2o = self$current_fin2o,
        fi_agents = list(
          sevoflurane = self$current_fi_sevoflurane,
          desflurane  = self$current_fi_desflurane,
          isoflurane  = self$current_fi_isoflurane
        ),
        vaporizers = self$vaporizers
      ), notify = TRUE)
    },
    
    validate_numeric_fields = function() {
      # Ensure critical fields remain numeric
      numeric_fields <- c("current_fio2", "current_fin2o", "current_fi_sevoflurane", 
                          "current_fi_desflurane", "current_fi_isoflurane",
                          "o2_flow", "air_flow", "n2o_flow", "frequency", "tidal_volume")
      
      for (field in numeric_fields) {
        if (exists(field, self) && !is.numeric(self[[field]])) {
          # Try to convert back to numeric, or reset to safe default
          val <- suppressWarnings(as.numeric(self[[field]]))
          if (is.na(val)) {
            # Set safe defaults
            if (field == "current_fio2") val <- 0.21
            else if (field %in% c("o2_flow", "air_flow")) val <- 2.0
            else val <- 0.0
          }
          self[[field]] <- val
          cat("Warning: Reset", field, "to", val, "due to type corruption\n")
        }
      }
    }
  )
)