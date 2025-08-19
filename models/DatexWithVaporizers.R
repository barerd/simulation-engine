DatexWithVaporizers <- R6::R6Class(
  "DatexWithVaporizers",
  inherit = Device,
  public = list(
    presets_path = NULL,
    vaporizer_bank = NULL,
    mode = NULL,  # pluggable behavior
    
    initialize = function(name, settings = list(), node_id = NULL, bus = NULL) {
      super$initialize(name = name, node_id = node_id, bus = bus)
      
      # Reuse an existing bank (e.g., same physical vaporizers) or create new
      if (!is.null(settings$reuse_vaporizer_bank)) {
        stopifnot(inherits(settings$reuse_vaporizer_bank, "R6"))
        self$vaporizer_bank <- settings$reuse_vaporizer_bank
      } else {
        self$vaporizer_bank <- VaporizerBank$new(
          parent_device = self,
          settings = settings,
          node_id = paste0(node_id, ".vaporizer_bank"),
          bus = bus
        )
      }
      
      # default presets path that works out of the box
      self$presets_path <- settings$presets_path %||% "machines/datex_ohmeda_s5_avance_presets.yml"
      
      # default mode = Manual Mask unless caller changes it
      self$mode <- DatexModeManualMask$new()
    },
    
    # --- Common delegates (write once, reuse everywhere) ---
    set_vaporizer_setting = function(agent, value) {
      self$vaporizer_bank$set_vaporizer_setting(agent, value)
    },
    open_vaporizer = function(agent) {
      self$vaporizer_bank$open_vaporizer(agent)
    },
    close_vaporizer = function(agent) {
      self$vaporizer_bank$close_vaporizer(agent)
      self$vaporizer_bank$set_vaporizer_setting(agent, 0)
    },
    get_vaporizer_status = function() {
      self$vaporizer_bank$get_vaporizer_status()
    },
    
    # --- mode control ---
    set_mode = function(mode_obj) {
      stopifnot(inherits(mode_obj, "R6"))
      self$mode <- mode_obj
      invisible(TRUE)
    },
    
    # These two make objects “GasSource”-compatible for lungs pulling locally
    get_current_fi_agents = function() {
      self$mode$get_current_fi_agents(self)
    },
    
    get_fio2 = function() {
      self$mode$get_fio2(self)
    },
    
    get_fresh_gas = function() {
      # default delegation to "mode" if present, else compute from own fields
      if (!is.null(self$mode) && is.function(self$mode$get_fresh_gas)) return(self$mode$get_fresh_gas())
      volatile_agents <- if (!is.null(self$vaporizer_bank) && is.function(self$vaporizer_bank$get_volatile_agent_composition))
        self$vaporizer_bank$get_volatile_agent_composition() else list()
      list(
        flow_rate = if (is.function(self$total_fresh_gas_flow)) self$total_fresh_gas_flow() else NA_real_,
        fio2 = tryCatch(self$get_fio2(), error = function(e) 0.21),
        fin2o = tryCatch(self$current_fin2o %||% 0, error = function(e) 0),
        fi_agents = volatile_agents
      )
    },
    
    get_flows = function() {
      o2  <- tryCatch(self$o2_flow,  error = function(e) NA_real_)
      air <- tryCatch(self$air_flow, error = function(e) NA_real_)
      n2o <- tryCatch(self$n2o_flow, error = function(e) NA_real_)
      total <- if (is.function(self$total_fresh_gas_flow)) self$total_fresh_gas_flow()
      else sum(c(o2, air, n2o), na.rm = TRUE)
      list(o2 = o2, air = air, n2o = n2o, total = total)
    },
    
    # --- Presets API ---
    set_presets_path = function(path) {
      stopifnot(is.character(path), nzchar(path))
      self$presets_path <- path
      invisible(TRUE)
    },
    
    .load_presets = function(path = NULL) {
      p <- path %||% self$presets_path
      if (is.null(p)) return(NULL)
      # normalize but don't force existence; we’ll check below
      p_norm <- tryCatch(normalizePath(p, mustWork = FALSE), error = function(e) p)
      if (!file.exists(p_norm)) return(NULL)
      y <- yaml::read_yaml(p_norm)
      y$presets %||% list()
    },
    
    list_presets = function(path = NULL) {
      pres <- self$.load_presets(path)
      if (is.null(pres) || !length(pres)) character(0) else names(pres)
    },
    
    apply_preset = function(name, path = NULL) {
      pres <- self$.load_presets(path)
      if (is.null(pres) || !length(pres)) stop("No presets file found (set presets_path or pass path)")
      if (!(name %in% names(pres))) stop(sprintf("Preset '%s' not found", name))
      cfg <- pres[[name]]
      
      # 1) Mode switch
      mode_name <- tolower(cfg$mode %||% "")
      params <- cfg$params %||% list()
      
      if (mode_name %in% c("manual_mask","manual","mask")) {
        # Fill defaults if not provided
        mm <- DatexModeManualMask$new(
          mask_seal = params$mask_seal %||% 0.3,
          patient_minute_vent_L_min = params$patient_minute_vent_L_min %||% 6,
          extra_leak_L_min = params$extra_leak_L_min %||% 0
        )
        self$set_mode(mm)
      } else if (mode_name %in% c("controlled","vent","volume","pressure")) {
        cm <- DatexModeControlled$new()  # add params handling here if needed later
        self$set_mode(cm)
      } else if (nzchar(mode_name)) {
        stop(sprintf("Unknown mode '%s' in preset '%s'", mode_name, name))
      }
      # if mode omitted, keep current mode
      
      # 2) Flows
      flows <- cfg$flows %||% list()
      if (!is.null(flows$o2))  self$set_o2_flow(as.numeric(flows$o2))
      if (!is.null(flows$air)) self$set_air_flow(as.numeric(flows$air))
      if (!is.null(flows$n2o)) self$set_n2o_flow(as.numeric(flows$n2o))
      
      # 3) Vaporizers
      vapors <- cfg$vapors %||% list()
      if (length(vapors)) {
        # close all first? your bank already enforces interlock on open;
        # we’ll just set dials and open/close based on value.
        for (ag in names(vapors)) {
          val <- as.numeric(vapors[[ag]])
          self$set_vaporizer_setting(ag, val)
          if (is.finite(val) && val > 0) self$open_vaporizer(ag) else self$close_vaporizer(ag)
        }
      }
      
      # Publish a snapshot so UI/clients see the change immediately
      if (is.function(self$publish_snapshot)) self$publish_snapshot()
      #invisible(TRUE)
      return(list(
        mode   = tryCatch(self$mode$label, error=function(e) NA_character_),
        flows  = list(o2 = self$o2_flow, air = self$air_flow, n2o = self$n2o_flow),
        fio2   = tryCatch(self$current_fio2, error=function(e) NA_real_),
        vapors = self$vaporizer_bank$get_vaporizer_status()
      ))
    },
    
    # concrete mode setters that replace self$mode and apply params
    set_mode_manual_mask = function(params = list()) {
      self$mode <- DatexModeManualMask$new()
      # known params: mask_seal, patient_minute_vent_L_min, extra_leak_L_min
      if (!is.null(params$mask_seal))               self$mode$mask_seal               <- as.numeric(params$mask_seal)
      if (!is.null(params$patient_minute_vent_L_min)) self$mode$patient_minute_vent_L_min <- as.numeric(params$patient_minute_vent_L_min)
      if (!is.null(params$extra_leak_L_min))        self$mode$extra_leak_L_min        <- as.numeric(params$extra_leak_L_min)
      invisible(TRUE)
    },
    
    set_mode_controlled = function(params = list()) {
      self$mode <- DatexModeControlled$new()
      # add controlled-mode params here as needed (e.g., frequency, tidal_volume, etc.)
      if (!is.null(params$frequency) && !is.null(self$frequency))   self$frequency   <- as.numeric(params$frequency)
      if (!is.null(params$tidal_volume) && !is.null(self$tidal_volume)) self$tidal_volume <- as.numeric(params$tidal_volume)
      invisible(TRUE)
    }
  )
)