AnesthesiaMachineWithVaporizers <- R6::R6Class(
  "AnesthesiaMachineWithVaporizers",
  inherit = Device,
  public = list(
    presets_path = NULL,
    vaporizer_bank = NULL,
    mode = NULL,                 # pluggable behavior
    lungs_ref = NULL,            # optional reference used in update()
    parts = list(),
    fgf_inlet_position = "post_absorber",   # "pre_absorber" | "post_absorber"
    
    # --- Gas mixer (machine-level, independent of mode) ---
    o2_flow = 2.0,
    air_flow = 2.0,
    n2o_flow = 0.0,
    
    current_fio2 = 0.21,         # display/telemetry smoothing of FiO2
    current_fin2o = 0.0,         # display/telemetry smoothing of FiN2O
    fio2_response_tau = 5.0,     # seconds
    
    # ventilation knobs visible on machine (modes may override patient MV)
    frequency = 12,
    tidal_volume = 500,          # mL
    
    # trace gases (lower case keys except CO2 which is upper-case in streams)
    trace_fractions = list(
      CO2      = 0.0,  # mapped to "co2" in streams
      co       = 0.0,
      methane  = 0.0,
      alcohol  = 0.0,
      acetone  = 0.0,
      phosgene = 0.0
    ),
    
    initialize = function(name, settings = list(), node_id = NULL, bus = NULL) {
      super$initialize(name = name, node_id = node_id, bus = bus)
      
      self$parts <- list()
      self$fgf_inlet_position <- settings$fgf_inlet_position %||% "post_absorber"
      
      # Attach parts defined in YAML
      if (!is.null(settings$parts) && length(settings$parts)) {
        self$attach_parts_from_settings(settings$parts, node_id = node_id, bus = bus)
      }
      
      # Gas settings
      self$o2_flow <- settings$o2_flow %||% self$o2_flow
      self$air_flow <- settings$air_flow %||% self$air_flow
      self$n2o_flow <- settings$n2o_flow %||% self$n2o_flow
      self$fio2_response_tau <- settings$datex_tau %||% self$fio2_response_tau
      self$frequency <- settings$frequency %||% self$frequency
      self$tidal_volume <- settings$tidal_volume %||% self$tidal_volume
      
      # Optional trace gases from YAML (fractions)
      if (!is.null(settings$trace_gases) && is.list(settings$trace_gases)) {
        for (nm in names(settings$trace_gases)) {
          low <- tolower(nm)
          if (low %in% names(self$trace_fractions)) {
            self$trace_fractions[[low]] <- max(0, as.numeric(settings$trace_gases[[nm]]))
          }
        }
      }
      
      # Initialize displayed fractions from flows
      self$current_fio2  <- self$compute_fio2_from_flows()
      self$current_fin2o <- self$compute_fin2o_from_flows()
      
      if (!is.null(bus)) self$publish_snapshot()
      
      # Vaporizer bank
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
      
      self$presets_path <- settings$presets_path %||% "machines/datex_ohmeda_s5_avance_presets.yml"
      
      # default mode = Manual Mask unless caller changes it
      self$mode <- ModeManualMask$new()
    },
    
    # --- Gas mixer API ---
    set_o2_flow = function(v) { self$o2_flow <- max(0, as.numeric(v)); invisible(TRUE) },
    set_air_flow = function(v) { self$air_flow <- max(0, as.numeric(v)); invisible(TRUE) },
    set_n2o_flow = function(v) { self$n2o_flow <- max(0, as.numeric(v)); invisible(TRUE) },
    
    set_trace_gas_fraction = function(name, value) {
      nm <- tolower(as.character(name))
      if (!(nm %in% names(self$trace_fractions))) {
        stop(sprintf("Unknown trace gas '%s'", nm))
      }
      self$trace_fractions[[nm]] <- max(0, as.numeric(value))
      invisible(TRUE)
    },
    
    # produce a list for stream composition: all lower-case
    get_trace_fractions = function() {
      tf <- self$trace_fractions %||% list()
      out <- list(
        co2     = as.numeric(tf$co2 %||% 0),
        co      = as.numeric(tf$co  %||% 0),
        methane = as.numeric(tf$methane %||% 0),
        alcohol = as.numeric(tf$alcohol %||% 0),
        acetone = as.numeric(tf$acetone %||% 0),
        phosgene= as.numeric(tf$phosgene %||% 0)
      )
      # drop zeros so they don’t clutter streams
      out[ vapply(out, function(x) is.finite(x) && x > 0, logical(1)) ]
    },
    
    total_fresh_gas_flow = function() {
      o2  <- suppressWarnings(as.numeric(self$o2_flow  %||% 0))
      air <- suppressWarnings(as.numeric(self$air_flow %||% 0))
      n2o <- suppressWarnings(as.numeric(self$n2o_flow %||% 0))
      o2 + air + n2o
    },
    
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
    
    # --- Vaporizer delegates ---
    set_vaporizer_setting = function(agent, value) self$vaporizer_bank$set_vaporizer_setting(agent, value),
    open_vaporizer = function(agent) self$vaporizer_bank$open_vaporizer(agent),
    close_vaporizer = function(agent) { self$vaporizer_bank$close_vaporizer(agent); self$vaporizer_bank$set_vaporizer_setting(agent, 0) },
    get_vaporizer_status = function() self$vaporizer_bank$get_vaporizer_status(),
    
    # --- Mode control ---
    set_mode = function(mode_obj) { stopifnot(inherits(mode_obj, "R6")); self$mode <- mode_obj; invisible(TRUE) },
    
    # GasSource interface for lungs (delegated to mode)
    get_current_fi_agents = function() self$mode$get_current_fi_agents(self),
    get_fio2 = function() self$mode$get_fio2(self),
    
    # What the patient actually receives (inspiratory stream)
    get_fresh_gas = function() {
      insp <- self$streams$inspiratory %||% make_stream(0, list())
      fr   <- insp$fr %||% list()
      
      fio2  <- as.numeric(fr$O2  %||% self$compute_fio2_from_flows())
      fin2o <- as.numeric(fr$N2O %||% 0)
      fico2 <- as.numeric(fr$CO2 %||% 0)
      
      # Volatiles: clamp to inspiratory if present
      agents <- if (!is.null(self$vaporizer_bank)) self$vaporizer_bank$get_volatile_agent_composition() else list()
      for (ag in names(agents)) {
        agents[[ag]] <- as.numeric(fr[[ag]] %||% agents[[ag]] %||% 0)
      }
      
      traces <- list(
        alcohol  = as.numeric(fr$alcohol  %||% 0),
        methane  = as.numeric(fr$methane  %||% 0),
        acetone  = as.numeric(fr$acetone  %||% 0),
        co       = as.numeric(fr$co       %||% 0),
        phosgene = as.numeric(fr$phosgene %||% 0)
      )
      
      list(
        flow_rate = self$total_fresh_gas_flow(),
        fio2      = fio2,
        fin2o     = fin2o,
        fi_agents = agents,
        fico2     = fico2,
        trace     = traces
      )
    },
    
    # Read CO2 fraction from a stream-like list (with $fr)
    .read_co2_from_stream = function(st) {
      if (is.null(st) || is.null(st$fr)) return(0)
      fr <- st$fr
      as.numeric(fr$CO2 %||% fr$co2 %||% 0)
    },
    
    # FiCO2 delivered to the patient (Y-piece, after absorber/hold-up)
    get_fico2 = function() {
      self$.read_co2_from_stream(self$streams$inspiratory)
    },
    
    # (Optional) CO2 fraction in raw fresh gas (should be ~0)
    get_fico2_fgf = function() {
      self$.read_co2_from_stream(self$streams$fgf)
    },
    
    get_flows = function() {
      list(o2 = self$o2_flow, air = self$air_flow, n2o = self$n2o_flow,
           total = self$total_fresh_gas_flow())
    },
    
    # --- Presets API (unchanged shape) ---
    set_presets_path = function(path) { stopifnot(is.character(path), nzchar(path)); self$presets_path <- path; invisible(TRUE) },
    .load_presets = function(path = NULL) {
      p <- path %||% self$presets_path
      if (is.null(p)) return(NULL)
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
        mm <- ModeManualMask$new(
          mask_seal = params$mask_seal %||% 0.3,
          patient_minute_vent_L_min = params$patient_minute_vent_L_min %||% 6,
          extra_leak_L_min = params$extra_leak_L_min %||% 0
        )
        self$set_mode(mm)
      } else if (mode_name %in% c("controlled","vent","volume","pressure")) {
        cm <- ModeControlled$new()
        self$set_mode(cm)
      } else if (nzchar(mode_name)) {
        stop(sprintf("Unknown mode '%s' in preset '%s'", mode_name, name))
      }
      
      # 2) Flows
      flows <- cfg$flows %||% list()
      if (!is.null(flows$o2))  self$set_o2_flow(as.numeric(flows$o2))
      if (!is.null(flows$air)) self$set_air_flow(as.numeric(flows$air))
      if (!is.null(flows$n2o)) self$set_n2o_flow(as.numeric(flows$n2o))
      
      # 3) Vaporizers
      vapors <- cfg$vapors %||% list()
      if (length(vapors)) {
        for (ag in names(vapors)) {
          val <- as.numeric(vapors[[ag]])
          self$set_vaporizer_setting(ag, val)
          if (is.finite(val) && val > 0) self$open_vaporizer(ag) else self$close_vaporizer(ag)
        }
      }
      
      if (is.function(self$publish_snapshot)) self$publish_snapshot()
      return(list(
        mode   = tryCatch(self$mode$label, error=function(e) NA_character_),
        flows  = list(o2 = self$o2_flow, air = self$air_flow, n2o = self$n2o_flow),
        fio2   = tryCatch(self$current_fio2, error=function(e) NA_real_),
        vapors = self$vaporizer_bank$get_vaporizer_status()
      ))
    },
    
    # concrete mode setters
    set_mode_manual_mask = function(params = list()) {
      self$mode <- ModeManualMask$new()
      if (!is.null(params$mask_seal))                 self$mode$mask_seal                 <- as.numeric(params$mask_seal)
      if (!is.null(params$patient_minute_vent_L_min)) self$mode$patient_minute_vent_L_min <- as.numeric(params$patient_minute_vent_L_min)
      if (!is.null(params$extra_leak_L_min))          self$mode$extra_leak_L_min          <- as.numeric(params$extra_leak_L_min)
      invisible(TRUE)
    },
    
    set_mode_controlled = function(params = list()) {
      self$mode <- ModeControlled$new()
      if (!is.null(params$frequency) && !is.null(self$frequency))       self$frequency   <- as.numeric(params$frequency)
      if (!is.null(params$tidal_volume) && !is.null(self$tidal_volume)) self$tidal_volume <- as.numeric(params$tidal_volume)
      invisible(TRUE)
    },
    
    # --- Parts management -------------------------------------------------
    attach_parts_from_settings = function(parts_spec, node_id = NULL, bus = NULL) {
      stopifnot(is.list(parts_spec))
      for (nm in names(parts_spec)) self$attach_part(nm, parts_spec[[nm]], node_id = node_id, bus = bus)
      invisible(TRUE)
    },
    
    attach_part = function(name, spec, node_id = NULL, bus = NULL) {
      typ  <- spec$type %||% spec$class %||% stop("Part spec missing 'type'")
      ctor <- get0(typ, envir = .GlobalEnv, inherits = TRUE, ifnotfound = NULL)
      if (is.null(ctor) || !inherits(ctor, "R6ClassGenerator")) {
        stop(sprintf("Part class '%s' is not loaded. Source its file before creating the machine.", typ))
      }
      
      # base args
      args <- list(
        name = name,
        parent_device = self,
        node_id = paste0(node_id %||% self$node_id, ".", name),
        bus = bus
      )
      
      # keep only extras that the class initialize() actually accepts
      extras <- spec[setdiff(names(spec), c("type","class"))]
      init_formals <- names(formals(ctor$public_methods$initialize))
      if (!is.null(init_formals)) {
        keep <- intersect(names(extras), init_formals)
        if (length(keep)) args[keep] <- extras[keep]
      }
      
      inst <- do.call(ctor$new, args)
      self$parts[[name]] <- inst
      invisible(TRUE)
    },
    
    list_parts = function() names(self$parts),
    
    parts_effects = function() {
      mode_label <- tryCatch(self$mode$label, error = function(e) "unknown")
      v <- 0; rb <- 0; mult <- list()
      for (p in self$parts) {
        v  <- v  + suppressWarnings(as.numeric(tryCatch(p$effective_internal_volume_L(mode_label), error = function(e) 0)))
        rb <- rb + suppressWarnings(as.numeric(tryCatch(p$rebreathing_fraction(mode_label),        error = function(e) 0)))
        tm <- tryCatch(p$tau_multipliers(), error = function(e) list())
        if (length(tm)) for (k in names(tm)) mult[[k]] <- (mult[[k]] %||% 1.0) * as.numeric(tm[[k]])
      }
      rb <- max(0, min(1, rb))
      list(effective_internal_volume_L = v, rebreathing_fraction = rb, tau_multipliers = mult)
    },
    
    get_tau_multiplier = function(agent) {
      pe <- self$parts_effects()
      as.numeric(pe$tau_multipliers[[agent]] %||% 1.0)
    },
    
    get_compartments_snapshot = function() {
      mode_label <- tryCatch(self$mode$label, error = function(e) NA_character_)
      pe <- self$parts_effects()
      parts_state <- list()
      for (nm in names(self$parts)) {
        parts_state[[nm]] <- tryCatch(self$parts[[nm]]$summary_state(),
                                      error = function(e) list(type = class(self$parts[[nm]])[1], error = e$message))
      }
      list(
        mode = mode_label,
        fgf_inlet_position = self$fgf_inlet_position,
        effective_internal_volume_L = pe$effective_internal_volume_L,
        rebreathing_fraction = pe$rebreathing_fraction,
        parts = parts_state
      )
    },
    
    # --- Streams / probes -------------------------------------------------
    streams = list(
      fgf = make_stream(0, list()),
      pre_absorber = make_stream(0, list()),
      post_absorber = make_stream(0, list()),
      inspiratory = make_stream(0, list()),
      exhaled = make_stream(0, list()),
      circle_mix = make_stream(0, list())
    ),
    
    # Helper: compute τ from total effective internal volume / flow
    tau_from_volume = function(volume_L) {
      flow_Lps <- (self$total_fresh_gas_flow() %||% 0) / 60
      if (flow_Lps <= 0) return(5)
      volume_L / max(1e-6, flow_Lps)
    },
    
    get_stream = function(which = c("fgf","pre_absorber","post_absorber","inspiratory","exhaled","circle")) {
      which <- match.arg(which)
      switch(which,
             fgf = self$streams$fgf,
             pre_absorber = self$streams$pre_absorber,
             post_absorber = self$streams$post_absorber,
             inspiratory = self$streams$inspiratory,
             exhaled = self$streams$exhaled,
             circle = self$streams$circle_mix
      )
    },
    
    # --- Ventilation derivations (published on the bus) ---
    deadspace_frac = 0.30,  # machine-side assumption; can be overridden by YAML or Mode
    minute_vent_L_min = function() {
      if (!is.null(self$mode) && is.function(self$mode$get_minute_vent_L_min)) {
        return(as.numeric(self$mode$get_minute_vent_L_min(self)))
      }
      as.numeric(self$frequency) * (as.numeric(self$tidal_volume)/1000)
    },
    alveolar_vent_L_min = function() {
      mv <- self$minute_vent_L_min()
      ds <- suppressWarnings(as.numeric(self$deadspace_frac))
      mv * max(0, 1 - ds)
    },
    
    update = function(dt) {
      # 0) Vaporizer dynamics
      if (!is.null(self$vaporizer_bank)) self$vaporizer_bank$update(dt)
      
      # 1) Build FGF (O2/N2O + volatiles + trace gases; back-fill with N2)
      agent_fr   <- if (!is.null(self$vaporizer_bank)) self$vaporizer_bank$get_volatile_agent_composition() else list()
      trace      <- self$get_trace_fractions()
      base_total <- self$total_fresh_gas_flow() %||% 0
      
      fio2  <- self$compute_fio2_from_flows()
      fin2o <- self$compute_fin2o_from_flows()
      
      # pre-sum without nitrogen
      sum_nonN2 <- fio2 + fin2o + sum(unlist(agent_fr), na.rm = TRUE) + sum(unlist(trace), na.rm = TRUE)
      
      # assign nitrogen as the remainder (never negative)
      N2 <- max(0, 1 - sum_nonN2)
      
      fgf_fr <- c(list(O2 = fio2, N2O = fin2o, N2 = N2), agent_fr, trace)
      
      # gentle renormalization if numeric drift pushed total off 1
      s <- sum(unlist(fgf_fr), na.rm = TRUE)
      if (is.finite(s) && s > 0 && abs(s - 1) > 1e-12) {
        for (k in names(fgf_fr)) fgf_fr[[k]] <- as.numeric(fgf_fr[[k]]) / s
      }
      
      self$streams$fgf <- make_stream(base_total, fgf_fr)
      
      # 2) FGF placement (pre or post absorber)
      pre_mix  <- self$streams$circle_mix
      if (identical(self$fgf_inlet_position, "pre_absorber")) {
        self$streams$pre_absorber  <- mix_streams(list(pre_mix, self$streams$fgf))
        self$streams$post_absorber <- self$streams$pre_absorber
      } else {
        self$streams$pre_absorber  <- pre_mix
        self$streams$post_absorber <- mix_streams(list(pre_mix, self$streams$fgf))
      }
      
      # 3) Inspiratory stream (hold‑up from parts)
      total_holdup_L <- 0
      if (length(self$parts)) {
        for (p in self$parts) {
          total_holdup_L <- total_holdup_L + suppressWarnings(as.numeric(tryCatch(
            p$effective_internal_volume_L(self$mode$label), error = function(e) 0)))
        }
      }
      tau_vol <- self$tau_from_volume(total_holdup_L)
      self$streams$inspiratory <- lowpass_stream(self$streams$inspiratory, self$streams$post_absorber, tau_vol, dt)
      
      # 4) Lungs → exhaled stream (if connected)
      if (!is.null(self$lungs_ref) && is.function(self$lungs_ref$step_with_inspiratory)) {
        self$streams$exhaled <- self$lungs_ref$step_with_inspiratory(self$streams$inspiratory, dt)
      }
      
      # 5) CO2 absorber / reflector on the return path
      circ_return <- self$streams$exhaled
      absorber <- self$parts[["CO2Absorber"]] %||% self$parts[["co2_absorber"]]
      if (!is.null(absorber) && is.function(absorber$scrub)) {
        scr <- absorber$scrub(flow_L_min = circ_return$flow_L_min, gas_in = circ_return$fr, dt = dt)
        circ_return <- make_stream(self$streams$exhaled$flow_L_min, scr$gas_out)
      }
      refl <- self$parts[["Reflector"]] %||% self$parts[["reflector"]]
      if (!is.null(refl) && is.function(refl$recycle)) {
        circ_return <- refl$recycle(circ_return, dt)
      }
      
      # 6) Update circle mix
      self$streams$circle_mix <- lowpass_stream(self$streams$circle_mix, circ_return, tau_vol, dt)
      
      # 7) Update smoothed display FiO2/FiN2O from inspiratory stream
      a <- 1 - exp(-as.numeric(dt) / max(1e-6, as.numeric(self$fio2_response_tau)))
      insp_fr <- self$streams$inspiratory$fr %||% list()
      live_fio2  <- as.numeric(insp_fr$O2  %||% self$current_fio2)
      live_fin2o <- as.numeric(insp_fr$N2O %||% self$current_fin2o)
      self$current_fio2  <- self$current_fio2  + (live_fio2  - self$current_fio2)  * a
      self$current_fin2o <- self$current_fin2o + (live_fin2o - self$current_fin2o) * a
      
      # 8) Broadcast
      if (is.function(self$publish_snapshot)) self$publish_snapshot()
      invisible(TRUE)
    },
    
    publish_snapshot = function() {
      if (is.null(self$bus)) return(invisible())
      volatile_agents <- tryCatch(self$vaporizer_bank$get_volatile_agent_composition(), error=function(e) list())
      self$publish_params(list(
        device = self$name,
        flows = list(o2 = self$o2_flow, air = self$air_flow, n2o = self$n2o_flow),
        total_fresh_gas_flow = self$total_fresh_gas_flow(),
        fio2 = self$current_fio2,
        fin2o = self$current_fin2o,
        fi_agents = volatile_agents,
        mode = tryCatch(self$mode$label, error=function(e) NA_character_),
        frequency = self$frequency,
        tidal_volume = self$tidal_volume
      ))
      self$bus$set_param(self$node_id, "MV_L_min", self$minute_vent_L_min(), notify = TRUE)
      self$bus$set_param(self$node_id, "VA_L_min", self$alveolar_vent_L_min(), notify = TRUE)
      self$bus$set_param(self$node_id, "deadspace_frac", self$deadspace_frac, notify = TRUE)
      invisible(TRUE)
    }
  )
)