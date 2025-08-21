Lungs <- R6Class(
  "Lungs",
  inherit = Organ,
  public = list(
    gas_source = NULL,
    # Environment & constants
    PB = 760,     # barometric pressure (mmHg)
    PH2O = 47,    # water vapor pressure (mmHg)
    RQ = 0.8,     # respiratory quotient
    K_CO2 = 0.863,    # mmHg · L / (mL); used in PACO2 = 0.863 * VCO2 / VA
    
    # CO2 state (alveolar & arterial)
    PACO2 = NA_real_,        # alveolar CO2 partial pressure (mmHg)
    target_paco2 = NA_real_, # target alveolar PACO2 from VCO2 & VA
    PaCO2 = 40,              # arterial CO2 partial pressure (mmHg)
    
    # O2 state
    alveolar_po2 = NA_real_, # current alveolar PO2 (mmHg)
    target_pao2 = NA_real_,  # target alveolar PO2 from FiO2 & PaCO2
    arterial_o2 = NA_real_,  # arterial PO2 (PaO2) in mmHg
    
    # Dynamics
    tau_seconds = 3.0,     # time constant for approach (ventilation-driven)
    tau_co2_alv_sec  = 5.0,   # alveolar CO2 mixing time constant (s)
    tau_co2_blood_sec= 10.0,  # blood equilibration time constant (s)
    
    # Pathophysiology parameters
    shunt_fraction = 0.05,    # normal physiological shunt (5%)
    vq_mismatch = 0.1,        # V/Q mismatch factor (0 = perfect, 1 = severe)
    compliance_factor = 1.0,   # lung compliance factor (1.0 = normal, <1 = reduced)
    
    # Inputs remembered
    last_fio2 = NA_real_,  # fraction [0..1]
    last_vco2_ml_min = NA_real_, # from MetabolicSystem (mL/min)
    last_vo2_ml_min  = NA_real_, # optional; to recompute RQ if present
    
    # Ventilation
    VA_L_min = 4.2,          # alveolar ventilation (L/min) if no ventilator yet

    # Current inspired fractions (what reaches the lungs, after mixing/delays)
    Fi_agents = list(),         # e.g., list(sevoflurane = 0.018, desflurane = 0.0)
    
    # Airway/Proximal mixed inspired concentration (low-pass of Fi)
    FA_agents  = list(),   # "airway" stage: approaches Fi with tau_agent_fi_sec
    
    # Current alveolar fractions (equilibrated with blood)
    Fa_agents = list(),         # e.g., list(sevoflurane = 0.015, desflurane = 0.0)
    
    # Optional: Arterial concentration (low-pass of Fa)
    Pa_agents  = list(),   # "arterial" stage: approaches Fa with tau_agent_pa_sec
    
    # Time constants for each agent (can be agent-specific)
    tau_agent_fi_sec = 8,       # proximal/airway mixing to “what actually reaches alveoli"
    tau_agent_fa_sec = 15,      # alveolo-arteriolar equilibration
    tau_agent_pa_sec = 20,   # Fa -> Pa (arterial)
    
    # Add params field to avoid locked environment error
    params = NULL,
    
    # configurable (or from patient YAML)
    VDaw_mL = 150,           # anatomic + apparatus dead space (mL)
    phase2_k = 12,           # higher = steeper phase II (unitless)
    phase3_slope = 0.25,     # mmHg per %VT, crude linear slope in phase III
    
    last_etco2_mmHg = NA_real_,
    last_vco2_ml_per_breath = NA_real_,
    
    # convenience ETCO2 getter
    get_etco2_mmHg = function() {
      x <- self$last_etco2_mmHg %||% NA_real_
      if (is.na(x)) x <- as.numeric(self$PACO2 %||% NA_real_)
      x
    },
    
    get_etco2_kPa = function() {
      self$get_etco2_mmHg() * 0.133322
    },
    
    initialize = function(name = "lungs", patient = NULL, config = list(),
                          node_id = NULL, bus = NULL) {
      # Initialize params first
      self$params <- list()
      
      super$initialize(name = name, patient = patient, config = config,
                       node_id = node_id, bus = bus)
      
      if (is.null(self$bus) && !is.null(bus)) self$connect_bus(bus)
      
      if (!is.null(config$tau_seconds))  self$tau_seconds  <- config$tau_seconds
      if (!is.null(config$RQ))           self$RQ           <- config$RQ
      if (!is.null(config$VA_L_min))     self$VA_L_min     <- config$VA_L_min
      if (!is.null(config$tau_co2_alv_sec))   self$tau_co2_alv_sec   <- config$tau_co2_alv_sec
      if (!is.null(config$tau_co2_blood_sec)) self$tau_co2_blood_sec <- config$tau_co2_blood_sec
      if (!is.null(config$metabolic_scope))   self$metabolic_scope   <- config$metabolic_scope
      if (!is.null(config$tau_agent_fi_sec))  self$tau_agent_fi_sec  <- config$tau_agent_fi_sec
      if (!is.null(config$tau_agent_fa_sec))  self$tau_agent_fa_sec  <- config$tau_agent_fa_sec
      if (!is.null(config$tau_agent_pa_sec))  self$tau_agent_pa_sec  <- config$tau_agent_pa_sec
      
      if (!is.null(config$alveolar_po2)) self$alveolar_po2 <- config$alveolar_po2
      if (!is.null(config$arterial_o2))  self$arterial_o2  <- config$arterial_o2
      
      # Pathophysiology parameters
      if (!is.null(config$shunt_fraction))    self$shunt_fraction    <- config$shunt_fraction
      if (!is.null(config$vq_mismatch))       self$vq_mismatch       <- config$vq_mismatch
      if (!is.null(config$compliance_factor)) self$compliance_factor <- config$compliance_factor
      
      if (!is.null(config$alveolar_po2))       self$alveolar_po2       <- config$alveolar_po2
      if (!is.null(config$arterial_o2))       self$arterial_o2       <- config$arterial_o2
      
      # Metabolism (optional)
      if (!is.null(self$bus)) {
        self$subscribe(
          pattern = "^state/.*/vco2_ml_min$",
          callback = function(topic, msg) {
            self$last_vco2_ml_min <- as.numeric(if (is.list(msg) && !is.null(msg$value)) msg$value else msg)
            self$compute_targets(); self$publish_outputs()
          },
          replay = TRUE
        )
        self$subscribe(
          pattern = "^state/.*/vo2_ml_min$",
          callback = function(topic, msg) {
            self$last_vo2_ml_min <- as.numeric(if (is.list(msg) && !is.null(msg$value)) msg$value else msg)
            if (is.finite(self$last_vco2_ml_min) && is.finite(self$last_vo2_ml_min) && self$last_vo2_ml_min > 0)
              self$RQ <- self$last_vco2_ml_min / self$last_vo2_ml_min
            self$compute_targets(); self$publish_outputs()
          },
          replay = TRUE
        )
      }
      
      # Ventilation subscriptions (from machine)
      if (!is.null(self$bus)) {
        self$subscribe(
          pattern = "^state/.*/VA_L_min$",
          callback = function(topic, msg) {
            self$VA_L_min <- as.numeric(if (is.list(msg) && !is.null(msg$value)) msg$value else msg)
            self$compute_targets(); self$publish_outputs()
          },
          replay = TRUE
        )
        self$subscribe(
          pattern = "^state/.*/deadspace_frac$",
          callback = function(topic, msg) {
            # keep locally if you want to expose it or use it in later formulas
            self$set_local_param("deadspace_frac", as.numeric(if (is.list(msg) && !is.null(msg$value)) msg$value else msg), TRUE)
          },
          replay = TRUE
        )
      }
      
      # Seed defaults
      if (is.na(self$last_vco2_ml_min)) {
        self$last_vco2_ml_min <- 250 * self$RQ  # default VO2=250 → VCO2 via RQ
      }
      self$compute_targets()
      
      if (is.na(self$PACO2) && is.finite(self$target_paco2)) self$PACO2 <- self$target_paco2
      if (is.na(self$alveolar_po2) && is.finite(self$target_pao2)) self$alveolar_po2 <- self$target_pao2
      if (is.finite(self$alveolar_po2)) self$arterial_o2 <- self$compute_arterial_po2(self$alveolar_po2)
      if (is.na(self$PACO2)) self$PACO2 <- self$PaCO2
      if (is.na(self$target_paco2)) self$target_paco2 <- self$PaCO2
      
      if (is.null(self$node_id)) {
        base <- if (!is.null(self$patient) && !is.null(self$patient$name)) self$patient$name else "patient"
        self$node_id <- paste0(base, ".lungs")
      }
      
      self$publish_outputs()
    },
    
    set_local_param = function(key, value, notify = TRUE) {
      # Initialize params if NULL
      if (is.null(self$params)) {
        self$params <- list()
      }
      
      # Prefer publishing to the bus under this node's scope
      if (!is.null(self$bus) && !is.null(self$node_id)) {
        return(invisible(self$bus$set_param(self$node_id, key, value, notify = notify)))
      }
      
      # Fallback: keep a local field so the value is still visible if no bus
      self$params[[key]] <- value
      invisible(TRUE)
    },
    
    # --- plug/unplug inspired gas source ---
    set_gas_source = function(src) { self$gas_source <- src; invisible(TRUE) },
    clear_gas_source = function()   { self$gas_source <- NULL; invisible(TRUE) },
    
    # pull FiO2 + volatiles (and any other ambient gases) from local gas_source
    pull_from_gas_source = function() {
      if (is.null(self$gas_source)) return(invisible())
      
      # 1) Ambient non-volatile fractions (if provided)
      #    Design: any of these getters are optional.
      if (is.function(self$gas_source$get_fio2)) {
        self$last_fio2 <- as.numeric(self$gas_source$get_fio2())
      }
      # You can later read others similarly if/when you use them:
      # if (is.function(self$gas_source$get_fin2o))  self$last_fin2o  <- as.numeric(self$gas_source$get_fin2o())
      # if (is.function(self$gas_source$get_fico2))  self$last_fico2  <- as.numeric(self$gas_source$get_fico2())
      # if (is.function(self$gas_source$get_co))     self$last_fico   <- as.numeric(self$gas_source$get_co())
      
      # 2) Volatile agents
      if (is.function(self$gas_source$get_current_fi_agents)) {
        val <- self$gas_source$get_current_fi_agents()
        if (is.list(val) && length(val)) {
          self$Fi_agents <- lapply(val, as.numeric)
          # make sure downstream stages exist
          for (ag in names(self$Fi_agents)) {
            if (is.null(self$FA_agents[[ag]])) self$FA_agents[[ag]] <- 0.0
            if (is.null(self$Fa_agents[[ag]])) self$Fa_agents[[ag]] <- 0.0
            if (is.null(self$Pa_agents[[ag]])) self$Pa_agents[[ag]] <- 0.0
          }
        } else {
          # no agents from source -> keep (or clear) Fi_agents; choose to clear to avoid “residual rise”
          self$Fi_agents <- list()
        }
      }
      invisible(TRUE)
    },
      
    # ---------- Targets (O2/CO2) ----------
    compute_targets = function() {
      self$compute_co2_target()
      self$compute_o2_target()
      invisible(TRUE)
    },
    
    compute_co2_target = function() {
      vco2 <- self$last_vco2_ml_min; VA <- self$VA_L_min
      if (!is.finite(vco2) || !is.finite(VA) || VA <= 0) {
        if (is.na(self$target_paco2)) self$target_paco2 <- self$PaCO2
        return(invisible(FALSE))
      }
      self$target_paco2 <- self$K_CO2 * (vco2 / VA)
      invisible(TRUE)
    },
    
    compute_o2_target = function() {
      fio2 <- self$last_fio2
      if (is.na(fio2)) return(invisible(FALSE))
      if (fio2 > 1) fio2 <- fio2 / 100
      self$target_pao2 <- (self$PB - self$PH2O) * fio2 - (self$PaCO2 / self$RQ)
      invisible(TRUE)
    },
    
    compute_arterial_po2 = function(pao2_alv) {
      if (!is.finite(pao2_alv)) return(NA_real_)
      pvo2 <- 40
      shunt_mix <- pao2_alv * (1 - self$shunt_fraction) + pvo2 * self$shunt_fraction
      vq_eff <- 1 - self$vq_mismatch * 0.3
      comp_eff <- 0.7 + 0.3 * self$compliance_factor
      max(30, shunt_mix * vq_eff * comp_eff)
    },
    
    # ---------- Tick ----------
    update = function(dt = 1) {
      # 0) Always pull the fresh inspired gas picture from the configured source
      self$pull_from_gas_source()
      
      # 1) O2/CO2 dynamics
      self$compute_targets()
      
      if (is.finite(self$target_paco2)) {
        a_alv <- 1 - exp(-dt / max(self$tau_co2_alv_sec, 1e-6))
        if (is.na(self$PACO2)) self$PACO2 <- self$target_paco2
        self$PACO2 <- self$PACO2 + (self$target_paco2 - self$PACO2) * a_alv
      }
      if (is.finite(self$PACO2)) {
        a_bld <- 1 - exp(-dt / max(self$tau_co2_blood_sec, 1e-6))
        self$PaCO2 <- self$PaCO2 + (self$PACO2 - self$PaCO2) * a_bld
      }
      
      self$compute_o2_target()
      if (is.finite(self$target_pao2)) {
        a_o2 <- 1 - exp(-dt / max(self$tau_seconds, 1e-6))
        if (is.na(self$alveolar_po2)) self$alveolar_po2 <- self$target_pao2
        self$alveolar_po2 <- self$alveolar_po2 + (self$target_pao2 - self$alveolar_po2) * a_o2
      }
      if (is.finite(self$alveolar_po2)) {
        target_paO2 <- self$compute_arterial_po2(self$alveolar_po2)
        if (is.finite(target_paO2)) {
          a_pa <- 1 - exp(-dt / max(self$tau_seconds * 1.2, 1e-6))
          if (is.na(self$arterial_o2)) self$arterial_o2 <- target_paO2
          self$arterial_o2 <- self$arterial_o2 + (target_paO2 - self$arterial_o2) * a_pa
        }
      }
      
      # 2) Volatile multi-stage dynamics
      self$update_volatile_agents(dt)
      
      self$publish_outputs()
      invisible(TRUE)
    },
    
    # compute a single-breath VOL capnogram; returns list(vol_mL, FEco2_mmHg, PetCO2, VCO2_ml)
    breath_capnogram = function(VT_mL, PACO2_mmHg, VDaw_mL = self$VDaw_mL,
                                k2 = self$phase2_k, s3 = self$phase3_slope,
                                n_pts = 120) {
      VT <- max(1, as.numeric(VT_mL)); VD <- max(0, as.numeric(VDaw_mL))
      PACO2 <- max(0, as.numeric(PACO2_mmHg))
      v <- seq(0, VT, length.out = n_pts)
      
      FE <- numeric(n_pts)
      # Phase I: 0..VD : ~0 CO2
      i1 <- v <= VD
      FE[i1] <- 0
      
      # Phase II: logistic rise from 0 at VD to ~PACO2 at VD+Δ
      # Δ chosen as ~15% VT; k2 controls steepness
      v0 <- VD
      d  <- 0.15 * VT
      i2 <- v > VD & v <= (VD + d)
      x2 <- (v[i2] - v0) / d
      FE[i2] <- PACO2 / (1 + exp(-k2 * (x2 - 0.5)))  # centered sigmoid
      
      # Phase III: ramp across remaining volume with slope s3 (mmHg per %VT)
      i3 <- v > (VD + d)
      if (any(i3)) {
        v3 <- v[i3]
        base <- PACO2 * 0.95  # start slightly below PACO2
        frac <- (v3 - (VD + d)) / max(1e-6, VT - (VD + d))
        FE[i3] <- pmin(PACO2, base + s3 * 100 * frac)  # cap at PACO2
      }
      
      Pet <- FE[n_pts]
      # VCO2 per breath (mL STPD approx): trapezoid integral / 713 (mmHg to fraction)*VT?
      # Simpler: convert mmHg to fraction by dividing by PB ~ 713 mmHg (BTPS-ish)
      # and multiply by breath volume in mL => mL CO2
      PB <- 713 # effective alveolar total pressure (mmHg)
      Ffrac <- FE / PB
      VCO2_ml <- sum( (head(Ffrac,-1) + tail(Ffrac,-1))/2 * diff(v) )
      
      list(vol_mL = v, FEco2_mmHg = FE, PetCO2 = Pet, VCO2_ml = VCO2_ml)
    },
    
    # one-step interface from the machine:
    # given an inspiratory stream (fractions), produce an EXHALED stream (fractions)
    step_with_inspiratory = function(insp_stream, dt) {
      # Update O2/volatile targets as you already do
      self$pull_from_gas_source()  # optional if you want to reuse existing code
      
      # Rebuild minute ventilation from machine if available (or keep VA_L_min)
      VT <- (self$patient$systems$respiratory$params$tidal_volume_mL %||% 500)
      RR <- (self$patient$systems$respiratory$params$rate_bpm %||% 12)
      # Your model already computes PACO2 target from VA & VCO2:
      PACO2_now <- as.numeric(self$PACO2 %||% 40)
      
      cg <- self$breath_capnogram(VT, PACO2_now, VDaw_mL = self$VDaw_mL,
                                  k2 = self$phase2_k, s3 = self$phase3_slope)
      self$last_etco2_mmHg <- cg$PetCO2
      self$last_vco2_ml_per_breath <- cg$VCO2_ml
      
      # Build exhaled stream fractions from the *mean* expired CO2 on this breath
      PB <- 713
      mean_FE_mmHg <- mean(cg$FEco2_mmHg)
      FEfrac <- mean_FE_mmHg / PB
      # Start from inspiratory mix and overwrite CO2; renormalize
      fr_in <- insp_stream$fr
      fr_out <- fr_in
      fr_out$CO2 <- FEfrac
      # renorm others (keep O2 + agents relative proportions)
      sum_nonco2 <- max(1e-9, 1 - (fr_in$CO2 %||% 0))
      scale <- (1 - FEfrac) / sum_nonco2
      for (k in names(fr_out)) if (k != "CO2") fr_out[[k]] <- as.numeric(fr_in[[k]] %||% 0) * scale
      
      make_stream(flow_L_min = insp_stream$flow_L_min, fr_out)
    },
    
    # ---------- Volatiles: Fi -> FA -> Fa -> Pa ----------
    update_volatile_agents = function(dt) {
      # 1) read incoming Fi from gas_source (may be NULL/empty)
      incoming <- list()
      if (!is.null(self$gas_source) && is.function(self$gas_source$get_current_fi_agents)) {
        val <- self$gas_source$get_current_fi_agents()
        if (is.list(val) && length(val)) incoming <- lapply(val, as.numeric)
      }
      
      # 2) set coefficients
      a_fi <- 1 - exp(-dt / max(self$tau_agent_fi_sec, 1e-6))
      a_fa <- 1 - exp(-dt / max(self$tau_agent_fa_sec, 1e-6))
      a_pa <- 1 - exp(-dt / max(self$tau_agent_pa_sec, 1e-6))
      
      # 3) make sure we keep updating agents that were present before
      prev_agents <- unique(c(
        names(self$Fi_agents), names(self$FA_agents),
        names(self$Fa_agents), names(self$Pa_agents)
      ))
      all_agents <- unique(c(prev_agents, names(incoming)))
      
      # 4) update per agent (use Fi=0 if not present)
      for (ag in all_agents) {
        Fi <- as.numeric(incoming[[ag]] %||% 0)
        FA <- as.numeric(self$FA_agents[[ag]] %||% 0)
        Fa <- as.numeric(self$Fa_agents[[ag]] %||% 0)
        Pa <- as.numeric(self$Pa_agents[[ag]] %||% 0)
        
        # Stage 1: proximal/airway low-pass toward Fi
        FA <- FA + (Fi - FA) * a_fi; self$FA_agents[[ag]] <- FA
        # Stage 2: alveolar toward FA
        Fa <- Fa + (FA - Fa) * a_fa; self$Fa_agents[[ag]] <- Fa
        # Stage 3: arterial toward Fa
        Pa <- Pa + (Fa - Pa) * a_pa; self$Pa_agents[[ag]] <- Pa
      }
      
      # 5) store the latest delivered Fi (for observability)
      self$Fi_agents <- incoming
      
      invisible(TRUE)
    },

    # ---------- Publishing ----------
    publish_outputs = function() {
      # O2/CO2
      self$set_local_param("alveolar_po2", self$alveolar_po2, TRUE)
      self$set_local_param("arterial_o2",  self$arterial_o2,  TRUE)
      self$set_local_param("PACO2",        self$PACO2,        TRUE)
      self$set_local_param("PaCO2",        self$PaCO2,        TRUE)
      self$set_local_param("RQ",           self$RQ,           TRUE)
      self$set_local_param("target_paco2", self$target_paco2, TRUE)
      self$set_local_param("target_pao2",  self$target_pao2,  TRUE)
      self$set_local_param("EtCO2_mmHg", self$get_etco2_mmHg(), TRUE)
      self$set_local_param("EtCO2_mmHg", self$last_etco2_mmHg, TRUE)
      self$set_local_param("VCO2_ml_per_breath", self$last_vco2_ml_per_breath, TRUE)
      
      # Volatiles
      self$set_local_param("Fi_agents", self$Fi_agents, TRUE)   # delivered target (Y-piece)
      self$set_local_param("FA_agents", self$FA_agents, TRUE)   # airway/proximal
      self$set_local_param("Fa_agents", self$Fa_agents, TRUE)   # alveolar
      self$set_local_param("Pa_agents", self$Pa_agents, TRUE)   # arterial (optional)
      
      invisible(TRUE)
    },
    
    get_state = function() {
      list(
        alveolar_po2 = self$alveolar_po2,
        arterial_o2  = self$arterial_o2,
        target_pao2  = self$target_pao2,
        PACO2        = self$PACO2,
        target_paco2 = self$target_paco2,
        PaCO2        = self$PaCO2,
        RQ           = self$RQ,
        VA_L_min     = self$VA_L_min,
        tau_seconds  = self$tau_seconds,
        tau_co2_alv_sec   = self$tau_co2_alv_sec,
        tau_co2_blood_sec = self$tau_co2_blood_sec,
        shunt_fraction    = self$shunt_fraction,
        vq_mismatch       = self$vq_mismatch,
        compliance_factor = self$compliance_factor,
        last_fio2         = self$last_fio2,
        last_vco2_ml_min  = self$last_vco2_ml_min,
        last_vo2_ml_min   = self$last_vo2_ml_min,
        
        # Volatiles
        Fi_agents = self$Fi_agents,
        FA_agents = self$FA_agents,
        Fa_agents = self$Fa_agents,
        Pa_agents = self$Pa_agents
      )
    }
  )
)