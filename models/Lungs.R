Lungs <- R6Class(
  "Lungs",
  inherit = Organ,
  public = list(
    # Environment & constants
    PB = 760,     # barometric pressure (mmHg)
    PH2O = 47,    # water vapor pressure (mmHg)
    RQ = 0.8,     # respiratory quotient
    K_CO2 = 0.863,    # mmHg · L / (mL); used in PACO2 = 0.863 * VCO2 / VA
    
    # Add params field to avoid locked environment error
    params = NULL,
    
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
    device_scope = "datex1", # used for FiO2; can later also carry VA
    metabolic_scope = NULL,  # will default to "<patient>.metabolic"
    
    # Volatile anesthetics
    Fi_agent = 0,
    Fa_agent = 0,
    tau_agent_sec = 10,
    current_agent = "sevoflurane",
    
    mirror_to_patient_scope = TRUE,  # publish a copy under patient scope for backward-compat
    
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
    
    initialize = function(name = "lungs", patient = NULL, config = list(),
                          node_id = NULL, bus = NULL) {
      # Initialize params first
      self$params <- list()
      
      super$initialize(name = name, patient = patient, config = config,
                       node_id = node_id, bus = bus)
      
      if (!is.null(config$device_scope)) self$device_scope <- config$device_scope
      if (!is.null(config$tau_seconds))  self$tau_seconds  <- config$tau_seconds
      if (!is.null(config$RQ))           self$RQ           <- config$RQ
      if (!is.null(config$VA_L_min))     self$VA_L_min     <- config$VA_L_min
      if (!is.null(config$tau_co2_alv_sec))   self$tau_co2_alv_sec   <- config$tau_co2_alv_sec
      if (!is.null(config$tau_co2_blood_sec)) self$tau_co2_blood_sec <- config$tau_co2_blood_sec
      if (!is.null(config$metabolic_scope))   self$metabolic_scope   <- config$metabolic_scope
      
      # Pathophysiology parameters
      if (!is.null(config$shunt_fraction))    self$shunt_fraction    <- config$shunt_fraction
      if (!is.null(config$vq_mismatch))       self$vq_mismatch       <- config$vq_mismatch
      if (!is.null(config$compliance_factor)) self$compliance_factor <- config$compliance_factor
      
      if (!is.null(config$alveolar_po2))       self$alveolar_po2       <- config$alveolar_po2
      if (!is.null(config$arterial_o2))       self$arterial_o2       <- config$arterial_o2
      
      # Default metabolic scope if not provided
      if (is.null(self$metabolic_scope) && !is.null(self$patient) && !is.null(self$patient$node_id)) {
        self$metabolic_scope <- paste0(self$patient$node_id, ".metabolic")
      }
      
      # Subscribe to device FiO2 leaf; compute immediately on changes
      if (!is.null(self$bus)) {
        self$subscribe(
          pattern = sprintf("^state/%s/fio2$", self$device_scope),
          callback = function(topic, msg) {
            self$last_fio2 <- as.numeric(msg$value)
            self$compute_targets()
            self$publish_outputs()  # publish new target immediately (optional)
          },
          replay = TRUE
        )
        # VCO2 and VO2 from MetabolicSystem (if present)
        if (!is.null(self$metabolic_scope)) {
          self$subscribe(
            pattern = sprintf("^state/%s/vco2_ml_min$", self$metabolic_scope),
            callback = function(topic, msg) {
              self$last_vco2_ml_min <- as.numeric(msg$value)
              self$compute_targets()
              self$publish_outputs()
            },
            replay = TRUE
          )
          self$subscribe(
            pattern = sprintf("^state/%s/vo2_ml_min$", self$metabolic_scope),
            callback = function(topic, msg) {
              self$last_vo2_ml_min <- as.numeric(msg$value)
              # if both present, refresh RQ from data
              if (is.finite(self$last_vco2_ml_min) && is.finite(self$last_vo2_ml_min) &&
                  self$last_vo2_ml_min > 0) {
                self$RQ <- self$last_vco2_ml_min / self$last_vo2_ml_min
              }
              self$compute_targets()
              self$publish_outputs()
            },
            replay = TRUE
          )
        }
      }
      
      # Seed from bus (FiO2; VCO2/VO2 if published)
      if (!is.null(self$bus)) {
        val <- self$get_param(self$device_scope, "fio2", default = NA_real_)
        if (is.finite(val)) self$last_fio2 <- val
        if (!is.null(self$metabolic_scope)) {
          vco2 <- self$get_param(self$metabolic_scope, "vco2_ml_min", default = NA_real_)
          vo2  <- self$get_param(self$metabolic_scope, "vo2_ml_min",  default = NA_real_)
          if (is.finite(vco2)) self$last_vco2_ml_min <- vco2
          if (is.finite(vo2))  self$last_vo2_ml_min  <- vo2
          if (is.finite(vco2) && is.finite(vo2) && vo2 > 0) self$RQ <- vco2/vo2
        }
      }
      
      # --- Initial targets & states with fallback values ---
      # Use default VCO2 if not available from metabolic system
      if (is.na(self$last_vco2_ml_min)) {
        # Calculate default VCO2 from RQ and estimated VO2
        default_vo2 <- 250  # mL/min for typical adult
        self$last_vco2_ml_min <- default_vo2 * self$RQ
      }
      
      self$compute_targets()
      
      # Initialize current states to targets if not set
      if (is.na(self$PACO2) && is.finite(self$target_paco2)) {
        self$PACO2 <- self$target_paco2
      }
      if (is.na(self$alveolar_po2) && is.finite(self$target_pao2)) {
        self$alveolar_po2 <- self$target_pao2
      }
      
      # Initialize PaO2 based on PAO2 and pathophysiology
      if (is.finite(self$alveolar_po2)) {
        self$arterial_o2 <- self$compute_arterial_po2(self$alveolar_po2)
      }
      
      # If we still don't have valid CO2 values, use baseline
      if (is.na(self$PACO2)) self$PACO2 <- self$PaCO2
      if (is.na(self$target_paco2)) self$target_paco2 <- self$PaCO2
      
      if (is.null(self$node_id)) {
        # Give lungs a predictable scope on the bus
        base <- if (!is.null(self$patient) && !is.null(self$patient$name)) self$patient$name else "patient"
        self$node_id <- paste0(base, ".lungs")
      }
      
      self$publish_outputs()
    },
    
    # ---------- Targets (combined function) ----------
    compute_targets = function() {
      self$compute_co2_target()
      self$compute_o2_target()
      invisible(TRUE)
    },
    
    # CO2: PACO2 target from VCO2 and alveolar ventilation VA
    compute_co2_target = function() {
      vco2 <- self$last_vco2_ml_min
      VA   <- self$VA_L_min
      
      if (!is.finite(vco2) || !is.finite(VA) || VA <= 0) {
        # Keep existing target if we can't compute a new one
        if (is.na(self$target_paco2)) {
          self$target_paco2 <- self$PaCO2  # fallback to current arterial value
        }
        return(invisible(FALSE))
      }
      
      self$target_paco2 <- self$K_CO2 * (vco2 / VA)   # mmHg
      invisible(TRUE)
    },
    
    # O2: PAO2 target from FiO2 and PaCO2 (alveolar gas equation)
    compute_o2_target = function() {
      fio2 <- self$last_fio2
      if (is.na(fio2)) {
        # Keep existing target if we can't compute a new one
        return(invisible(FALSE))
      }
      if (fio2 > 1) fio2 <- fio2 / 100
      self$target_pao2 <- (self$PB - self$PH2O) * fio2 - (self$PaCO2 / self$RQ)
      invisible(TRUE)
    },
    
    # Calculate arterial PO2 from alveolar PO2 considering pathophysiology
    compute_arterial_po2 = function(pao2_alv) {
      if (!is.finite(pao2_alv)) return(NA_real_)
      
      # Start with ideal alveolar PO2
      pao2_ideal <- pao2_alv
      
      # Apply shunt effect (mixing with venous blood)
      # Simplified shunt equation: PaO2 ≈ PAO2 * (1 - Qs/Qt) + PvO2 * (Qs/Qt)
      # Assuming mixed venous PO2 (PvO2) around 40 mmHg
      pvo2_mixed_venous <- 40  # mmHg
      pao2_with_shunt <- pao2_ideal * (1 - self$shunt_fraction) + pvo2_mixed_venous * self$shunt_fraction
      
      # Apply V/Q mismatch effect
      # V/Q mismatch reduces oxygen transfer efficiency
      vq_efficiency <- 1 - self$vq_mismatch * 0.3  # 30% max reduction
      pao2_with_vq <- pao2_with_shunt * vq_efficiency
      
      # Apply compliance factor (affects ventilation effectiveness)
      # Reduced compliance (stiff lungs) reduces gas exchange efficiency
      compliance_efficiency <- 0.7 + 0.3 * self$compliance_factor  # 70-100% efficiency
      pao2_final <- pao2_with_vq * compliance_efficiency
      
      # Ensure PaO2 doesn't go below physiological minimum
      pao2_final <- max(pao2_final, 30)  # minimum ~30 mmHg
      
      return(pao2_final)
    },
    
    # ---------- Tick ----------
    update = function(dt = 1) {
      # 1) Recompute targets (VCO2/VA may have changed)
      self$compute_targets()
      
      # 2) Alveolar CO2 relaxes toward target
      if (is.finite(self$target_paco2)) {
        alpha_co2_alv <- 1 - exp(-dt / max(self$tau_co2_alv_sec, 1e-6))
        if (is.na(self$PACO2)) self$PACO2 <- self$target_paco2
        self$PACO2 <- self$PACO2 + (self$target_paco2 - self$PACO2) * alpha_co2_alv
      }
      
      # 3) Arterial PaCO2 approaches PACO2 with a shorter time constant
      if (is.finite(self$PACO2)) {
        alpha_blood <- 1 - exp(-dt / max(self$tau_co2_blood_sec, 1e-6))
        self$PaCO2 <- self$PaCO2 + (self$PACO2 - self$PaCO2) * alpha_blood
      }
      
      # 4) Recompute O2 target using updated PaCO2, then relax O2 toward target
      self$compute_o2_target()
      if (is.finite(self$target_pao2)) {
        alpha_o2 <- 1 - exp(-dt / max(self$tau_seconds, 1e-6))
        if (is.na(self$alveolar_po2)) self$alveolar_po2 <- self$target_pao2
        self$alveolar_po2 <- self$alveolar_po2 + (self$target_pao2 - self$alveolar_po2) * alpha_o2
      }
      
      # 5) Update arterial PO2 based on current alveolar PO2 and pathophysiology
      if (is.finite(self$alveolar_po2)) {
        target_arterial_po2 <- self$compute_arterial_po2(self$alveolar_po2)
        if (is.finite(target_arterial_po2)) {
          # Arterial PO2 approaches target with similar dynamics to alveolar
          alpha_arterial <- 1 - exp(-dt / max(self$tau_seconds * 1.2, 1e-6))  # slightly slower than alveolar
          if (is.na(self$arterial_o2)) self$arterial_o2 <- target_arterial_po2
          self$arterial_o2 <- self$arterial_o2 + (target_arterial_po2 - self$arterial_o2) * alpha_arterial
        }
      }
      
      if (is.finite(self$Fi_agent)) {
        alpha_a <- 1 - exp(-dt / max(self$tau_agent_sec, 1e-6))
        if (is.na(self$Fa_agent)) self$Fa_agent <- self$Fi_agent
        self$Fa_agent <- self$Fa_agent + (self$Fi_agent - self$Fa_agent) * alpha_a
      }
      
      self$publish_outputs()
      invisible(TRUE)
    },
    
    # ---------- Publishing ----------
    publish_outputs = function() {
      # Publish under lungs scope (safe helper handles either self$set_param or bus$set_param)
      self$set_local_param("alveolar_po2", self$alveolar_po2, notify = TRUE)
      self$set_local_param("arterial_o2",  self$arterial_o2,  notify = TRUE)
      self$set_local_param("PACO2",        self$PACO2,        notify = TRUE)
      self$set_local_param("PaCO2",        self$PaCO2,        notify = TRUE)
      self$set_local_param("RQ",           self$RQ,           notify = TRUE)
      self$set_local_param("target_paco2", self$target_paco2, notify = TRUE)
      self$set_local_param("target_pao2",  self$target_pao2,  notify = TRUE)
      self$set_local_param("Fi_agent",     self$Fi_agent,     notify = TRUE)
      self$set_local_param("Fa_agent",     self$Fa_agent,     notify = TRUE)
      if (tolower(self$current_agent) == "sevoflurane") {
        self$set_local_param("Fa_sevo", self$Fa_agent, notify = TRUE)
      }
      
      # Back-compat mirror under patient scope (only if bus + patient available)
      if (isTRUE(self$mirror_to_patient_scope) && !is.null(self$bus) &&
          !is.null(self$patient) && !is.null(self$patient$node_id)) {
        self$bus$set_param(self$patient$node_id, "alveolar_po2", self$alveolar_po2, notify = TRUE)
        self$bus$set_param(self$patient$node_id, "arterial_o2",  self$arterial_o2,  notify = TRUE)
        self$bus$set_param(self$patient$node_id, "PaCO2",        self$PaCO2,        notify = TRUE)
        self$bus$set_param(self$patient$node_id, "PACO2",        self$PACO2,        notify = TRUE)
      }
      
      invisible(TRUE)
    },
    
    get_state = function() {
      list(
        alveolar_po2 = self$alveolar_po2,
        arterial_o2  = self$arterial_o2,   # PaO2
        target_pao2  = self$target_pao2,
        PACO2        = self$PACO2,
        target_paco2 = self$target_paco2,
        PaCO2        = self$PaCO2,
        RQ           = self$RQ,
        VA_L_min     = self$VA_L_min,
        tau_seconds = self$tau_seconds,
        tau_co2_alv_sec = self$tau_co2_alv_sec,
        tau_co2_blood_sec = self$tau_co2_blood_sec,
        shunt_fraction = self$shunt_fraction,
        vq_mismatch = self$vq_mismatch,
        compliance_factor = self$compliance_factor,
        last_fio2    = self$last_fio2,
        last_vco2_ml_min = self$last_vco2_ml_min,
        last_vo2_ml_min  = self$last_vo2_ml_min,
        Fi_agent = self$Fi_agent,
        Fa_agent = self$Fa_agent,
        Fa_sevo  = if (tolower(self$current_agent)=="sevoflurane") self$Fa_agent else NA_real_
      )
    }
  )
)