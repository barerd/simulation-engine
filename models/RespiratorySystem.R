RespiratorySystem <- R6Class(
  "RespiratorySystem",
  inherit = PhysiologicalSystem,
  public = list(
    gas_source = NULL,  # will hold the machine (DatexOhmedaS5Avance)
    # Direct access properties (mirrors from lungs)
    Fi_agents = NULL,
    Fa_agents = NULL,
    alveolar_po2 = NA_real_,
    arterial_o2 = NA_real_,
    PACO2 = NA_real_,
    PaCO2 = NA_real_,
    
    initialize = function(patient, node_id = NULL, bus = NULL,
                          config = list(), lungs_config = NULL, ...) {
      super$initialize(name = "respiratory", patient = patient,
                       node_id = node_id, bus = bus, config = config)
      
      if (is.null(lungs_config) && !is.null(config$lungs)) {
        lungs_config <- config$lungs
      }
      if (is.null(lungs_config)) {
        lungs_config <- list(name = "lungs", device_scope = "datex1",
                             tau_seconds = 3.0)
      }
      
      # Create lungs and attach
      lungs <- Lungs$new(
        name = lungs_config$name %||% "lungs",
        patient = patient,
        config = lungs_config,
        node_id = if (!is.null(lungs_config$node_id)) lungs_config$node_id else paste0(patient$node_id, ".lungs"),
        bus = self$bus
      )
      self$add_organ("lungs", lungs)
      
      # Initialize direct access properties from lungs
      self$sync_from_lungs()
    },
    
    # Sync properties from lungs to respiratory system level
    sync_from_lungs = function() {
      if (!is.null(self$organs$lungs)) {
        lungs <- self$organs$lungs
        self$Fi_agents <- lungs$Fi_agents
        self$Fa_agents <- lungs$Fa_agents
        self$alveolar_po2 <- lungs$alveolar_po2
        self$arterial_o2 <- lungs$arterial_o2
        self$PACO2 <- lungs$PACO2
        self$PaCO2 <- lungs$PaCO2
      }
      invisible(TRUE)
    },
    
    # Override update to sync properties after lungs update
    update = function(dt = 1) {
      # Call parent update (which will update all organs including lungs)
      super$update(dt)
      
      # Sync properties from lungs to system level
      self$sync_from_lungs()
      
      invisible(TRUE)
    },
    
    # Method to get current gas exchange state
    get_gas_exchange_state = function() {
      list(
        volatile_agents = list(
          Fi_agents = self$Fi_agents,
          Fa_agents = self$Fa_agents,
        ),
        oxygenation = list(
          alveolar_po2 = self$alveolar_po2,
          arterial_o2 = self$arterial_o2,
          target_pao2 = if (!is.null(self$organs$lungs)) self$organs$lungs$target_pao2 else NA_real_
        ),
        co2_status = list(
          PACO2 = self$PACO2,
          PaCO2 = self$PaCO2,
          target_paco2 = if (!is.null(self$organs$lungs)) self$organs$lungs$target_paco2 else NA_real_
        )
      )
    },
    
    get_outputs = function() {
      # Export what other systems/devices may read
      # Include both detailed lungs state and direct system-level access
      list(
        lungs = self$organs$lungs$get_state(),
        # Direct access at system level
        Fi_agents = self$Fi_agents,
        Fa_agents = self$Fa_agents,
        alveolar_po2 = self$alveolar_po2,
        arterial_o2 = self$arterial_o2,
        PACO2 = self$PACO2,
        PaCO2 = self$PaCO2
      )
    },
    
    get_fi = function(agent) {
      # Delivered inspired fraction at the Y-piece (what machine is sending)
      return(self$organs$lungs$Fi_agents[[agent]] %||% 0)
    },
    
    get_fa = function(agent) {
      # Arterial fraction (post-mixing)
      return(self$organs$lungs$Fa_agents[[agent]] %||% 0)
    },
    
    get_fA = function(agent) {
      # Alveolar concentration
      return(self$organs$lungs$FA_agents[[agent]] %||% 0)
    },
    
    get_Pa = function(agent) {
      # partial pressure, CNS effect site
      return(self$organs$lungs$Pa_agents[[agent]] %||% 0)
    },
    
    get_total_mac = function() {
      # Sum MAC using alveolar fractions (Fa)
      total <- 0
      fa <- self$organs$lungs$Fa_agents %||% list()
      for (agent in names(fa)) {
        mac_val <- self$get_mac_value(agent)
        if (is.finite(mac_val) && mac_val > 0) {
          total <- total + as.numeric(fa[[agent]]) / mac_val
        }
      }
      total
    },
    
    get_mac_value = function(agent) {
      mac_table <- list(sevoflurane = 2.0, desflurane = 6.0, isoflurane = 1.15)
      return(mac_table[[agent]] %||% NA)
    },
    
    set_gas_source = function(source) {
      self$gas_source <- source
      if (!is.null(self$organs$lungs)) {
        self$organs$lungs$set_gas_source(source)
      }
      invisible(TRUE)
    }
  )
)