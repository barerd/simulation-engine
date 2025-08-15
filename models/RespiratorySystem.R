RespiratorySystem <- R6Class(
  "RespiratorySystem",
  inherit = PhysiologicalSystem,
  public = list(
    # Direct access properties (mirrors from lungs)
    Fi_agent = NA_real_,
    Fa_agent = NA_real_,
    alveolar_po2 = NA_real_,
    arterial_o2 = NA_real_,
    PACO2 = NA_real_,
    PaCO2 = NA_real_,
    
    initialize = function(patient, node_id = NULL, bus = NULL,
                          lungs_config = list(name = "lungs", device_scope = "datex1",
                                              tau_seconds = 3.0)) {
      super$initialize(name = "respiratory", patient = patient,
                       node_id = node_id, bus = bus, config = list())
      
      # Create lungs and attach
      lungs <- Lungs$new(
        name = lungs_config$name,
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
        self$Fi_agent <- lungs$Fi_agent
        self$Fa_agent <- lungs$Fa_agent
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
    
    # Method to set volatile agent (delegates to lungs)
    set_volatile_agent = function(fi_agent, agent_name = "sevoflurane") {
      if (!is.null(self$organs$lungs)) {
        self$organs$lungs$Fi_agent <- fi_agent
        self$organs$lungs$current_agent <- agent_name
        self$sync_from_lungs()
      }
      invisible(TRUE)
    },
    
    # Method to get current gas exchange state
    get_gas_exchange_state = function() {
      list(
        volatile_agents = list(
          Fi_agent = self$Fi_agent,
          Fa_agent = self$Fa_agent,
          current_agent = if (!is.null(self$organs$lungs)) self$organs$lungs$current_agent else "sevoflurane"
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
        Fi_agent = self$Fi_agent,
        Fa_agent = self$Fa_agent,
        alveolar_po2 = self$alveolar_po2,
        arterial_o2 = self$arterial_o2,
        PACO2 = self$PACO2,
        PaCO2 = self$PaCO2
      )
    }
  )
)