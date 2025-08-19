DatexWithVaporizers <- R6::R6Class(
  "DatexWithVaporizers",
  inherit = Device,
  public = list(
    vaporizer_bank = NULL,
    
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
    
    # These two make objects “GasSource”-compatible for lungs pulling locally
    get_current_fi_agents = function() {
      self$vaporizer_bank$get_volatile_agent_composition()
    },
    
    get_fio2 = function() {
      # default; subclasses override with their own O2 mixing
      0.21
    }
  )
)