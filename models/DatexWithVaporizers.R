DatexWithVaporizers <- R6::R6Class(
  "DatexWithVaporizers",
  inherit = Device,
  public = list(
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
    }
  )
)