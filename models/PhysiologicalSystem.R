PhysiologicalSystem <- R6Class(
  "PhysiologicalSystem",
  inherit = BaseNode,
  public = list(
    patient = NULL,
    name = NULL,
    organs = NULL,        # named list of Organ instances
    
    initialize = function(name = "", patient = NULL, config = list(),
                          node_id = NULL, bus = NULL) {
      super$initialize(node_id = node_id)
      self$name <- name
      self$patient <- patient
      self$organs <- list()
      if (!is.null(bus)) self$connect_bus(bus)
      
      # (Optional) Build organs from config$organs: list of {class, name, node_id, ...}
      if (!is.null(config$organs) && length(config$organs)) {
        for (org_cfg in config$organs) {
          stopifnot(!is.null(org_cfg$class), !is.null(org_cfg$name))
          cls <- get(org_cfg$class, mode = "function")
          org <- cls$new(
            name = org_cfg$name,
            patient = patient,
            config = org_cfg,
            node_id = if (!is.null(org_cfg$node_id)) org_cfg$node_id else NULL,
            bus = self$bus
          )
          self$add_organ(org_cfg$name, org)
        }
      }
    },
    
    add_organ = function(name, organ_instance) {
      stopifnot(inherits(organ_instance, "Organ"))
      # Ensure the organ shares bus & patient
      if (is.null(organ_instance$bus) && !is.null(self$bus))
        organ_instance$connect_bus(self$bus)
      if (is.null(organ_instance$patient) && !is.null(self$patient))
        organ_instance$patient <- self$patient
      self$organs[[name]] <- organ_instance
      invisible(TRUE)
    },
    
    update = function(dt = 1) {
      # Forward the tick to each organ
      for (nm in names(self$organs)) {
        try(self$organs[[nm]]$update(dt), silent = TRUE)
      }
      invisible(TRUE)
    },
    
    get_state = function() {
      st <- list()
      for (nm in names(self$organs)) {
        st[[nm]] <- self$organs[[nm]]$get_state()
      }
      st
    },
    
    get_outputs = function() {
      # Optional: summarize outputs the system exposes to other systems/devices
      self$get_state()
    }
  )
)