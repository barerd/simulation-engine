Organ <- R6::R6Class(
  "Organ",
  inherit = BaseNode,
  public = list(
    patient = NULL,
    name = NULL,
    components = NULL,   # named list of sub-components (optional)
    
    initialize = function(name = "", patient = NULL, config = list(),
                          node_id = NULL, bus = NULL) {
      super$initialize(node_id = node_id)
      self$name <- name
      self$patient <- patient
      self$components <- list()
      if (!is.null(bus)) self$connect_bus(bus)
      self$initialize_components(config)
    },
    
    initialize_components = function(config = list()) {
      # Optional dynamic component creation: each item needs fields {class, name, ...}
      if (!is.null(config$components) && length(config$components)) {
        for (comp_config in config$components) {
          stopifnot(!is.null(comp_config$class), !is.null(comp_config$name))
          comp_class <- get(comp_config$class, mode = "function")
          comp_instance <- comp_class$new(config = comp_config)
          self$components[[comp_config$name]] <- comp_instance
        }
      }
    },
    
    # Default organ update: no-op; override in subclasses
    update = function(dt = 1) { invisible(TRUE) },
    
    # Optional organ outputs/state accessors
    get_state = function() { list() }
  )
)