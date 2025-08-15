DevicePart <- R6::R6Class(
  "DevicePart",
  inherit = BaseNode,  # Assuming you have this base class
  public = list(
    name = NULL,
    parent_device = NULL,
    enabled = TRUE,
    
    initialize = function(name, parent_device = NULL, node_id = NULL, bus = NULL) {
      super$initialize(node_id = node_id)
      self$name <- name
      self$parent_device <- parent_device
      if (!is.null(bus)) self$connect_bus(bus)
    },
    
    # Override this in subclasses to perform component-specific updates
    update = function(dt) {
      # Default implementation does nothing
      invisible(TRUE)
    },
    
    # Override this to publish component-specific data
    publish_data = function() {
      if (is.null(self$bus)) return(invisible())
      # Default implementation publishes basic status
      self$publish_params(list(
        component = self$name,
        enabled = self$enabled
      ), notify = TRUE)
    },
    
    # Helper to validate numeric fields (can be overridden)
    validate_numeric_field = function(field_name, current_value, default_value = 0) {
      if (!is.numeric(current_value)) {
        val <- suppressWarnings(as.numeric(current_value))
        if (is.na(val)) val <- default_value
        cat("Warning: Reset", self$name, field_name, "to", val, "due to type corruption\n")
        return(val)
      }
      return(current_value)
    }
  )
)