DeviceComponent <- R6Class(
  "DeviceComponent",
  public = list(
    type = NULL,
    parameters = list(),
    
    initialize = function(type, parameters = list()) {
      self$type <- type
      self$parameters <- parameters
    },
    
    # Process gas mixture (to be overridden by specific components)
    process_gas = function(input_mixture, dt, part_context = NULL) {
      # Default: pass through unchanged
      return(input_mixture$copy())
    },
    
    # Get component-specific parameters
    get_parameters = function() {
      return(self$parameters)
    }
  )
)