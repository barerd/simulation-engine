Patient <- R6Class(
  "Patient",
  inherit = BaseNode,
  public = list(
    age = 40, weight = 70, height = 170, gender = "male", systems = NULL,
    
    initialize = function(node_id = NULL, bus = NULL) {
      super$initialize(node_id = node_id)
      if (!is.null(bus)) self$connect_bus(bus)
      
      # Publish initial demographics
      # self$publish_demographics()
    },
    
    add_system = function(name, system_instance) {
      stopifnot(inherits(system_instance, "PhysiologicalSystem"))
      if (is.null(system_instance$bus) && !is.null(self$bus))
        system_instance$connect_bus(self$bus)
      if (is.null(system_instance$patient)) system_instance$patient <- self
      self$systems[[name]] <- system_instance
      invisible(TRUE)
    },
    
    publish_demographics = function() {
      self$publish_params(list(
        age = self$age,
        weight = self$weight,
        height = self$height,
        gender = self$gender
      ))
    },
    
    # Called by the engine once per tick with dt (seconds)
    update = function(dt = 1) {
      for (nm in names(self$systems)) {
        try(self$systems[[nm]]$update(dt), silent = TRUE)
      }
      invisible(TRUE)
    },
    
    # Getter for Fi of a specific agent
    get_fi = function(agent) {
      if (is.null(self$systems$respiratory)) {
        stop("Respiratory system not connected")
      }
      return(self$systems$respiratory$get_fi(agent))
    },
    
    # Getter for FA (alveolar fraction), if your model has it
    get_fa = function(agent) {
      if (is.null(self$systemsrespiratory)) {
        stop("Respiratory system not connected")
      }
      return(self$systemsrespiratory$get_fa(agent))
    },
    
    # Getter for MAC fraction (total effect)
    get_mac_fraction = function() {
      if (is.null(self$systemsrespiratory)) {
        return(0)
      }
      return(self$systemsrespiratory$get_total_mac())
    }
  )
)