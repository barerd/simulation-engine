Device <- R6Class(
  "Device",
  inherit = BaseNode,
  public = list(
    name = NULL, 
    parts = list(), 
    settings = list(), 
    internal_connections = list(),
    topology_config = NULL,
    
    initialize = function(name, topology_config = NULL, node_id = NULL, bus = NULL) {
      super$initialize(node_id = node_id)
      self$name <- name
      self$parts <- list()
      self$settings <- list()
      self$internal_connections <- list()
      self$topology_config <- topology_config
      
      if (!is.null(topology_config)) {
        self$build_from_topology(topology_config)
      }
      
      if (!is.null(bus)) self$connect_bus(bus)
    },
    
    # Build device from topology configuration
    build_from_topology = function(topology_config) {
      # Create parts from configuration
      if ("parts" %in% names(topology_config)) {
        for (part_name in names(topology_config$parts)) {
          part_config <- topology_config$parts[[part_name]]
          part <- self$create_part(part_name, part_config)
          self$parts[[part_name]] <- part
        }
      }
      
      # Create internal connections
      if ("connections" %in% names(topology_config)) {
        self$create_connections(topology_config$connections)
      }
      
      # Apply device-level settings
      if ("settings" %in% names(topology_config)) {
        self$settings <- topology_config$settings
      }
    },
    
    # Factory method to create parts based on configuration
    create_part = function(name, config) {
      part_type <- config$type
      parameters <- config$parameters %||% list()
      
      part <- switch(part_type,
                     "fresh_gas_mixer" = FreshGasMixer$new(name, parameters),
                     "vaporizer" = VaporizerPart$new(name, parameters),
                     "breathing_circuit" = BreathingCircuit$new(name, parameters),
                     "co2_absorber" = CO2Absorber$new(name, parameters),
                     # Default to generic DevicePart
                     DevicePart$new(name, part_type, parameters)
      )
      
      # Add any specified components
      if ("components" %in% names(config)) {
        for (component_config in config$components) {
          component <- self$create_component(component_config)
          part$add_component(component)
        }
      }
      
      return(part)
    },
    
    # Factory method to create components
    create_component = function(config) {
      component_type <- config$type
      parameters <- config$parameters %||% list()
      
      return(switch(component_type,
                    "co2_absorber" = CO2AbsorberComponent$new(
                      parameters$chemistry %||% "soda_lime",
                      parameters$capacity %||% 5.0,
                      parameters
                    ),
                    "vaporizer" = VaporizerComponent$new(
                      parameters$agent %||% "sevoflurane",
                      parameters$setting %||% 0,
                      parameters
                    ),
                    # Default component
                    DeviceComponent$new(component_type, parameters)
      ))
    },
    
    # Create connections between parts
    create_connections = function(connections_config) {
      for (connection in connections_config) {
        from_part <- connection$from_part
        to_part <- connection$to_part
        from_compartment <- connection$from_compartment %||% "outlet"
        to_compartment <- connection$to_compartment %||% "inlet"
        flow_rate <- connection$flow_rate %||% 0
        
        self$connect_parts(from_part, to_part, from_compartment, to_compartment, flow_rate)
      }
    },
    
    # Connect two parts
    connect_parts = function(from_part_name, to_part_name, 
                             from_compartment = "outlet", to_compartment = "inlet", 
                             flow_rate = 0) {
      
      if (!(from_part_name %in% names(self$parts))) {
        stop(paste("Source part", from_part_name, "not found"))
      }
      if (!(to_part_name %in% names(self$parts))) {
        stop(paste("Target part", to_part_name, "not found"))
      }
      
      from_part <- self$parts[[from_part_name]]
      to_part <- self$parts[[to_part_name]]
      
      # Get compartments (use outlet/inlet by default)
      from_comp <- if (from_compartment == "outlet") {
        from_part$get_outlet_compartment()
      } else {
        from_part$compartments[[from_compartment]]
      }
      
      to_comp <- if (to_compartment == "inlet") {
        to_part$get_inlet_compartment()  
      } else {
        to_part$compartments[[to_compartment]]
      }
      
      if (is.null(from_comp) || is.null(to_comp)) {
        stop("Could not find specified compartments for connection")
      }
      
      # Create the connection
      outlet_id <- from_comp$add_outlet_flow(to_comp, flow_rate)
      inlet_id <- to_comp$add_inlet_flow(from_comp, flow_rate)
      
      # Store connection info
      connection_id <- length(self$internal_connections) + 1
      self$internal_connections[[connection_id]] <- list(
        from_part = from_part_name,
        to_part = to_part_name,
        from_compartment = from_compartment,
        to_compartment = to_compartment,
        flow_rate = flow_rate,
        outlet_id = outlet_id,
        inlet_id = inlet_id
      )
      
      return(connection_id)
    },
    
    # Simulate the entire device
    simulate_step = function(dt = 0.1) {
      # Process each part
      for (part in self$parts) {
        part$process(dt)
      }
      
      invisible(self)
    },
    
    # Get part by name
    get_part = function(name) {
      return(self$parts[[name]])
    },
    
    # Set device parameters
    set_parameter = function(part_name, parameter, value) {
      if (part_name %in% names(self$parts)) {
        part <- self$parts[[part_name]]
        if (parameter %in% names(part)) {
          part[[parameter]] <- value
        } else if ("set_flows" %in% names(part) && parameter %in% c("o2_flow", "air_flow", "n2o_flow")) {
          # Special handling for gas flows
          flows <- list()
          flows[[gsub("_flow", "", parameter)]] <- value
          do.call(part$set_flows, flows)
        }
      }
    },
    
    # Load topology from YAML file
    load_topology = function(yaml_file) {
      self$topology_config <- yaml::read_yaml(yaml_file)
      self$build_from_topology(self$topology_config)
    },
    
    publish_demographics = function() {
      self$publish_params(list(
        name = self$name,
        parts = names(self$parts),
        settings = self$settings
      ))
    },
    
    # Get current state summary
    get_state = function() {
      state <- list()
      for (part_name in names(self$parts)) {
        part <- self$parts[[part_name]]
        part_state <- list()
        for (comp_name in names(part$compartments)) {
          compartment <- part$compartments[[comp_name]]
          part_state[[comp_name]] <- list(
            O2 = round(compartment$mixture$O2, 4),
            CO2 = round(compartment$mixture$CO2, 4),
            volatiles = compartment$mixture$volatiles,
            pressure = compartment$mixture$pressure,
            volume = compartment$volume
          )
        }
        state[[part_name]] <- part_state
      }
      return(state)
    }
  )
)