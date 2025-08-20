SimulationEngine <- R6Class(
  "SimulationEngine",
  inherit = BaseNode,
  public = list(
    # Core simulation components - these are just containers now
    patient = NULL,
    machine = NULL,
    
    # Configuration
    step_interval = 1.0,
    current_time = 0,
    
    # Simulation state
    simulation_running = FALSE,
    simulation_data = list(),
    
    # HTTP components
    http_controller = NULL,
    http_server = NULL,
    http_port = 8080,
    
    initialize = function(patient_config = NULL, machine_config = NULL, 
                          step_interval = 1.0, http_port = 8080, 
                          node_id = NULL, bus = NULL) {
      super$initialize(node_id = node_id)
      
      self$step_interval <- step_interval
      self$http_port <- http_port
      
      if (is.null(bus)) {
        bus <- MessageBus$new()
      }
      self$connect_bus(bus)
      
      # Initialize components from config files
      self$initialize_patient(patient_config)
      self$initialize_machine(machine_config)
      
      # Wire the gas source into the patient’s lungs (safe)
      if (!is.null(self$patient) &&
          !is.null(self$patient$systems$respiratory) &&
          !is.null(self$machine)) {
        
        rs <- self$patient$systems$respiratory
        if (is.function(rs$set_gas_source)) {
          rs$set_gas_source(self$machine)
        } else if (!is.null(rs$organs$lungs) && is.function(rs$organs$lungs$set_gas_source)) {
          # Fallback: wire directly into lungs if RespiratorySystem doesn’t expose the method
          rs$organs$lungs$set_gas_source(self$machine)
        } else {
          message("No set_gas_source() method found on respiratory system or lungs; skipping auto-wiring.")
        }
      }
      
      # Initialize HTTP controller and server
      self$http_controller <- HTTPController$new(self)
      self$initialize_http_server()
    },
    
    # Load patient from YAML config - no hardcoded parameter names
    initialize_patient = function(config_path) {
      if (is.null(config_path)) {
        cat("No patient configuration provided. Using default patient.\n")
        self$patient <- Patient$new(node_id = "patient", bus = self$bus)
      } else {
        if (!file.exists(config_path)) {
          stop(paste("Patient configuration file not found:", config_path))
        }
        
        cat("Loading patient configuration from:", config_path, "\n")
        patient_config <- yaml::read_yaml(config_path)
        
        # Create a Patient R6 object
        patient <- Patient$new(node_id = "patient", bus = self$bus)
        
        # Demographics
        patient$age    <- patient_config$age    %||% patient$age
        patient$weight <- patient_config$weight %||% patient$weight
        patient$height <- patient_config$height %||% patient$height
        patient$gender <- patient_config$gender %||% patient$gender
        
        # Initialize systems
        if (!is.null(patient_config$systems)) {
          for (sys in patient_config$systems) {
            sys_class <- get(sys$class, envir = globalenv())
            sys_obj   <- sys_class$new(patient = patient, config = sys, bus = self$bus)
            patient$add_system(sys$name, sys_obj)
          }
        }
        
        self$patient <- patient
      }
    },
    
    initialize_machine = function(config_path) {
      if (is.null(config_path)) stop("Provide a machine YAML")
      
      cfg <- yaml::read_yaml(config_path)
      dev <- cfg$device %||% cfg
      
      cls_name <- dev$class %||% "Device"
      if (!exists(cls_name, inherits = TRUE)) {
        stop(paste("Device class not found:", cls_name))
      }
      gen <- get(cls_name, inherits = TRUE)
      
      # R6 classes are "R6ClassGenerator", not functions
      if (!inherits(gen, "R6ClassGenerator"))
        stop(sprintf("Symbol '%s' exists but is not an R6 class generator", cls_name))
      
      nm <- dev$name %||% cls_name
      settings <- dev
      settings$class <- NULL
      settings$name  <- NULL
      
      self$machine <- gen$new(
        name    = nm,
        settings = settings,  # your DatexOhmedaS5Avance reads fields from here
        node_id = paste0(gsub("\\s+","_", nm), ".machine"),
        bus     = self$bus
      )
    },
    
    # Initialize HTTP server with generic routing
    initialize_http_server = function() {
      if (is.null(self$http_port)) return(invisible(self))
      
      cat("Starting HTTP server on port", self$http_port, "\n")
      
      app <- list(
        call = function(req) {
          path <- req$PATH_INFO
          method <- req$REQUEST_METHOD
          
          if (method == "GET") {
            return(self$http_controller$handle_get_request(path))
          } else if (method == "POST") {
            return(self$http_controller$handle_post_request(path, req))
          }
          
          # Default 404 response
          list(
            status = 404L,
            headers = list("Content-Type" = "application/json"),
            body = jsonlite::toJSON(list(error = "Method not allowed"))
          )
        }
      )
      
      tryCatch({
        self$http_server <- httpuv::startServer("0.0.0.0", self$http_port, app)
        cat("HTTP server started successfully on http://localhost:", self$http_port, "\n")
        cat("Try: curl http://localhost:", self$http_port, "/ to see available endpoints\n")
      }, error = function(e) {
        warning(paste("Failed to start HTTP server:", e$message))
      })
    },
    
    # Simulation control - these don't need to know about specific parameters
    start_simulation = function() {
      if (self$simulation_running) {
        cat("Simulation is already running.\n")
        return(invisible(self))
      }
      
      cat("Starting simulation...\n")
      self$simulation_running <- TRUE
      self$current_time <- 0
      self$schedule_tick()
    },
    
    stop_simulation = function() {
      if (!self$simulation_running) {
        cat("Simulation is not running.\n")
        return(invisible(self))
      }
      
      cat("Stopping simulation...\n")
      self$simulation_running <- FALSE
    },
    
    # Simulation tick - generic update
    tick = function() {
      if (!self$simulation_running) return(invisible(NULL))
      self$step()
      self$current_time <- self$current_time + self$step_interval
      self$schedule_tick()
      invisible(NULL)
    },
    
    schedule_tick = function() {
      if (requireNamespace("later", quietly = TRUE)) {
        later::later(function() self$tick(), delay = self$step_interval)
      } else {
        Sys.sleep(self$step_interval)
        self$tick()
      }
    },
    
    step = function() {
      # Let each top‑level actor tick itself
      if (!is.null(self$machine) && is.function(self$machine$update)) {
        self$machine$update(self$step_interval)
      }
      if (!is.null(self$patient) && is.function(self$patient$update)) {
        self$patient$update(self$step_interval)
      }
      
      # (Optional) publish simulation time on the bus for anyone who cares
      if (!is.null(self$bus)) {
        self$bus$set_param("simulation", "time_s", self$current_time + self$step_interval, notify = FALSE)
      }
      
      # Bookkeeping
      self$record_simulation_data()
    },
    
    update_object = function(obj, dt) {
      if (inherits(obj, "R6")) {
        if ("update" %in% names(obj) && is.function(obj$update)) obj$update(dt)
        # walk public members that are lists/R6
        for (nm in names(obj)) {
          child <- tryCatch(obj[[nm]], error=function(e) NULL)
          if (is.list(child) || inherits(child, "R6")) self$update_object(child, dt)
        }
        return(invisible())
      }
      if (is.list(obj)) {
        if ("update" %in% names(obj) && is.function(obj$update)) obj$update(dt)
        for (item in obj) if (is.list(item)) self$update_object(item, dt)
      }
    },
    
    # Record current state without knowing specific parameter names
    record_simulation_data = function() {
      data_point <- list(
        time = self$current_time,
        patient = self$patient,
        machine = self$machine
      )
      
      self$simulation_data[[length(self$simulation_data) + 1]] <- data_point
      
      # Keep only last 1000 data points
      if (length(self$simulation_data) > 1000) {
        self$simulation_data <- tail(self$simulation_data, 1000)
      }
    },
    
    shutdown = function() {
      private$finalize()
    },
    
    connect_patient_to_machine_manual_mask = function(mask_seal = 0.3, mv_L_min = NULL) {
      m <- ModeManualMask$new(mask_seal = mask_seal)
      if (!is.null(mv_L_min)) m$patient_minute_vent_L_min <- mv_L_min
      else {
        lungs <- self$patient$systems$respiratory$organs$lungs
        m$patient_minute_vent_L_min <- lungs$VA_L_min * 1.2
      }
      self$machine$set_mode(m)
      self$patient$systems$respiratory$set_gas_source(self$machine)
      invisible(TRUE)
    },
    
    connect_patient_to_machine_controlled = function() {
      self$machine$set_mode(ModeControlled$new())
      self$patient$systems$respiratory$set_gas_source(self$machine)
      invisible(TRUE)
    },
    
    disconnect_patient_to_room_air = function() {
      # keep machine; just make mode deliver room air (no agents)
      room_mode <- R6::R6Class(
        "ModeRoomAir", inherit = Mode,
        public = list(
          label = "room_air",
          get_fio2 = function(machine) 0.21,
          get_current_fi_agents = function(machine) list()
        )
      )$new()
      self$machine$set_mode(room_mode)
      self$patient$systems$respiratory$set_gas_source(self$machine)
      invisible(TRUE)
    }
  ),
  
  private = list(
    # Cleanup
    finalize = function() {
      if (!is.null(self$http_server)) {
        httpuv::stopServer(self$http_server)
        cat("HTTP server stopped\n")
      }
      
      if (self$simulation_running) {
        self$stop_simulation()
      }
    }    
  )
)