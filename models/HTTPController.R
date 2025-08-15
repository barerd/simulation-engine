HTTPController <- R6Class(
  "HTTPController",
  public = list(
    simulation_engine = NULL,
    
    initialize = function(simulation_engine) {
      self$simulation_engine <- simulation_engine
    },
    
    # Generic GET handler for any path
    handle_get_request = function(path) {
      # Remove leading slash and split path
      path_parts <- strsplit(gsub("^/", "", path), "/")[[1]]
      
      if (length(path_parts) == 0) {
        return(self$handle_root_request())
      }
      
      # Try to get value from patient or machine config
      result <- self$get_config_value(path_parts)
      
      if (is.null(result)) {
        return(list(
          status = 404L,
          headers = list("Content-Type" = "application/json"),
          body = jsonlite::toJSON(list(error = paste("Path not found:", path)))
        ))
      }
      
      return(list(
        status = 200L,
        headers = list("Content-Type" = "application/json"),
        body = jsonlite::toJSON(result, auto_unbox = TRUE)
      ))
    },
    
    # Generic POST handler for any path
    handle_post_request = function(path, req) {
      path_parts <- strsplit(gsub("^/", "", path), "/")[[1]]
      
      tryCatch({
        # Read the request body
        body <- rawToChar(req$rook.input$read())
        
        # Handle empty body (for simple commands like start/stop)
        if (nchar(body) == 0 || body == "") {
          post_data <- list()  # Empty data for commands that don't need parameters
        } else {
          post_data <- jsonlite::fromJSON(body)
        }
        
        result <- self$set_config_value(path_parts, post_data)
        
        return(list(
          status = 200L,
          headers = list("Content-Type" = "application/json"),
          body = jsonlite::toJSON(result, auto_unbox = TRUE)
        ))
        
      }, error = function(e) {
        return(list(
          status = 400L,
          headers = list("Content-Type" = "application/json"),
          body = jsonlite::toJSON(list(error = paste("Invalid request:", e$message)))
        ))
      })
    },
    
    # Navigate through nested config structure
    get_config_value = function(path_parts) {
      # First part determines if it's patient or machine
      root_type <- path_parts[1]
      remaining_path <- path_parts[-1]
      
      if (root_type == "patient") {
        return(self$navigate_object(self$simulation_engine$patient, remaining_path))
      } else if (root_type == "machine") {
        # strip an optional 'device' path element to keep old URLs working
        if (length(remaining_path) && identical(remaining_path[1], "device")) {
          remaining_path <- remaining_path[-1]
        }
        # Fixed: Use navigate_object for GET requests, not set_nested_value
        return(self$navigate_object(self$simulation_engine$machine, remaining_path))
      } else if (root_type == "simulation") {
        # Special case for simulation metadata
        return(list(
          running = self$simulation_engine$simulation_running,
          current_time = self$simulation_engine$current_time,
          step_interval = self$simulation_engine$step_interval
        ))
      }
      
      return(NULL)
    },
    
    # Set value in config structure
    set_config_value = function(path_parts, value) {
      if (length(path_parts) == 0) {
        return(list(success = FALSE, message = "Empty path"))
      }
      
      root_type <- path_parts[1]
      remaining_path <- path_parts[-1]
      
      tryCatch({
        if (root_type == "patient") {
          res <- self$set_nested_value(self$simulation_engine$patient, remaining_path, value)
          if (!is.null(res$obj)) self$simulation_engine$patient <- res$obj
          return(res$result)
        } else if (root_type == "machine") {
          if (length(remaining_path) && identical(remaining_path[1], "device")) {
            remaining_path <- remaining_path[-1]
          }
          res <- self$set_nested_value(self$simulation_engine$machine, remaining_path, value)
          # for R6 this is by reference; for lists we reassign
          if (!is.null(res$obj)) self$simulation_engine$machine <- res$obj
          return(res$result)
        } else if (root_type == "simulation") {
          # Handle simulation commands directly instead of using set_nested_value
          return(self$handle_simulation_control(remaining_path, value))
        }
        
        return(list(success = FALSE, message = "Invalid root path"))
        
      }, error = function(e) {
        return(list(success = FALSE, message = paste("Error in set_config_value:", e$message)))
      })
    },
    
    handle_simulation_control = function(path_parts, value) {
      if (length(path_parts) == 0) {
        return(list(success = FALSE, message = "No simulation command specified"))
      }
      
      command <- path_parts[1]
      
      # Debug: Check if simulation_engine exists and what type it is
      if (is.null(self$simulation_engine)) {
        return(list(success = FALSE, message = "simulation_engine is NULL"))
      }
      
      cat("Debug: simulation_engine class:", class(self$simulation_engine), "\n")
      cat("Debug: available methods:", paste(names(self$simulation_engine), collapse = ", "), "\n")
      
      tryCatch({
        if (command == "start") {
          cat("Debug: Attempting to call start_simulation\n")
          # Try direct access to the method
          start_method <- self$simulation_engine$start_simulation
          if (is.function(start_method)) {
            cat("Debug: start_simulation is a function, calling it\n")
            start_method()
            return(list(success = TRUE, message = "Simulation started"))
          } else {
            return(list(success = FALSE, message = paste("start_simulation is not a function, it's:", class(start_method))))
          }
        } else if (command == "stop") {
          stop_method <- self$simulation_engine$stop_simulation
          if (is.function(stop_method)) {
            stop_method()
            return(list(success = TRUE, message = "Simulation stopped"))
          } else {
            return(list(success = FALSE, message = paste("stop_simulation is not a function, it's:", class(stop_method))))
          }
        } else if (command == "step_interval") {
          # Handle both empty value and value with "value" key
          new_value <- if (is.list(value) && "value" %in% names(value)) {
            value$value
          } else if (is.numeric(value)) {
            value
          } else {
            return(list(success = FALSE, message = "step_interval requires a numeric value"))
          }
          
          self$simulation_engine$step_interval <- as.numeric(new_value)
          return(list(success = TRUE, message = paste("Set step interval to", new_value)))
        }
        
        return(list(success = FALSE, message = paste("Unknown simulation command:", command)))
        
      }, error = function(e) {
        cat("Debug: Error in handle_simulation_control:", e$message, "\n")
        return(list(success = FALSE, message = paste("Error executing command:", e$message)))
      })
    },
    
    navigate_object = function(obj, path_parts) {
      if (!length(path_parts)) return(obj)
      key <- path_parts[1]; rest <- path_parts[-1]
      
      if (is.R6(obj) || is.list(obj)) {
        if (!(key %in% names(obj))) return(NULL)
        next_obj <- obj[[key]]
        if (!length(rest)) return(next_obj)
        return(self$navigate_object(next_obj, rest))
      }
      NULL
    },
    
    set_nested_value = function(obj, path_parts, value) {
      if (!length(path_parts)) {
        return(list(
          obj = obj,
          result = list(success = FALSE, message = "Empty path")
        ))
      }
      
      key  <- path_parts[1]
      rest <- path_parts[-1]
      
      # Special handling for vaporizer paths
      if (R6::is.R6(obj) && key == "vaporizers" && length(rest) >= 2) {
        agent <- rest[1]
        property <- rest[2]
        
        if (property == "vaporizer_setting") {
          # Use the set_vaporizer_setting method instead of direct assignment
          val <- if (is.list(value) && "value" %in% names(value)) value$value else value
          tryCatch({
            obj$set_vaporizer_setting(agent, val)
            return(list(obj = obj, result = list(success = TRUE, message = paste("Set", agent, "vaporizer setting to", val))))
          }, error = function(e) {
            return(list(obj = obj, result = list(success = FALSE, message = paste("Error setting vaporizer:", e$message))))
          })
        } else if (property == "is_open") {
          # Handle is_open by calling open_vaporizer or close_vaporizer
          val <- if (is.list(value) && "value" %in% names(value)) value$value else value
          tryCatch({
            if (as.logical(val)) {
              obj$open_vaporizer(agent)
            } else {
              obj$close_vaporizer(agent)
            }
            return(list(obj = obj, result = list(success = TRUE, message = paste("Set", agent, "vaporizer open status to", val))))
          }, error = function(e) {
            return(list(obj = obj, result = list(success = FALSE, message = paste("Error setting vaporizer open status:", e$message))))
          })
        }
        # For other vaporizer properties, fall through to normal handling
      }
      
      # final hop: assign/call setter
      if (!length(rest)) {
        val <- if (is.list(value) && "value" %in% names(value)) value$value else value
        
        if (R6::is.R6(obj)) {
          # Special handling for methods that need parameters
          if (key %in% names(obj) && is.function(obj[[key]])) {
            # Get the function to check its parameters
            func <- obj[[key]]
            
            # Special cases for methods that need specific parameter handling
            if (key == "open_vaporizer" || key == "close_vaporizer") {
              # These methods expect an agent name as parameter
              if (is.character(val)) {
                do.call(obj[[key]], list(val))
              } else {
                return(list(obj = obj, result = list(success = FALSE, message = paste("Method", key, "requires agent name as string"))))
              }
            } else if (key == "set_vaporizer_setting") {
              # This method expects (agent, value)
              return(list(obj = obj, result = list(success = FALSE, message = "Use vaporizers/{agent}/vaporizer_setting path instead")))
            } else {
              # Regular setter method with one parameter
              do.call(obj[[key]], list(val))
            }
            return(list(obj = obj, result = list(success = TRUE, message = paste("Called", key, "with", val))))
          } else {
            # Direct field assignment - ensure we keep numeric types
            if (is.numeric(obj[[key]]) && is.character(val)) {
              val <- as.numeric(val)
            }
            obj[[key]] <- val
            return(list(obj = obj, result = list(success = TRUE, message = paste("Set", key, "to", val))))
          }
        }
        
        if (is.list(obj)) {
          # For list assignments, preserve types
          if (key %in% names(obj) && is.numeric(obj[[key]]) && is.character(val)) {
            val <- as.numeric(val)
          }
          obj[[key]] <- val
          return(list(obj = obj, result = list(success = TRUE, message = paste("Set", key, "to", val))))
        }
        
        return(list(obj = obj, result = list(success = FALSE, message = "Invalid target object")))
      }
      
      # recurse
      if (!(R6::is.R6(obj) || is.list(obj))) {
        return(list(obj = obj, result = list(success = FALSE, message = "Invalid path")))
      }
      
      # ensure child exists for lists
      if (is.list(obj) && !(key %in% names(obj))) obj[[key]] <- list()
      
      child <- if (R6::is.R6(obj) || is.list(obj)) obj[[key]] else NULL
      res   <- self$set_nested_value(child, rest, value)
      
      # write back child if list
      if (is.list(obj)) obj[[key]] <- res$obj
      
      return(list(obj = obj, result = res$result))
    },
    
    # Generate API documentation based on current config
    handle_root_request = function() {
      available_endpoints <- list(
        patient = self$get_available_paths(self$simulation_engine$patient, "patient"),
        machine = self$get_available_paths(self$simulation_engine$machine, "machine"),
        simulation = list(
          "simulation/start (POST)",
          "simulation/stop (POST)", 
          "simulation/step_interval (GET/POST)",
          "simulation (GET) - status"
        )
      )
      
      return(list(
        status = 200L,
        headers = list("Content-Type" = "application/json"),
        body = jsonlite::toJSON(list(
          message = "Generic Simulation API",
          available_endpoints = available_endpoints,
          usage = list(
            "GET /{path} - retrieve value at path",
            "POST /{path} - set value at path (send {\"value\": new_value})"
          )
        ), pretty = TRUE, auto_unbox = TRUE)
      ))
    },
    
    # Recursively discover available paths in config
    get_available_paths = function(obj, prefix = "") {
      # If it’s an R6 object, enumerate public members
      if (inherits(obj, "R6")) {
        paths <- list()
        pub_names <- names(obj)
        for (nm in pub_names) {
          # Skip R6 internals
          if (nm %in% c("clone", ".__enclos_env__")) next
          current_path <- if (prefix == "") nm else paste0(prefix, "/", nm)
          val <- tryCatch(obj[[nm]], error = function(e) NULL)
          
          if (is.list(val) || inherits(val, "R6")) {
            nested <- self$get_available_paths(val, current_path)
            if (is.list(nested)) paths <- c(paths, nested) else paths[[length(paths)+1]] <- nested
          } else {
            # leaf: show its class
            paths[[length(paths) + 1]] <- paste0(current_path, " (", class(val)[1], ")")
          }
        }
        return(paths)
      }
      
      # If it’s a plain list, recurse as before
      if (is.list(obj)) {
        paths <- list()
        for (name in names(obj)) {
          current_path <- if (prefix == "") name else paste0(prefix, "/", name)
          if (is.list(obj[[name]]) || inherits(obj[[name]], "R6")) {
            nested_paths <- self$get_available_paths(obj[[name]], current_path)
            if (is.list(nested_paths)) {
              paths <- c(paths, nested_paths)
            } else {
              paths[[length(paths) + 1]] <- nested_paths
            }
          } else {
            paths[[length(paths) + 1]] <- paste0(current_path, " (", class(obj[[name]])[1], ")")
          }
        }
        return(paths)
      }
      
      # Fallback: leaf
      paste0(prefix, " (", class(obj)[1], ")")
    }
  )
)