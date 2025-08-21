# Fixed HTTPController with stack overflow prevention and better R6 handling

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
      
      # GET /machine/parts  -> list of parts (name, type, enabled)
      if (length(path_parts) == 2 && identical(path_parts, c("machine","parts"))) {
        mg <- self$simulation_engine$machine
        if (is.null(mg) || !is.list(mg$parts)) {
          return(list(
            status = 404L,
            headers = list("Content-Type" = "application/json"),
            body = jsonlite::toJSON(list(error = "No parts attached"), auto_unbox = TRUE)
          ))
        }
        out <- lapply(names(mg$parts), function(nm) {
          p <- mg$parts[[nm]]
          list(name = nm,
               type = class(p)[1],
               enabled = tryCatch(isTRUE(p$enabled), error = function(e) NA))
        })
        return(list(
          status = 200L,
          headers = list("Content-Type" = "application/json"),
          body = jsonlite::toJSON(out, auto_unbox = TRUE)
        ))
      }
      
      # GET /machine/parts/{name} -> summary_state() of a single part
      if (length(path_parts) == 3 && identical(path_parts[1:2], c("machine","parts"))) {
        mg <- self$simulation_engine$machine
        nm <- path_parts[3]
        if (is.null(mg) || is.null(mg$parts[[nm]])) {
          return(list(
            status = 404L,
            headers = list("Content-Type" = "application/json"),
            body = jsonlite::toJSON(list(error = paste("Part not found:", nm)), auto_unbox = TRUE)
          ))
        }
        s <- tryCatch(mg$parts[[nm]]$summary_state(),
                      error = function(e) list(error = e$message))
        return(list(
          status = 200L,
          headers = list("Content-Type" = "application/json"),
          body = jsonlite::toJSON(s, auto_unbox = TRUE)
        ))
      }
      
      # GET /machine/compartments -> combined snapshot (mode, volumes, rebreathing, per-part)
      if (length(path_parts) == 2 && identical(path_parts, c("machine","compartments"))) {
        mg <- self$simulation_engine$machine
        if (is.null(mg) || !is.function(mg$get_compartments_snapshot)) {
          return(list(
            status = 404L,
            headers = list("Content-Type" = "application/json"),
            body = jsonlite::toJSON(list(error = "compartments not available"), auto_unbox = TRUE)
          ))
        }
        snap <- mg$get_compartments_snapshot()
        return(list(
          status = 200L,
          headers = list("Content-Type" = "application/json"),
          body = jsonlite::toJSON(snap, auto_unbox = TRUE)
        ))
      }
      
      # fast-path GET /machine/gas
      if (length(path_parts) == 2 && identical(path_parts[1], "machine") && identical(path_parts[2], "gas")) {
        mg <- self$simulation_engine$machine
        if (is.null(mg) || !is.function(mg$get_fresh_gas)) {
          return(list(
            status = 404L,
            headers = list("Content-Type" = "application/json"),
            body = jsonlite::toJSON(list(error = "machine/gas not available"))
          ))
        }
        gas <- mg$get_fresh_gas()
        return(list(
          status = 200L,
          headers = list("Content-Type" = "application/json"),
          body = jsonlite::toJSON(gas, auto_unbox = TRUE)
        ))
      }
      
      # fast-path: GET /machine/presets -> list available presets
      if (length(path_parts) == 2 &&
          identical(path_parts[1], "machine") &&
          identical(path_parts[2], "presets")) {
        
        mg <- self$simulation_engine$machine
        if (is.null(mg) || !is.function(mg$list_presets)) {
          return(list(
            status = 404L,
            headers = list("Content-Type" = "application/json"),
            body = jsonlite::toJSON(list(error = "No presets available"), auto_unbox = TRUE)
          ))
        }
        names <- tryCatch(mg$list_presets(), error=function(e) character(0))
        return(list(
          status = 200L,
          headers = list("Content-Type" = "application/json"),
          body = jsonlite::toJSON(names, auto_unbox = TRUE)
        ))
      }
      
      # fast-path GET /machine/flows
      if (length(path_parts) == 2 &&
          identical(path_parts[1], "machine") &&
          identical(path_parts[2], "flows")) {
        mg <- self$simulation_engine$machine
        if (is.null(mg) || !is.function(mg$get_flows)) {
          return(list(
            status = 404L,
            headers = list("Content-Type" = "application/json"),
            body = jsonlite::toJSON(list(error = "machine/flows not available"))
          ))
        }
        flows <- mg$get_flows()
        return(list(
          status = 200L,
          headers = list("Content-Type" = "application/json"),
          body = jsonlite::toJSON(flows, auto_unbox = TRUE)
        ))
      }
      
      if (length(path_parts) == 2 && identical(path_parts, c("machine","mode"))) {
        md <- self$simulation_engine$machine$mode
        lab <- tryCatch(md$label, error = function(e) "unknown")
        return(list(
          status = 200L,
          headers = list("Content-Type" = "application/json"),
          body = jsonlite::toJSON(list(label = lab), auto_unbox = TRUE)
        ))
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
        
        # --- SPECIAL MACHINE COMMANDS (flows etc.) ---
        special <- self$handle_machine_command(path_parts, post_data)
        if (!is.null(special)) {
          return(list(
            status = 200L,
            headers = list("Content-Type" = "application/json"),
            body = jsonlite::toJSON(special, auto_unbox = TRUE)
          ))
        }
        # --- END SPECIALS ---
        
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
      
      if (is.null(self$simulation_engine)) {
        return(list(success = FALSE, message = "simulation_engine is NULL"))
      }
      
      tryCatch({
        if (command == "start") {
          if (isTRUE(self$simulation_engine$simulation_running)) {
            return(list(
              success = TRUE,
              already_running = TRUE,
              message = "Simulation already running"
              # http_status = 200  # keep 200, but you could set 409 if you wanted
            ))
          }
          # do start
          start_method <- self$simulation_engine$start_simulation
          if (!is.function(start_method)) {
            return(list(success = FALSE, message = paste("start_simulation is not a function, it's:", class(start_method))))
          }
          start_method()
          return(list(success = TRUE, started = TRUE, message = "Simulation started"))
          
        } else if (command == "stop") {
          if (!isTRUE(self$simulation_engine$simulation_running)) {
            return(list(
              success = TRUE,
              already_stopped = TRUE,
              message = "Simulation already stopped"
              # http_status = 200
            ))
          }
          # do stop
          stop_method <- self$simulation_engine$stop_simulation
          if (!is.function(stop_method)) {
            return(list(success = FALSE, message = paste("stop_simulation is not a function, it's:", class(stop_method))))
          }
          stop_method()
          return(list(success = TRUE, stopped = TRUE, message = "Simulation stopped"))
          
        } else if (command == "step_interval") {
          new_value <- if (is.list(value) && "value" %in% names(value)) value$value else value
          if (!is.numeric(new_value)) {
            return(list(success = FALSE, message = "step_interval requires a numeric value"))
          }
          self$simulation_engine$step_interval <- as.numeric(new_value)
          return(list(success = TRUE, message = paste("Set step interval to", new_value)))
        }
        
        list(success = FALSE, message = paste("Unknown simulation command:", command))
      }, error = function(e) {
        list(success = FALSE, message = paste("Error executing command:", e$message))
      })
    },
    
    handle_machine_command = function(path_parts, post_data) {
      # Returns a plain R list (content) on handled paths, or NULL if not handled.
      # handle_post_request will wrap this into a proper HTTP response.
      
      if (length(path_parts) == 0) return(NULL)
      if (!identical(path_parts[1], "machine")) return(NULL)
      
      # /machine/flows  (POST)  {o2, air, n2o}
      if (length(path_parts) == 2 && identical(path_parts[2], "flows")) {
        mg <- self$simulation_engine$machine
        if (is.null(mg)) return(list(success=FALSE, message="machine not available"))
        
        o2  <- as.numeric(post_data$o2  %||% mg$o2_flow)
        air <- as.numeric(post_data$air %||% mg$air_flow)
        n2o <- as.numeric(post_data$n2o %||% mg$n2o_flow)
        
        if (is.function(mg$set_o2_flow))  mg$set_o2_flow(o2)
        if (is.function(mg$set_air_flow)) mg$set_air_flow(air)
        if (is.function(mg$set_n2o_flow)) mg$set_n2o_flow(n2o)
        
        # Let the device update its displayed FiO2 on next tick, but also
        # provide the immediate recompute here for convenience if available.
        fio2 <- tryCatch(mg$compute_fio2_from_flows(), error=function(e) mg$current_fio2 %||% NA_real_)
        
        return(list(
          success = TRUE,
          message = "flows applied",
          flows   = list(o2=o2, air=air, n2o=n2o),
          fio2    = fio2
        ))
      }
      
      # /machine/apply_preset  (POST) {name: "preoxygenation"}
      if (length(path_parts) == 2 && identical(path_parts[2], "apply_preset")) {
        mg <- self$simulation_engine$machine
        if (is.null(mg)) return(list(success=FALSE, message="machine not available"))
        
        nm <- post_data$name %||% post_data$preset %||% NULL
        if (is.null(nm) || !nzchar(nm)) {
          return(list(success=FALSE, message="missing preset name"))
        }
        
        res <- tryCatch({
          out <- mg$apply_preset(nm)   # we’ll make this return a list (section 2)
          if (is.null(out) || isTRUE(out)) out <- list(ok=TRUE)  # defensive
          list(success=TRUE, applied=nm, details=out)
        }, error=function(e) {
          list(success=FALSE, message=paste("apply_preset failed:", e$message))
        })
        
        return(res)
      }
      
      # Not handled here
      NULL
    },
    
    # FIXED: Safe navigation with recursion depth limit and simple cycle detection
    navigate_object = function(obj, path_parts, depth = 0, max_depth = 10) {
      # Prevent stack overflow with depth limit
      if (depth > max_depth) {
        return(list(error = "Maximum navigation depth exceeded"))
      }
      
      # If no more path parts, return the current object (but safely serialize it)
      if (!length(path_parts)) {
        return(self$safe_serialize_object(obj, depth))
      }
      
      key <- path_parts[1]
      rest <- path_parts[-1]
      
      if (is.R6(obj)) {
        # For R6 objects, check if the key exists in public fields
        if (key %in% names(obj)) {
          next_obj <- tryCatch(obj[[key]], error = function(e) NULL)
          if (!is.null(next_obj)) {
            if (!length(rest)) {
              return(self$safe_serialize_object(next_obj, depth + 1))
            }
            return(self$navigate_object(next_obj, rest, depth + 1))
          }
        }
        
        # Check if it's a method we should not navigate into
        if (key %in% c("clone", "initialize", ".__enclos_env__")) {
          return(NULL)
        }
        
        return(NULL)
      } else if (is.list(obj)) {
        if (!(key %in% names(obj))) return(NULL)
        next_obj <- obj[[key]]
        if (!length(rest)) {
          return(self$safe_serialize_object(next_obj, depth + 1))
        }
        return(self$navigate_object(next_obj, rest, depth + 1))
      }
      
      NULL
    },
    
    # ADDED: Safe object serialization to prevent stack overflow in JSON conversion
    safe_serialize_object = function(obj, depth = 0, max_depth = 5) {
      if (depth > max_depth) {
        return(paste("Object too deep (", class(obj)[1], ")"))
      }
      
      if (is.null(obj)) return(NULL)
      if (is.atomic(obj) && length(obj) <= 100) return(obj)  # Limit large vectors
      if (is.function(obj)) return("function")
      
      if (is.R6(obj)) {
        # For R6 objects, return a summary instead of full object to prevent recursion
        result <- list()
        pub_names <- names(obj)
        
        # Only include safe, non-function fields
        hidden <- c(".__enclos_env__", "initialize", "clone", "initialize", "subscriptions",
                    "bus", "node_id", "parent_device", "patient", "organs", "systems")
        safe_fields <- setdiff(pub_names, hidden)
        safe_fields <- head(safe_fields, 30)  # Limit number of fields
        
        for (nm in safe_fields) {
          val <- tryCatch(obj[[nm]], error = function(e) paste("Error:", e$message))
          
          if (is.function(val)) {
            result[[nm]] <- "function"
          } else if (is.atomic(val) && length(val) <= 10) {
            result[[nm]] <- val
          } else if (is.list(val) && length(val) <= 20 && depth < max_depth) {
            result[[nm]] <- self$safe_serialize_object(val, depth + 1)
          } else if (is.R6(val)) {
            result[[nm]] <- paste("R6 object:", class(val)[1])
          } else {
            result[[nm]] <- paste("Complex object:", class(val)[1], "length:", length(val))
          }
        }
        return(result)
      }
      
      if (is.list(obj) && length(obj) <= 50) {  # Limit large lists
        result <- list()
        obj_names <- head(names(obj), 25)  # Limit items shown
        for (name in obj_names) {
          result[[name]] <- self$safe_serialize_object(obj[[name]], depth + 1)
        }
        return(result)
      }
      
      # Fallback for other object types
      paste("Object:", class(obj)[1], "length:", length(obj))
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
      
      if (R6::is.R6(obj) && key == "presets_path" && !length(rest)) {
        val <- if (is.list(value) && "value" %in% names(value)) value$value else value
        obj$set_presets_path(as.character(val))
        return(list(obj = obj, result = list(success = TRUE, message = paste("Set presets_path to", val))))
      }
      
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
      
      # protect mode/label from direct writes
      if (R6::is.R6(obj) && key == "mode" && length(rest) == 1 && identical(rest[1], "label")) {
        return(list(obj = obj,
                    result = list(success = FALSE,
                                  message = "mode/label is read-only; use /machine/connect/* endpoints")))
      }
      
      # final hop: assign/call setter
      if (!length(rest)) {
        val <- if (is.list(value) && "value" %in% names(value)) value$value else value
        
        if (R6::is.R6(obj)) {
          # Check if it's a systems collection (like in Patient)
          if (key == "systems" && is.list(obj$systems)) {
            return(list(obj = obj, result = list(success = FALSE, message = "Cannot directly modify systems collection")))
          }
          
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
            if (key %in% names(obj)) {
              if (is.numeric(obj[[key]]) && is.character(val)) {
                val <- as.numeric(val)
              }
              obj[[key]] <- val
              return(list(obj = obj, result = list(success = TRUE, message = paste("Set", key, "to", val))))
            } else {
              return(list(obj = obj, result = list(success = FALSE, message = paste("Field", key, "not found in", class(obj)[1]))))
            }
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
      
      # For R6 objects, check systems collection
      if (R6::is.R6(obj) && key == "systems") {
        if (length(rest) > 0) {
          system_name <- rest[1]
          if (system_name %in% names(obj$systems)) {
            child <- obj$systems[[system_name]]
            res <- self$set_nested_value(child, rest[-1], value)
            # Don't reassign systems - R6 objects are by reference
            return(list(obj = obj, result = res$result))
          } else {
            return(list(obj = obj, result = list(success = FALSE, message = paste("System", system_name, "not found"))))
          }
        }
      }
      
      # ensure child exists for lists
      if (is.list(obj) && !(key %in% names(obj))) obj[[key]] <- list()
      
      child <- if (R6::is.R6(obj) || is.list(obj)) obj[[key]] else NULL
      if (is.null(child)) {
        return(list(obj = obj, result = list(success = FALSE, message = paste("Cannot navigate to", key))))
      }
      
      res   <- self$set_nested_value(child, rest, value)
      
      # write back child if list
      if (is.list(obj)) obj[[key]] <- res$obj
      
      return(list(obj = obj, result = res$result))
    },
    
    # FIXED: Generate API documentation with stack overflow prevention
    handle_root_request = function() {
      tryCatch({
        available_endpoints <- list(
          patient = self$get_available_paths_safe(self$simulation_engine$patient, "patient"),
          machine = c(
            self$get_available_paths_safe(self$simulation_engine$machine, "machine"),
            # Friendly high-level endpoints:
            "machine/flows (POST: {o2, air, n2o})",
            "machine/gas (GET)",
            "machine/flows (GET/POST)",
            "machine/connect/controlled (POST)",
            "machine/connect/manual_mask (POST: {mask_seal})",
            "machine/disconnect/room_air (POST)",
            # Vaporizer shortcuts (already working):
            "machine/vaporizer_bank/vaporizers/{agent}/vaporizer_setting (POST: {value})",
            "machine/vaporizer_bank/vaporizers/{agent}/is_open (POST: {value:true|false})",
            "machine/presets (GET) - list available preset names",
            "machine/apply_preset (POST) - body: {\"name\":\"preoxygenation\" [, \"file\":\"path.yml\"] }",
            "machine/parts (GET) - list attached parts",
            "machine/parts/{name} (GET) - detail for a part",
            "machine/compartments (GET) - effective volumes & rebreathing snapshot"
          ),
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
      }, error = function(e) {
        return(list(
          status = 500L,
          headers = list("Content-Type" = "application/json"),
          body = jsonlite::toJSON(list(error = paste("Error generating documentation:", e$message)))
        ))
      })
    },
    
    # FIXED: Safe path discovery with depth protection
    get_available_paths_safe = function(obj, prefix = "", depth = 0, max_depth = 3) {
      if (depth > max_depth) {
        return(paste(prefix, "(max depth reached)"))
      }
      
      if (is.null(obj)) {
        return(paste(prefix, "(null)"))
      }
      
      # Handle R6 objects
      if (is.R6(obj)) {
        paths <- list()
        pub_names <- names(obj)
        
        # Limit number of fields to prevent excessive output
        safe_names <- setdiff(pub_names, c("clone", "initialize", ".__enclos_env__"))
        safe_names <- head(safe_names, 15)  # Limit to first 15 fields
        
        for (nm in safe_names) {
          current_path <- if (prefix == "") nm else paste0(prefix, "/", nm)
          
          val <- tryCatch(obj[[nm]], error = function(e) NULL)
          if (is.null(val)) next
          
          if (is.function(val)) {
            paths[[length(paths) + 1]] <- paste0(current_path, " (function)")
          } else if (is.atomic(val) && length(val) <= 5) {
            paths[[length(paths) + 1]] <- paste0(current_path, " (", class(val)[1], ")")
          } else if ((is.list(val) || is.R6(val)) && depth < max_depth) {
            nested <- self$get_available_paths_safe(val, current_path, depth + 1)
            if (is.list(nested) && length(nested) > 0) {
              paths <- c(paths, nested)
            } else if (is.character(nested)) {
              paths[[length(paths) + 1]] <- nested
            }
          } else {
            paths[[length(paths) + 1]] <- paste0(current_path, " (", class(val)[1], ")")
          }
          
          # Prevent too many paths
          if (length(paths) > 40) break
        }
        return(paths)
      }
      
      # Handle lists
      if (is.list(obj)) {
        paths <- list()
        obj_names <- head(names(obj), 10)  # Limit list exploration
        
        for (name in obj_names) {
          current_path <- if (prefix == "") name else paste0(prefix, "/", name)
          if (is.list(obj[[name]]) || is.R6(obj[[name]])) {
            if (depth < max_depth) {
              nested_paths <- self$get_available_paths_safe(obj[[name]], current_path, depth + 1)
              if (is.list(nested_paths)) {
                paths <- c(paths, nested_paths)
              } else {
                paths[[length(paths) + 1]] <- nested_paths
              }
            } else {
              paths[[length(paths) + 1]] <- paste0(current_path, " (", class(obj[[name]])[1], " - max depth)")
            }
          } else {
            paths[[length(paths) + 1]] <- paste0(current_path, " (", class(obj[[name]])[1], ")")
          }
          
          if (length(paths) > 40) break
        }
        return(paths)
      }
      
      # Fallback
      paste0(prefix, " (", class(obj)[1], ")")
    }
  )
)