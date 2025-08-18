VaporizerBank <- R6Class(
  "VaporizerBank",
  inherit = DevicePart,
  public = list(
    # Vaporizer slots - typically 2 slots on real machines
    max_slots = 2,
    
    # Vaporizer configuration: agent_name -> {setting, is_open, response_tau, decay_tau, slot_number}
    vaporizers = list(),
    
    # Current delivered fractions (smoothed)
    current_fi_agents = list(),
    
    # Track which agents are available (for validation)
    available_agents = character(0),
    
    # ADDED: Track active agent name for easier access
    active_agent = NULL,
    
    initialize = function(parent_device = NULL, settings = list(), node_id = NULL, bus = NULL) {
      super$initialize("vaporizer_bank", parent_device, node_id, bus)
      
      # Get max slots from settings or use default
      self$max_slots <- settings$max_slots %||% 2
      
      # Determine which agents to install
      agents_to_install <- self$determine_agents_from_settings(settings)
      
      # Install the vaporizers
      self$install_vaporizers(agents_to_install, settings)
      
      # Initialize current fractions
      self$initialize_agent_fractions()
      
      # FIXED: Ensure we have a proper node_id for bus publishing
      if (is.null(self$node_id)) {
        self$node_id <- "vaporizer_bank"
      }
      
      # FIXED: Initial publishing to establish bus data
      self$publish_data()
      
      cat("VaporizerBank initialized with agents:", paste(names(self$vaporizers), collapse = ", "), "\n")
      cat("Initial bus publication completed\n")
    },
    
    # === CONFIGURATION HELPERS ===
    
    determine_agents_from_settings = function(settings) {
      if (!is.null(settings$agents) && is.character(settings$agents)) {
        # Explicit agent list: agents: ["sevoflurane", "desflurane"]
        return(settings$agents)
      } else if (!is.null(settings$vaporizers) && is.list(settings$vaporizers)) {
        # Agent list from vaporizer config keys: vaporizers: {sevoflurane: {...}, isoflurane: {...}}
        return(names(settings$vaporizers))
      } else {
        # Default combination - most common in practice
        return(c("sevoflurane", "desflurane"))
      }
    },
    
    install_vaporizers = function(agents, settings) {
      if (length(agents) > self$max_slots) {
        warning(paste("Requested", length(agents), "agents but only", self$max_slots, 
                      "slots available. Installing first", self$max_slots))
        agents <- agents[1:self$max_slots]
      }
      
      self$available_agents <- agents
      slot_number <- 1
      
      for (agent in agents) {
        # Default vaporizer configuration
        default_config <- list(
          vaporizer_setting = 0.0,
          is_open = FALSE,
          response_tau = 5.0,
          residual_decay_tau = 60.0,
          slot_number = slot_number
        )
        
        # Merge with any agent-specific settings from config
        if (!is.null(settings$vaporizers) && !is.null(settings$vaporizers[[agent]])) {
          agent_config <- utils::modifyList(default_config, settings$vaporizers[[agent]])
        } else {
          agent_config <- default_config
        }
        
        # Ensure slot number is set
        agent_config$slot_number <- slot_number
        
        self$vaporizers[[agent]] <- agent_config
        slot_number <- slot_number + 1
        
        cat("Installed", agent, "vaporizer in slot", agent_config$slot_number, "\n")
      }
    },
    
    initialize_agent_fractions = function() {
      for (agent in names(self$vaporizers)) {
        v <- self$vaporizers[[agent]]
        # Initialize to setting if open, otherwise 0
        self$current_fi_agents[[agent]] <- if (v$is_open) v$vaporizer_setting / 100 else 0.0
      }
      
      # Update active agent
      self$update_active_agent()
    },
    
    # ADDED: Update active agent tracking
    update_active_agent = function() {
      self$active_agent <- NULL
      for (agent in self$available_agents) {
        if (isTRUE(self$vaporizers[[agent]]$is_open)) {
          self$active_agent <- agent
          break
        }
      }
    },
    
    # === SETTERS (callable via HTTP API) ===
    
    set_vaporizer_setting = function(agent, value) {
      agent <- tolower(as.character(agent))
      
      # Validate agent is installed
      if (!(agent %in% self$available_agents)) {
        available <- paste(self$available_agents, collapse = ", ")
        stop(paste("Agent", agent, "not installed. Available agents:", available))
      }
      
      # Set within reasonable bounds (0-8% is typical for volatile agents)
      self$vaporizers[[agent]]$vaporizer_setting <- max(0, min(8, as.numeric(value)))
      
      cat("Set", agent, "vaporizer to", self$vaporizers[[agent]]$vaporizer_setting, "%\n")
      
      # FIXED: Publish immediately after changes
      self$publish_data()
      
      invisible(TRUE)
    },
    
    open_vaporizer = function(agent) {
      agent <- tolower(as.character(agent))
      
      if (!(agent %in% self$available_agents)) {
        available <- paste(self$available_agents, collapse = ", ")
        stop(paste("Agent", agent, "not installed. Available agents:", available))
      }
      
      # INTERLOCK: Close all other vaporizers first (safety requirement)
      for (ag in names(self$vaporizers)) {
        self$vaporizers[[ag]]$is_open <- FALSE
      }
      
      # Open the requested vaporizer
      self$vaporizers[[agent]]$is_open <- TRUE
      
      # Update active agent
      self$update_active_agent()
      
      # Log which slot is now active
      slot_num <- self$vaporizers[[agent]]$slot_number
      cat("Opened", agent, "vaporizer in slot", slot_num, "- all others closed due to interlock\n")
      
      # FIXED: Publish immediately after changes
      self$publish_data()
      
      invisible(TRUE)
    },
    
    close_vaporizer = function(agent) {
      agent <- tolower(as.character(agent))
      
      if (!(agent %in% self$available_agents)) {
        available <- paste(self$available_agents, collapse = ", ")
        stop(paste("Agent", agent, "not installed. Available agents:", available))
      }
      
      self$vaporizers[[agent]]$is_open <- FALSE
      
      # Update active agent
      self$update_active_agent()
      
      slot_num <- self$vaporizers[[agent]]$slot_number
      cat("Closed", agent, "vaporizer in slot", slot_num, "\n")
      
      # FIXED: Publish immediately after changes
      self$publish_data()
      
      invisible(TRUE)
    },
    
    close_all_vaporizers = function() {
      for (agent in names(self$vaporizers)) {
        self$vaporizers[[agent]]$is_open <- FALSE
      }
      
      # Update active agent
      self$update_active_agent()
      
      cat("Closed all vaporizers\n")
      
      # FIXED: Publish immediately after changes
      self$publish_data()
      
      invisible(TRUE)
    },
    
    # === UPDATE LOGIC ===
    
    update = function(dt) {
      # Update each installed agent's delivered fraction
      for (agent in self$available_agents) {
        if (agent %in% names(self$current_fi_agents)) {
          current_fi <- self$current_fi_agents[[agent]]
          new_fi <- self$update_agent_fraction(current_fi, agent, dt)
          self$current_fi_agents[[agent]] <- new_fi
        }
      }
      
      # Publish updated data
      self$publish_data()
      invisible(TRUE)
    },
    
    update_agent_fraction = function(current_fi, agent, dt) {
      v <- self$vaporizers[[agent]]
      if (is.null(v)) return(0)
      
      if (isTRUE(v$is_open)) {
        # Vaporizer is open: approach the dialed setting
        target_fi <- (v$vaporizer_setting %||% 0) / 100  # Convert % to fraction
        alpha_val <- self$alpha(dt, v$response_tau %||% 5)
        new_val <- current_fi + (target_fi - current_fi) * alpha_val
        return(new_val)
      } else {
        # Vaporizer is closed: decay toward zero (residual washout)
        alpha_val <- self$alpha(dt, v$residual_decay_tau %||% 60)
        new_val <- current_fi + (0 - current_fi) * alpha_val
        return(new_val)
      }
    },
    
    # === INTERFACE FOR OTHER COMPONENTS ===
    
    get_volatile_agent_composition = function() {
      # Return current fractions for all installed agents
      result <- list()
      for (agent in self$available_agents) {
        result[[agent]] <- self$current_fi_agents[[agent]] %||% 0
      }
      return(result)
    },
    
    get_total_volatile_concentration = function() {
      # Sum of all volatile agents (useful for some calculations)
      total <- 0
      for (agent in self$available_agents) {
        total <- total + (self$current_fi_agents[[agent]] %||% 0)
      }
      return(total)
    },
    
    get_active_agent = function() {
      # Return which agent is currently open (if any)
      if (!is.null(self$active_agent)) {
        return(list(
          agent = self$active_agent,
          slot = self$vaporizers[[self$active_agent]]$slot_number,
          setting = self$vaporizers[[self$active_agent]]$vaporizer_setting,
          current_fi = self$current_fi_agents[[self$active_agent]] %||% 0
        ))
      }
      return(NULL)  # No agent is open
    },
    
    # === DIAGNOSTICS & STATUS ===
    
    get_bank_status = function() {
      list(
        max_slots = self$max_slots,
        installed_agents = self$available_agents,
        slots_used = length(self$available_agents),
        active_agent = self$get_active_agent()
      )
    },
    
    get_vaporizer_status = function() {
      result <- list()
      for (agent in self$available_agents) {
        v <- self$vaporizers[[agent]]
        result[[agent]] <- list(
          slot_number = v$slot_number,
          vaporizer_setting_percent = v$vaporizer_setting,
          is_open = v$is_open,
          current_fi = self$current_fi_agents[[agent]] %||% 0,
          target_fi = if (v$is_open) v$vaporizer_setting / 100 else 0,
          response_tau = v$response_tau,
          residual_decay_tau = v$residual_decay_tau
        )
      }
      return(result)
    },
    
    list_available_agents = function() {
      return(self$available_agents)
    },
    
    # === UTILITIES ===
    
    alpha = function(dt, tau) {
      1 - exp(-as.numeric(dt) / max(1e-6, as.numeric(tau)))
    },
    
    # FIXED: Enhanced publishing with multiple methods
    publish_data = function() {
      if (is.null(self$bus)) {
        cat("Warning: No bus available for publishing VaporizerBank data\n")
        return(invisible())
      }
      
      # Method 1: Use the standard publish_params (if available from DevicePart)
      if (exists("publish_params", where = self, inherits = TRUE)) {
        tryCatch({
          self$publish_params(list(
            component = "vaporizer_bank",
            max_slots = self$max_slots,
            installed_agents = self$available_agents,
            active_agent = self$get_active_agent(),
            vaporizers = self$vaporizers,
            current_fi_agents = self$current_fi_agents,
            total_volatile_fi = self$get_total_volatile_concentration()
          ))
        }, error = function(e) {
          cat("Error in publish_params:", e$message, "\n")
        })
      }
      
      # Method 2: Direct bus parameter setting with multiple topic patterns
      tryCatch({
        # Publish under vaporizer_bank scope
        self$bus$set_param(self$node_id, "current_fi_agents", self$current_fi_agents, notify = TRUE)
        self$bus$set_param(self$node_id, "active_agent", self$active_agent, notify = TRUE)
        self$bus$set_param(self$node_id, "available_agents", self$available_agents, notify = TRUE)
      }, error = function(e) {
        cat("Error publishing to bus:", e$message, "\n")
      })
      
      invisible(TRUE)
    },
    
    get_current_fi_agents = function() {
      # Always return a plain named list of numerics
      lapply(self$current_fi_agents, as.numeric)
    }
  )
)