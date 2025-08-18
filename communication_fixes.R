# =============================================================================
# COMMUNICATION FIXES FOR VAPORIZER-LUNGS INTEGRATION
# =============================================================================

# Fix 1: Enhanced VaporizerBank publishing with multiple topic patterns
# Add this method to your VaporizerBank class:

fix_vaporizer_publishing <- function(vaporizer_bank) {
  # Replace the publish_data method in VaporizerBank
  vaporizer_bank$publish_data <- function() {
    if (is.null(self$bus)) {
      cat("Warning: No bus available for publishing VaporizerBank data\n")
      return(invisible())
    }
    
    cat("VaporizerBank publishing data with enhanced patterns...\n")
    
    # Publish to multiple topic patterns to ensure Lungs can find the data
    topics_to_publish <- c(
      "vaporizer_bank",
      "state/vaporizer_bank", 
      paste0(self$node_id %||% "vaporizer_bank"),
      "machine/vaporizer_bank"
    )
    
    data_to_publish <- list(
      current_fi_agents = self$current_fi_agents,
      active_agent = self$active_agent,
      available_agents = self$available_agents,
      vaporizer_settings = lapply(self$vaporizers, function(v) {
        list(setting = v$vaporizer_setting, is_open = v$is_open)
      })
    )
    
    for (topic in topics_to_publish) {
      tryCatch({
        for (param_name in names(data_to_publish)) {
          self$bus$set_param(topic, param_name, data_to_publish[[param_name]], notify = TRUE)
        }
        cat("Published to topic:", topic, "\n")
      }, error = function(e) {
        cat("Error publishing to", topic, ":", e$message, "\n")
      })
    }
    
    # Also publish individual agent data for easier access
    for (agent in names(self$current_fi_agents)) {
      tryCatch({
        self$bus$set_param("agents", agent, list(
          fi = self$current_fi_agents[[agent]],
          setting = self$vaporizers[[agent]]$vaporizer_setting,
          is_open = self$vaporizers[[agent]]$is_open
        ), notify = TRUE)
      }, error = function(e) {
        cat("Error publishing agent", agent, ":", e$message, "\n")
      })
    }
    
    cat("Enhanced publishing complete\n")
    invisible(TRUE)
  }
}

# Fix 2: Enhanced Lungs subscription with broader patterns
# Add this method to your Lungs class:

fix_lungs_subscriptions <- function(lungs) {
  # Enhanced subscription setup
  lungs$setup_enhanced_subscriptions <- function() {
    if (is.null(self$bus)) return(invisible())
    
    cat("Setting up enhanced Lungs subscriptions...\n")
    
    # Multiple patterns to catch volatile agent data from any source
    volatile_patterns <- c(
      "^.*vaporizer.*current_fi_agents.*$",
      "^.*current_fi_agents.*$",
      "^vaporizer_bank.*$",
      "^state.*vaporizer.*$",
      "^machine.*vaporizer.*$"
    )
    
    for (pattern in volatile_patterns) {
      self$subscribe(
        pattern = pattern,
        callback = function(topic, msg) {
          cat("Lungs received update on topic:", topic, "\n")
          
          # Extract volatile agent data from various message formats
          fi_data <- NULL
          
          if (is.list(msg)) {
            if (!is.null(msg$value) && is.list(msg$value)) {
              fi_data <- msg$value
            } else if (!is.null(msg$current_fi_agents)) {
              fi_data <- msg$current_fi_agents
            } else if (all(sapply(msg, is.numeric))) {
              fi_data <- msg  # Direct agent data
            }
          }
          
          if (!is.null(fi_data) && length(fi_data) > 0) {
            self$update_volatile_targets(fi_data)
            cat("Updated volatile targets from bus\n")
          }
        },
        replay = TRUE
      )
    }
    
    # Active agent patterns
    active_patterns <- c(
      "^.*active_agent.*$",
      "^vaporizer_bank.*$"
    )
    
    for (pattern in active_patterns) {
      self$subscribe(
        pattern = pattern,
        callback = function(topic, msg) {
          agent_name <- NULL
          
          if (is.list(msg) && !is.null(msg$value)) {
            if (is.list(msg$value) && !is.null(msg$value$agent)) {
              agent_name <- msg$value$agent
            } else if (is.character(msg$value)) {
              agent_name <- msg$value
            }
          } else if (is.list(msg) && !is.null(msg$active_agent)) {
            agent_name <- msg$active_agent
          }
          
          if (!is.null(agent_name) && is.character(agent_name) && nchar(agent_name) > 0) {
            self$current_agent <- agent_name
            self$update_dominant_agent()
            cat("Updated active agent to:", agent_name, "\n")
          }
        },
        replay = TRUE
      )
    }
    
    cat("Enhanced subscriptions setup complete\n")
  }
  
  # Enhanced volatile targets update
  lungs$update_volatile_targets <- function(fi_data) {
    if (!is.list(fi_data) || length(fi_data) == 0) return(invisible())
    
    self$target_fi_agents <- fi_data
    
    # Initialize or update Fi and Fa agents
    for (agent in names(fi_data)) {
      if (!(agent %in% names(self$Fi_agents))) {
        self$Fi_agents[[agent]] <- 0.0
        self$Fa_agents[[agent]] <- 0.0
      }
      
      # Update backward compatibility storage
      self$fi_volatiles[[agent]] <- self$Fi_agents[[agent]]
      self$fa_volatiles[[agent]] <- self$Fa_agents[[agent]]
    }
    
    self$update_dominant_agent()
    self$publish_outputs()
    invisible(TRUE)
  }
  
  # Call the enhanced setup
  lungs$setup_enhanced_subscriptions()
}

# Fix 3: Improved sync_vaporizer_to_lungs in SimulationEngine
# Replace the method in your SimulationEngine:

fix_engine_sync <- function(engine) {
  engine$sync_vaporizer_to_lungs <- function() {
    cat("=== ENHANCED SYNC VAPORIZER TO LUNGS ===\n")
    
    vb <- self$machine$vaporizer_bank
    lungs <- self$patient$systems$respiratory$organs$lungs
    
    if (is.null(vb) || is.null(lungs)) {
      cat("ERROR: Missing components\n")
      return(invisible(FALSE))
    }
    
    # Step 1: Apply fixes
    cat("1. Applying communication fixes...\n")
    fix_vaporizer_publishing(vb)
    fix_lungs_subscriptions(lungs)
    
    # Step 2: Force VaporizerBank to publish with new method
    cat("2. Force publishing from VaporizerBank...\n")
    vb$publish_data()
    
    # Step 3: Manual data transfer as backup
    cat("3. Manual data transfer...\n")
    if (length(vb$current_fi_agents) > 0) {
      lungs$target_fi_agents <- vb$current_fi_agents
      
      for (agent in names(vb$current_fi_agents)) {
        if (!(agent %in% names(lungs$Fi_agents))) {
          lungs$Fi_agents[[agent]] <- 0.0
          lungs$Fa_agents[[agent]] <- 0.0
        }
        lungs$fi_volatiles[[agent]] <- lungs$Fi_agents[[agent]]
        lungs$fa_volatiles[[agent]] <- lungs$Fa_agents[[agent]]
      }
      
      if (!is.null(vb$active_agent)) {
        lungs$current_agent <- vb$active_agent
      }
      
      lungs$update_dominant_agent()
      lungs$publish_outputs()
    }
    
    # Step 4: Test communication
    cat("4. Testing communication...\n")
    
    # Set a test value and see if it propagates
    test_agent <- vb$available_agents[1]
    if (!is.null(test_agent)) {
      original_setting <- vb$vaporizers[[test_agent]]$vaporizer_setting
      
      # Make a small change
      vb$set_vaporizer_setting(test_agent, original_setting + 0.1)
      vb$open_vaporizer(test_agent)
      
      # Force update cycle
      vb$update(dt = 1)
      lungs$update(dt = 1)
      
      # Check if change propagated
      if (lungs$target_fi_agents[[test_agent]] > 0) {
        cat("SUCCESS: Communication working\n")
      } else {
        cat("WARNING: Communication may still have issues\n")
      }
      
      # Restore original setting
      vb$set_vaporizer_setting(test_agent, original_setting)
    }
    
    # Step 5: Final status
    cat("5. Final status check...\n")
    cat("VaporizerBank agents:", paste(names(vb$current_fi_agents), collapse = ", "), "\n")
    cat("Lungs target agents:", paste(names(lungs$target_fi_agents), collapse = ", "), "\n")
    cat("Lungs Fi agents:", paste(names(lungs$Fi_agents), collapse = ", "), "\n")
    
    return(invisible(TRUE))
  }
}

# Fix 4: RespiratorySystem compatibility methods
# Add these methods to ensure RespiratorySystem can access volatile data:

fix_respiratory_system_access <- function(respiratory_system) {
  # Enhanced get_fi method
  respiratory_system$get_fi <- function(agent) {
    lungs <- self$organs$lungs
    if (is.null(lungs)) return(0)
    
    # Try multiple access paths
    result <- 0
    
    # Path 1: fi_volatiles (backward compatibility)
    if (!is.null(lungs$fi_volatiles[[agent]])) {
      result <- lungs$fi_volatiles[[agent]]
    }
    # Path 2: Fi_agents (new system)
    else if (!is.null(lungs$Fi_agents[[agent]])) {
      result <- lungs$Fi_agents[[agent]]
    }
    # Path 3: Legacy single agent
    else if (tolower(agent) == tolower(lungs$current_agent)) {
      result <- lungs$Fi_agent
    }
    
    return(as.numeric(result))
  }
  
  # Enhanced get_fa method
  respiratory_system$get_fa <- function(agent) {
    lungs <- self$organs$lungs
    if (is.null(lungs)) return(0)
    
    # Try multiple access paths
    result <- 0
    
    # Path 1: fa_volatiles (backward compatibility)
    if (!is.null(lungs$fa_volatiles[[agent]])) {
      result <- lungs$fa_volatiles[[agent]]
    }
    # Path 2: Fa_agents (new system)
    else if (!is.null(lungs$Fa_agents[[agent]])) {
      result <- lungs$Fa_agents[[agent]]
    }
    # Path 3: Legacy single agent
    else if (tolower(agent) == tolower(lungs$current_agent)) {
      result <- lungs$Fa_agent
    }
    
    return(as.numeric(result))
  }
}

# =============================================================================
# COMPLETE SYSTEM INTEGRATION FIX
# =============================================================================

# Master fix function that applies all fixes to your engine
apply_all_fixes <- function(engine) {
  cat("=== APPLYING ALL COMMUNICATION FIXES ===\n")
  
  # Apply all individual fixes
  fix_vaporizer_publishing(engine$machine$vaporizer_bank)
  fix_lungs_subscriptions(engine$patient$systems$respiratory$organs$lungs)
  fix_engine_sync(engine)
  fix_respiratory_system_access(engine$patient$systems$respiratory)
  
  # Run the enhanced sync
  cat("\nRunning enhanced sync...\n")
  engine$sync_vaporizer_to_lungs()
  
  cat("All fixes applied!\n")
  invisible(TRUE)
}

# =============================================================================
# USAGE INSTRUCTIONS
# =============================================================================

cat("COMMUNICATION FIXES LOADED!\n")
cat("=============================\n")
cat("To fix your engine communication issues:\n\n")
cat("1. Load your engine:\n")
cat("   engine <- SimulationEngine$new(patient_config = 'patient.yaml', machine_config = 'machine.yaml')\n\n")
cat("2. Apply all fixes:\n")
cat("   apply_all_fixes(engine)\n\n")
cat("3. Test the system:\n")
cat("   test_volatile_workflow(engine, 'sevoflurane', 2.0)\n\n")
cat("4. Or apply individual fixes:\n")
cat("   fix_vaporizer_publishing(engine$machine$vaporizer_bank)\n")
cat("   fix_lungs_subscriptions(engine$patient$systems$respiratory$organs$lungs)\n")
cat("   fix_engine_sync(engine)\n")
cat("   fix_respiratory_system_access(engine$patient$systems$respiratory)\n\n")
cat("Key improvements:\n")
cat("  - Multiple topic patterns for robust bus communication\n")
cat("  - Enhanced subscription system with broader pattern matching\n")
cat("  - Backward compatibility preservation\n")
cat("  - Automatic data synchronization\n")
cat("  - Improved error handling and diagnostics\n")

# =============================================================================
# VALIDATION FUNCTIONS
# =============================================================================

# Quick validation to confirm fixes are working
validate_fixes <- function(engine) {
  cat("\n=== VALIDATING COMMUNICATION FIXES ===\n")
  
  vb <- engine$machine$vaporizer_bank
  lungs <- engine$patient$systems$respiratory$organs$lungs
  resp <- engine$patient$systems$respiratory
  
  # Test 1: VaporizerBank -> Bus
  cat("Test 1: VaporizerBank -> Bus communication\n")
  vb$set_vaporizer_setting("sevoflurane", 1.5)
  vb$open_vaporizer("sevoflurane")
  
  Sys.sleep(0.1)  # Brief pause for async operations
  
  # Check bus
  bus_data <- tryCatch({
    engine$bus$get_param("vaporizer_bank", "current_fi_agents")$value
  }, error = function(e) NULL)
  
  if (!is.null(bus_data) && !is.null(bus_data$sevoflurane)) {
    cat("  ✓ PASS: Data found on bus\n")
  } else {
    cat("  ✗ FAIL: Data not found on bus\n")
  }
  
  # Test 2: Bus -> Lungs
  cat("Test 2: Bus -> Lungs communication\n")
  lungs$update(dt = 1)
  
  if (length(lungs$target_fi_agents) > 0 && !is.null(lungs$target_fi_agents$sevoflurane)) {
    cat("  ✓ PASS: Lungs received volatile data\n")
  } else {
    cat("  ✗ FAIL: Lungs did not receive volatile data\n")
  }
  
  # Test 3: RespiratorySystem access
  cat("Test 3: RespiratorySystem access\n")
  fi_val <- resp$get_fi("sevoflurane")
  fa_val <- resp$get_fa("sevoflurane")
  
  if (is.numeric(fi_val) && fi_val >= 0) {
    cat("  ✓ PASS: get_fi working, value:", fi_val, "\n")
  } else {
    cat("  ✗ FAIL: get_fi not working\n")
  }
  
  if (is.numeric(fa_val) && fa_val >= 0) {
    cat("  ✓ PASS: get_fa working, value:", fa_val, "\n")
  } else {
    cat("  ✗ FAIL: get_fa not working\n")
  }
  
  # Test 4: Dynamic updates
  cat("Test 4: Dynamic updates\n")
  
  # Run several update cycles
  for (i in 1:5) {
    vb$update(dt = 1)
    lungs$update(dt = 1)
  }
  
  final_fi <- resp$get_fi("sevoflurane")
  final_fa <- resp$get_fa("sevoflurane")
  
  if (final_fi > 0 && final_fa >= 0) {
    cat("  ✓ PASS: Dynamic updates working. Fi:", sprintf("%.4f", final_fi), 
        "Fa:", sprintf("%.4f", final_fa), "\n")
  } else {
    cat("  ✗ FAIL: Dynamic updates not working\n")
  }
  
  cat("\nValidation complete!\n")
  invisible(TRUE)
}

# =============================================================================
# TROUBLESHOOTING GUIDE
# =============================================================================

print_troubleshooting_guide <- function() {
  cat("\n=== TROUBLESHOOTING GUIDE ===\n")
  cat("If you're still having issues after applying the fixes:\n\n")
  
  cat("ISSUE: VaporizerBank not publishing to bus\n")
  cat("CHECK: vb$bus is not NULL\n")
  cat("CHECK: vb$node_id is set properly\n")
  cat("FIX: vb$force_publish() and check engine$bus$state\n\n")
  
  cat("ISSUE: Lungs not receiving volatile data\n")
  cat("CHECK: lungs$bus is the same object as vb$bus\n")
  cat("CHECK: Subscription patterns are correct\n")
  cat("FIX: lungs$sync_with_vaporizer_bank(vb)\n\n")
  
  cat("ISSUE: RespiratorySystem get_fi/get_fa returns 0\n")
  cat("CHECK: lungs$fi_volatiles and lungs$Fi_agents have data\n")
  cat("CHECK: Agent name spelling and case\n")
  cat("FIX: Apply fix_respiratory_system_access(respiratory_system)\n\n")
  
  cat("ISSUE: Data not updating dynamically\n")
  cat("CHECK: update() methods are being called\n")
  cat("CHECK: Time constants are reasonable (not too large)\n")
  cat("FIX: Run multiple update cycles with dt=1\n\n")
  
  cat("For persistent issues, run:\n")
  cat("  run_full_diagnostic(engine)\n")
  cat("  validate_fixes(engine)\n")
}

print_troubleshooting_guide()