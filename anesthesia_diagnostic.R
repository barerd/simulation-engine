# Anesthesia Simulation Diagnostic and Testing Tool
# This script helps diagnose communication issues between VaporizerBank and Lungs

# =============================================================================
# DIAGNOSTIC FUNCTIONS
# =============================================================================

#' Comprehensive Bus State Inspector
#' Examines all topics and data on the message bus
inspect_bus_state <- function(engine) {
  cat("=== BUS STATE INSPECTION ===\n")
  
  if (is.null(engine$bus)) {
    cat("ERROR: Engine has no bus!\n")
    return(invisible(FALSE))
  }
  
  if (is.null(engine$bus$state)) {
    cat("ERROR: Bus has no state!\n")
    return(invisible(FALSE))
  }
  
  cat("Bus scopes found:", length(engine$bus$state), "\n")
  
  for (scope in names(engine$bus$state)) {
    scope_data <- engine$bus$state[[scope]]
    cat("\n[SCOPE:", scope, "] - ", length(scope_data), "parameters\n")
    
    if (grepl("vaporizer", scope, ignore.case = TRUE)) {
      cat("  ** VAPORIZER SCOPE **\n")
      for (param in names(scope_data)) {
        value <- scope_data[[param]]
        if (is.list(value) && length(value) > 0) {
          cat("    ", param, ":", paste(names(value), "=", 
                                        sapply(value, function(x) sprintf("%.4f", as.numeric(x))), 
                                        collapse = ", "), "\n")
        } else {
          cat("    ", param, ":", deparse(value), "\n")
        }
      }
    } else {
      # Show only relevant parameters for other scopes
      relevant_params <- names(scope_data)[grepl("fi|fa|agent|volatile", names(scope_data), ignore.case = TRUE)]
      if (length(relevant_params) > 0) {
        for (param in relevant_params) {
          cat("    ", param, ":", deparse(scope_data[[param]]), "\n")
        }
      } else {
        cat("    (", paste(head(names(scope_data), 3), collapse = ", "), "...)\n")
      }
    }
  }
  
  invisible(TRUE)
}

#' VaporizerBank Status Check
#' Examines the current state of the vaporizer bank
check_vaporizer_status <- function(engine) {
  cat("\n=== VAPORIZER BANK STATUS ===\n")
  
  vb <- engine$machine$vaporizer_bank
  if (is.null(vb)) {
    cat("ERROR: No VaporizerBank found!\n")
    return(invisible(FALSE))
  }
  
  cat("Node ID:", vb$node_id %||% "NULL", "\n")
  cat("Available agents:", paste(vb$available_agents, collapse = ", "), "\n")
  cat("Active agent:", vb$active_agent %||% "NULL", "\n")
  
  cat("\nVaporizer settings:\n")
  for (agent in vb$available_agents) {
    v <- vb$vaporizers[[agent]]
    cat("  ", agent, "- Setting:", v$vaporizer_setting, "%, Open:", v$is_open, 
        ", Current Fi:", vb$current_fi_agents[[agent]] %||% 0, "\n")
  }
  
  invisible(TRUE)
}

#' Lungs Status Check
#' Examines the current state of the lungs
check_lungs_status <- function(engine) {
  cat("\n=== LUNGS STATUS ===\n")
  
  lungs <- engine$patient$systems$respiratory$organs$lungs
  if (is.null(lungs)) {
    cat("ERROR: No Lungs found!\n")
    return(invisible(FALSE))
  }
  
  cat("Node ID:", lungs$node_id %||% "NULL", "\n")
  cat("Current agent:", lungs$current_agent, "\n")
  
  cat("\nTarget Fi agents:\n")
  if (length(lungs$target_fi_agents) > 0) {
    for (agent in names(lungs$target_fi_agents)) {
      cat("  ", agent, ":", lungs$target_fi_agents[[agent]], "\n")
    }
  } else {
    cat("  (none)\n")
  }
  
  cat("\nCurrent Fi agents:\n")
  if (length(lungs$Fi_agents) > 0) {
    for (agent in names(lungs$Fi_agents)) {
      cat("  ", agent, ":", lungs$Fi_agents[[agent]], "\n")
    }
  } else {
    cat("  (none)\n")
  }
  
  cat("\nCurrent Fa agents:\n")
  if (length(lungs$Fa_agents) > 0) {
    for (agent in names(lungs$Fa_agents)) {
      cat("  ", agent, ":", lungs$Fa_agents[[agent]], "\n")
    }
  } else {
    cat("  (none)\n")
  }
  
  cat("\nBackward compatibility storage:\n")
  cat("  fi_volatiles:", length(lungs$fi_volatiles), "agents -", 
      paste(names(lungs$fi_volatiles), collapse = ", "), "\n")
  cat("  fa_volatiles:", length(lungs$fa_volatiles), "agents -", 
      paste(names(lungs$fa_volatiles), collapse = ", "), "\n")
  
  invisible(TRUE)
}

#' Test Bus Communication
#' Manually tests communication between components
test_bus_communication <- function(engine) {
  cat("\n=== TESTING BUS COMMUNICATION ===\n")
  
  vb <- engine$machine$vaporizer_bank
  lungs <- engine$patient$systems$respiratory$organs$lungs
  
  # Test 1: Manual publish from VaporizerBank
  cat("\n1. Testing VaporizerBank publish...\n")
  old_fi <- vb$current_fi_agents$sevoflurane %||% 0
  cat("   Before publish - sevoflurane Fi:", old_fi, "\n")
  
  vb$force_publish()
  
  # Check if data appeared on bus
  fi_on_bus <- NULL
  tryCatch({
    fi_on_bus <- engine$bus$get_param("vaporizer_bank", "current_fi_agents")$value
  }, error = function(e) {
    cat("   Error getting from bus:", e$message, "\n")
  })
  
  if (!is.null(fi_on_bus) && is.list(fi_on_bus)) {
    cat("   SUCCESS: Found on bus - sevoflurane:", fi_on_bus$sevoflurane %||% 0, "\n")
  } else {
    cat("   FAILED: Data not found on bus\n")
  }
  
  # Test 2: Manual sync to Lungs
  cat("\n2. Testing direct sync to Lungs...\n")
  sync_result <- lungs$sync_with_vaporizer_bank(vb)
  
  if (sync_result) {
    cat("   SUCCESS: Direct sync completed\n")
    cat("   Lungs Fi sevoflurane:", lungs$Fi_agents$sevoflurane %||% 0, "\n")
  } else {
    cat("   FAILED: Direct sync failed\n")
  }
  
  # Test 3: Bus subscription test
  cat("\n3. Testing bus subscriptions...\n")
  
  # Check what patterns Lungs is subscribed to
  if (!is.null(lungs$bus) && !is.null(lungs$bus$subscriptions)) {
    cat("   Lungs subscriptions:\n")
    for (pattern in names(lungs$bus$subscriptions)) {
      if (grepl("vaporizer|agent", pattern, ignore.case = TRUE)) {
        cat("     ", pattern, "\n")
      }
    }
  } else {
    cat("   No subscription information available\n")
  }
  
  invisible(TRUE)
}

#' Comprehensive Diagnostic
#' Runs all diagnostic checks
run_full_diagnostic <- function(engine) {
  cat("ANESTHESIA SIMULATION DIAGNOSTIC REPORT\n")
  cat("======================================\n")
  
  inspect_bus_state(engine)
  check_vaporizer_status(engine)
  check_lungs_status(engine)
  test_bus_communication(engine)
  
  cat("\n=== RECOMMENDATIONS ===\n")
  
  vb <- engine$machine$vaporizer_bank
  lungs <- engine$patient$systems$respiratory$organs$lungs
  
  # Check for common issues
  issues_found <- 0
  
  if (is.null(engine$bus$state) || length(engine$bus$state) == 0) {
    cat("ISSUE: Bus state is empty or NULL\n")
    cat("  -> Check MessageBus initialization\n")
    issues_found <- issues_found + 1
  }
  
  if (length(vb$current_fi_agents) == 0) {
    cat("ISSUE: VaporizerBank has no current_fi_agents\n")
    cat("  -> Call vb$initialize_agent_fractions()\n")
    issues_found <- issues_found + 1
  }
  
  if (length(lungs$target_fi_agents) == 0) {
    cat("ISSUE: Lungs has no target_fi_agents\n")
    cat("  -> Call lungs$sync_with_vaporizer_bank(vb)\n")
    issues_found <- issues_found + 1
  }
  
  if (is.null(vb$node_id) || is.null(lungs$node_id)) {
    cat("ISSUE: Missing node_id in components\n")
    cat("  -> Ensure proper node_id assignment\n")
    issues_found <- issues_found + 1
  }
  
  if (issues_found == 0) {
    cat("No major issues detected. System appears functional.\n")
  }
  
  cat("\n=== SUGGESTED FIXES ===\n")
  cat("1. Force sync: engine$sync_vaporizer_to_lungs()\n")
  cat("2. Manual publish: engine$machine$vaporizer_bank$force_publish()\n")
  cat("3. Check subscriptions: inspect_bus_state(engine)\n")
  
  invisible(TRUE)
}

# =============================================================================
# TESTING FUNCTIONS
# =============================================================================

#' Test Volatile Agent Workflow
#' Tests the complete workflow from vaporizer setting to lung uptake
test_volatile_workflow <- function(engine, agent = "sevoflurane", setting = 2.0) {
  cat("\n=== TESTING VOLATILE AGENT WORKFLOW ===\n")
  cat("Agent:", agent, ", Setting:", setting, "%\n\n")
  
  vb <- engine$machine$vaporizer_bank
  lungs <- engine$patient$systems$respiratory$organs$lungs
  
  # Step 1: Set vaporizer
  cat("Step 1: Setting vaporizer...\n")
  tryCatch({
    vb$set_vaporizer_setting(agent, setting)
    vb$open_vaporizer(agent)
    cat("  SUCCESS: Vaporizer set to", setting, "% and opened\n")
  }, error = function(e) {
    cat("  ERROR:", e$message, "\n")
    return(invisible(FALSE))
  })
  
  # Step 2: Force publish
  cat("\nStep 2: Publishing to bus...\n")
  vb$force_publish()
  cat("  Published current_fi_agents:", deparse(vb$current_fi_agents), "\n")
  
  # Step 3: Check bus state
  cat("\nStep 3: Checking bus state...\n")
  fi_on_bus <- tryCatch({
    engine$bus$get_param("vaporizer_bank", "current_fi_agents")$value
  }, error = function(e) NULL)
  
  if (!is.null(fi_on_bus)) {
    cat("  SUCCESS: Data found on bus\n")
  } else {
    cat("  WARNING: Data not found on bus\n")
  }
  
  # Step 4: Sync to lungs
  cat("\nStep 4: Syncing to lungs...\n")
  sync_result <- lungs$sync_with_vaporizer_bank(vb)
  
  if (sync_result) {
    cat("  SUCCESS: Sync completed\n")
  } else {
    cat("  ERROR: Sync failed\n")
  }
  
  # Step 5: Run simulation steps
  cat("\nStep 5: Running simulation steps...\n")
  for (i in 1:10) {
    lungs$update(dt = 1)
    fi_val <- lungs$Fi_agents[[agent]] %||% 0
    fa_val <- lungs$Fa_agents[[agent]] %||% 0
    cat("  Step", i, "- Fi:", sprintf("%.4f", fi_val), "Fa:", sprintf("%.4f", fa_val), "\n")
  }
  
  # Step 6: Check respiratory system access
  cat("\nStep 6: Testing RespiratorySystem access...\n")
  resp_fi <- tryCatch({
    engine$patient$systems$respiratory$get_fi(agent)
  }, error = function(e) {
    cat("  ERROR in get_fi:", e$message, "\n")
    return(NA)
  })
  
  resp_fa <- tryCatch({
    engine$patient$systems$respiratory$get_fa(agent)
  }, error = function(e) {
    cat("  ERROR in get_fa:", e$message, "\n")
    return(NA)
  })
  
  cat("  RespiratorySystem get_fi(", agent, "):", resp_fi, "\n")
  cat("  RespiratorySystem get_fa(", agent, "):", resp_fa, "\n")
  
  # Final summary
  cat("\n=== WORKFLOW SUMMARY ===\n")
  cat("Vaporizer setting:", vb$vaporizers[[agent]]$vaporizer_setting, "%\n")
  cat("Vaporizer Fi:", vb$current_fi_agents[[agent]] %||% 0, "\n")
  cat("Lungs target Fi:", lungs$target_fi_agents[[agent]] %||% 0, "\n")
  cat("Lungs current Fi:", lungs$Fi_agents[[agent]] %||% 0, "\n")
  cat("Lungs current Fa:", lungs$Fa_agents[[agent]] %||% 0, "\n")
  cat("RespiratorySystem Fi:", resp_fi, "\n")
  cat("RespiratorySystem Fa:", resp_fa, "\n")
  
  invisible(TRUE)
}

#' Test Multiple Agents
#' Tests switching between different volatile agents
test_multiple_agents <- function(engine) {
  cat("\n=== TESTING MULTIPLE AGENTS ===\n")
  
  vb <- engine$machine$vaporizer_bank
  
  # Test agent 1: Sevoflurane
  cat("\n--- Testing Sevoflurane ---\n")
  test_volatile_workflow(engine, "sevoflurane", 2.0)
  
  # Wait a bit
  cat("\nWaiting 5 simulation steps...\n")
  for (i in 1:5) {
    engine$patient$systems$respiratory$organs$lungs$update(dt = 1)
  }
  
  # Switch to agent 2: Desflurane (if available)
  if ("desflurane" %in% vb$available_agents) {
    cat("\n--- Switching to Desflurane ---\n")
    test_volatile_workflow(engine, "desflurane", 6.0)
  }
  
  invisible(TRUE)
}

# =============================================================================
# FIX FUNCTIONS
# =============================================================================

#' Force Complete Resync
#' Forces a complete resynchronization of the system
force_complete_resync <- function(engine) {
  cat("\n=== FORCING COMPLETE RESYNC ===\n")
  
  vb <- engine$machine$vaporizer_bank
  lungs <- engine$patient$systems$respiratory$organs$lungs
  
  # Step 1: Reinitialize VaporizerBank state
  cat("1. Reinitializing VaporizerBank...\n")
  vb$initialize_agent_fractions()
  vb$force_publish()
  
  # Step 2: Clear and resync Lungs
  cat("2. Clearing and resyncing Lungs...\n")
  lungs$target_fi_agents <- list()
  lungs$Fi_agents <- list()
  lungs$Fa_agents <- list()
  lungs$fi_volatiles <- list()
  lungs$fa_volatiles <- list()
  
  # Step 3: Manual sync
  cat("3. Performing manual sync...\n")
  sync_result <- lungs$sync_with_vaporizer_bank(vb)
  
  # Step 4: Force publish from both
  cat("4. Force publishing from both components...\n")
  vb$force_publish()
  lungs$publish_outputs()
  
  # Step 5: Run a few update cycles
  cat("5. Running update cycles...\n")
  for (i in 1:5) {
    vb$update(dt = 1)
    lungs$update(dt = 1)
  }
  
  cat("Resync complete!\n")
  invisible(TRUE)
}

# =============================================================================
# USAGE EXAMPLES
# =============================================================================

# Example usage:
# engine <- SimulationEngine$new(patient_config = "patient.yaml", machine_config = "machine.yaml")
# run_full_diagnostic(engine)
# test_volatile_workflow(engine, "sevoflurane", 2.0)
# force_complete_resync(engine)

cat("Anesthesia Simulation Diagnostic Tool Loaded!\n")
cat("Available functions:\n")
cat("  run_full_diagnostic(engine)     - Complete system check\n")
cat("  test_volatile_workflow(engine)  - Test volatile agent workflow\n")
cat("  inspect_bus_state(engine)       - Examine message bus\n")
cat("  force_complete_resync(engine)   - Force system resync\n")
cat("  test_multiple_agents(engine)    - Test agent switching\n")