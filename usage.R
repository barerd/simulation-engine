# =============================================================================
# EXAMPLE USAGE OF ANESTHESIA SIMULATION DIAGNOSTIC AND FIXES
# =============================================================================

# This script demonstrates how to use the diagnostic tools and fixes
# to resolve communication issues in your anesthesia simulation

# =============================================================================
# SETUP
# =============================================================================

# Assuming you have the diagnostic and fix scripts loaded:
source("anesthesia_diagnostic.R")  # The first artifact
source("communication_fixes.R")    # The second artifact

# Initialize your simulation engine
cat("Initializing simulation engine...\n")
# engine <- SimulationEngine$new(
#   patient_config = "path/to/your/patient.yaml",
#   machine_config = "path/to/your/machine.yaml"
# )

# For demonstration, let's assume you have an engine object
# engine <- your_existing_engine

# =============================================================================
# DIAGNOSTIC WORKFLOW
# =============================================================================

diagnostic_workflow <- function(engine) {
  cat("=== DIAGNOSTIC WORKFLOW ===\n")
  
  # Step 1: Run comprehensive diagnostic
  cat("\n--- Step 1: Full System Diagnostic ---\n")
  run_full_diagnostic(engine)
  
  # Step 2: Check current volatile agent status
  cat("\n--- Step 2: Current Status Check ---\n")
  vb <- engine$machine$vaporizer_bank
  lungs <- engine$patient$systems$respiratory$organs$lungs
  
  cat("VaporizerBank available agents:", paste(vb$available_agents, collapse = ", "), "\n")
  cat("VaporizerBank active agent:", vb$active_agent %||% "none", "\n")
  cat("Lungs current agent:", lungs$current_agent, "\n")
  cat("Lungs Fi agents count:", length(lungs$Fi_agents), "\n")
  
  # Step 3: Try basic volatile workflow test
  cat("\n--- Step 3: Basic Workflow Test ---\n")
  tryCatch({
    test_volatile_workflow(engine, "sevoflurane", 2.0)
  }, error = function(e) {
    cat("ERROR in workflow test:", e$message, "\n")
  })
  
  invisible(TRUE)
}

# =============================================================================
# FIX APPLICATION WORKFLOW
# =============================================================================

fix_workflow <- function(engine) {
  cat("\n=== FIX APPLICATION WORKFLOW ===\n")
  
  # Step 1: Apply all communication fixes
  cat("\n--- Step 1: Applying Communication Fixes ---\n")
  apply_all_fixes(engine)
  
  # Step 2: Validate that fixes worked
  cat("\n--- Step 2: Validating Fixes ---\n")
  validate_fixes(engine)
  
  # Step 3: Test with different agents and settings
  cat("\n--- Step 3: Extended Testing ---\n")
  
  # Test sevoflurane
  cat("\nTesting sevoflurane at 2.5%:\n")
  tryCatch({
    test_volatile_workflow(engine, "sevoflurane", 2.5)
  }, error = function(e) {
    cat("Error testing sevoflurane:", e$message, "\n")
  })
  
  # Test desflurane if available
  vb <- engine$machine$vaporizer_bank
  if ("desflurane" %in% vb$available_agents) {
    cat("\nTesting desflurane at 6.0%:\n")
    tryCatch({
      test_volatile_workflow(engine, "desflurane", 6.0)
    }, error = function(e) {
      cat("Error testing desflurane:", e$message, "\n")
    })
  }
  
  invisible(TRUE)
}

# =============================================================================
# MONITORING WORKFLOW
# =============================================================================

monitoring_workflow <- function(engine, duration_seconds = 30) {
  cat("\n=== MONITORING WORKFLOW ===\n")
  cat("Monitoring system for", duration_seconds, "seconds...\n")
  
  vb <- engine$machine$vaporizer_bank
  lungs <- engine$patient$systems$respiratory$organs$lungs
  resp <- engine$patient$systems$respiratory
  
  # Set up a volatile agent
  vb$set_vaporizer_setting("sevoflurane", 2.0)
  vb$open_vaporizer("sevoflurane")
  
  # Monitor changes over time
  start_time <- Sys.time()
  iteration <- 0
  
  while (difftime(Sys.time(), start_time, units = "secs") < duration_seconds) {
    iteration <- iteration + 1
    
    # Run simulation step
    vb$update(dt = 1)
    lungs$update(dt = 1)
    
    # Get current values
    vb_fi <- vb$current_fi_agents$sevoflurane %||% 0
    lungs_fi <- lungs$Fi_agents$sevoflurane %||% 0
    lungs_fa <- lungs$Fa_agents$sevoflurane %||% 0
    resp_fi <- resp$get_fi("sevoflurane")
    resp_fa <- resp$get_fa("sevoflurane")
    
    # Print every 5 seconds
    if (iteration %% 5 == 0) {
      cat(sprintf("Time %02ds: VB=%.4f, Lungs Fi=%.4f, Fa=%.4f, Resp Fi=%.4f, Fa=%.4f\n",
                  iteration, vb_fi, lungs_fi, lungs_fa, resp_fi, resp_fa))
    }
    
    Sys.sleep(1)
  }
  
  cat("Monitoring complete.\n")
  invisible(TRUE)
}

# =============================================================================
# COMPLETE SYSTEM TEST
# =============================================================================

complete_system_test <- function(engine) {
  cat("=== COMPLETE ANESTHESIA SIMULATION SYSTEM TEST ===\n")
  
  # Phase 1: Diagnostics
  cat("\n>>> PHASE 1: DIAGNOSTICS <<<\n")
  diagnostic_workflow(engine)
  
  # Phase 2: Apply fixes
  cat("\n>>> PHASE 2: APPLYING FIXES <<<\n")
  fix_workflow(engine)
  
  # Phase 3: Extended testing
  cat("\n>>> PHASE 3: EXTENDED TESTING <<<\n")
  
  # Test multiple scenarios
  scenarios <- list(
    list(agent = "sevoflurane", setting = 1.0, description = "Low sevoflurane"),
    list(agent = "sevoflurane", setting = 3.0, description = "High sevoflurane")
  )
  
  # Add desflurane if available
  vb <- engine$machine$vaporizer_bank
  if ("desflurane" %in% vb$available_agents) {
    scenarios <- append(scenarios, list(
      list(agent = "desflurane", setting = 6.0, description = "Standard desflurane")
    ))
  }
  
  for (scenario in scenarios) {
    cat("\n--- Testing:", scenario$description, "---\n")
    
    tryCatch({
      # Set the vaporizer
      vb$set_vaporizer_setting(scenario$agent, scenario$setting)
      vb$open_vaporizer(scenario$agent)
      
      # Run several update cycles
      for (i in 1:10) {
        vb$update(dt = 1)
        engine$patient$systems$respiratory$organs$lungs$update(dt = 1)
      }
      
      # Check results
      fi_val <- engine$patient$systems$respiratory$get_fi(scenario$agent)
      fa_val <- engine$patient$systems$respiratory$get_fa(scenario$agent)
      
      cat("Results: Fi =", sprintf("%.4f", fi_val), ", Fa =", sprintf("%.4f", fa_val), "\n")
      
      if (fi_val > 0) {
        cat("✓ SUCCESS: Agent uptake working\n")
      } else {
        cat("✗ FAILURE: No agent uptake\n")
      }
      
    }, error = function(e) {
      cat("ERROR in scenario:", e$message, "\n")
    })
  }
  
  # Phase 4: Final system state
  cat("\n>>> PHASE 4: FINAL SYSTEM STATE <<<\n")
  inspect_bus_state(engine)
  
  cat("\n=== SYSTEM TEST COMPLETE ===\n")
  invisible(TRUE)
}

# =============================================================================
# USAGE EXAMPLES
# =============================================================================

cat("EXAMPLE USAGE SCRIPT LOADED!\n")
cat("==============================\n")
cat("Available test workflows:\n\n")

cat("1. DIAGNOSTIC WORKFLOW:\n")
cat("   diagnostic_workflow(engine)\n")
cat("   - Runs comprehensive system diagnostics\n")
cat("   - Identifies communication issues\n")
cat("   - Tests basic functionality\n\n")

cat("2. FIX WORKFLOW:\n")
cat("   fix_workflow(engine)\n")
cat("   - Applies all communication fixes\n")
cat("   - Validates that fixes work\n")
cat("   - Tests multiple agents\n\n")

cat("3. MONITORING WORKFLOW:\n")
cat("   monitoring_workflow(engine, 30)\n")
cat("   - Monitors system for 30 seconds\n")
cat("   - Shows real-time volatile agent dynamics\n")
cat("   - Useful for debugging time constants\n\n")

cat("4. COMPLETE SYSTEM TEST:\n")
cat("   complete_system_test(engine)\n")
cat("   - Runs all phases: diagnostic, fix, test\n")
cat("   - Comprehensive validation of the entire system\n")
cat("   - Best for thorough troubleshooting\n\n")

cat("5. QUICK FIX (if you just want to fix the issues):\n")
cat("   apply_all_fixes(engine)\n")
cat("   validate_fixes(engine)\n\n")

cat("Example usage:\n")
cat("  # Load your engine\n")
cat("  engine <- SimulationEngine$new(patient_config = 'patient.yaml', \n")
cat("                                machine_config = 'machine.yaml')\n")
cat("  \n")
cat("  # Run complete test\n")
cat("  complete_system_test(engine)\n")
cat("  \n")
cat("  # Or just apply fixes quickly\n")
cat("  apply_all_fixes(engine)\n")

# =============================================================================
# TROUBLESHOOTING QUICK REFERENCE
# =============================================================================

print_quick_reference <- function() {
  cat("\n=== QUICK TROUBLESHOOTING REFERENCE ===\n")
  
  cat("SYMPTOM: RespiratorySystem$get_fi('sevoflurane') returns 0\n")
  cat("SOLUTION: apply_all_fixes(engine)\n\n")
  
  cat("SYMPTOM: Lungs$Fi_agents is empty\n")
  cat("SOLUTION: engine$sync_vaporizer_to_lungs()\n\n")
  
  cat("SYMPTOM: VaporizerBank changes don't affect Lungs\n")
  cat("SOLUTION: fix_vaporizer_publishing(engine$machine$vaporizer_bank)\n\n")
  
  cat("SYMPTOM: Volatile concentrations don't increase over time\n")
  cat("SOLUTION: Check time constants, run multiple update() cycles\n\n")
  
  cat("SYMPTOM: Bus is empty or NULL\n")
  cat("SOLUTION: Check MessageBus initialization in SimulationEngine\n\n")
  
  invisible(TRUE)
}

print_quick_reference()









# 1. Load your engine
engine <- SimulationEngine$new(
  patient_config = "patient.yaml", 
  machine_config = "machine.yaml"
)

# 2. Quick fix (most common case):
apply_all_fixes(engine)
validate_fixes(engine)

# 3. Or run complete diagnostic and test:
complete_system_test(engine)

# 4. Test specific agent:
test_volatile_workflow(engine, "sevoflurane", 2.0)