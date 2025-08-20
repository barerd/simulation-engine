ModeControlled <- R6::R6Class(
  "ModeControlled",
  inherit = DatexMode,
  public = list(
    label = "controlled",
    get_fio2 = function(machine) {
      # no dilution: circuit FiO2 is what the machine is mixing
      machine$current_fio2
    },
    get_current_fi_agents = function(machine) {
      # no dilution: agent fractions from the circuit
      machine$vaporizer_bank$get_volatile_agent_composition()
    }
  )
)