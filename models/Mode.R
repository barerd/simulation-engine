Mode <- R6::R6Class(
  "Mode",
  public = list(
    label = "base",
    get_fio2 = function(machine) 0.21,
    get_current_fi_agents = function(machine) list(),
    total_fresh_gas_flow = function(machine) machine$total_fresh_gas_flow()
  )
)