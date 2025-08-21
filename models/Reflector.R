Reflector <- R6::R6Class(
  "Reflector",
  inherit = DevicePart,
  public = list(
    V_L = 1.6,
    eta = 0.6, # fraction captured/returned
    store = list(fio2=0.21, fin2o=0.0, fi_agents=list()),  # initialize as room air
    
    effective_internal_volume_L = function(mode) self$V_L,
    
    flow_path_effect = function(fgf, phase, dt) fgf, # usually FGF unchanged
    
    on_exhalation = function(flow_L_min, gas_vec, dt) {
      # add eta*flow*dt to store (first-order mix)
    },
    on_inhalation = function(flow_L_min, upstream_gas, dt) {
      # draw from store up to eta*flow*dt, mix with upstream
      # return mixed gas to inspiratory limb
    },
    
    state_snapshot = function() list(V_L=self$V_L, eta=self$eta, store=self$store)
  )
)