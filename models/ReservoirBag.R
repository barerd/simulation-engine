# models/ReservoirBag.R
ReservoirBag <- R6::R6Class(
  "ReservoirBag",
  inherit = DevicePart,
  public = list(
    bag_volume_L = 3.0,              # nominal bag volume
    include_in_controlled = FALSE,   # Primus: TRUE, others: FALSE
    vendor = NULL,
    
    initialize = function(name,
                          parent_device = NULL,
                          node_id = NULL,
                          bus = NULL,
                          bag_volume_L = 3.0,
                          include_in_controlled = FALSE,
                          vendor = NULL) {
      super$initialize(name = name, parent_device = parent_device, node_id = node_id, bus = bus)
      self$bag_volume_L <- max(0, as.numeric(bag_volume_L))
      self$include_in_controlled <- isTRUE(include_in_controlled)
      self$vendor <- vendor
      invisible(TRUE)
    },
    
    # ---- glue methods (match the base machine’s expectations) ----
    
    # 1) Fresh-gas composition shaping by the part (no change for a plain bag)
    mix_fresh_gas = function(fgf) {
      fgf
    },
    
    # 2) Rebreathing fraction (0 for a plain bag; reflectors override this)
    rebreathing_fraction = function(mode_label) {
      0
    },
    
    # 3) How much internal volume this part adds, as a function of mode
    effective_internal_volume_L = function(mode_label) {
      ml <- tolower(mode_label %||% "")
      if (ml %in% c("manual", "manual_mask", "mask")) return(self$bag_volume_L)
      if (ml == "controlled") return(if (self$include_in_controlled) self$bag_volume_L else 0)
      0
    },
    
    # 4) τ multiplier hook used by VaporizerBank$effective_tau()
    #    Keep it simple: a big bag can slightly slow response for all agents.
    #    Tune the 0.05 factor later or make it YAML-driven if you like.
    tau_multiplier = function(agent, kind = c("response","decay")) {
      kind <- match.arg(kind)
      volL <- max(0, as.numeric(self$bag_volume_L))
      # very mild effect; you can calibrate later
      1.0 + 0.05 * volL
    },
    
    # 5) Minimal status dump for /machine/parts/state/{name}
    summary_state = function() {
      list(
        type = "ReservoirBag",
        enabled = self$enabled,
        bag_volume_L = self$bag_volume_L,
        include_in_controlled = self$include_in_controlled,
        vendor = self$vendor
      )
    }
  )
)