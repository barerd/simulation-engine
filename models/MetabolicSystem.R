MetabolicSystem <- R6::R6Class(
  "MetabolicSystem",
  inherit = PhysiologicalSystem,
  public = list(
    vo2_ml_min = 250, vco2_ml_min = 200,
    initialize = function(patient=NULL, config=list(), node_id=NULL, bus=NULL) {
      super$initialize(name="metabolism", patient=patient, config=config, node_id=node_id, bus=bus)
      self$vo2_ml_min  <- as.numeric(config$vo2_ml_min  %||% self$vo2_ml_min)
      self$vco2_ml_min <- as.numeric(config$vco2_ml_min %||% self$vco2_ml_min)
      if (!is.null(self$bus) && !is.null(self$node_id)) {
        self$bus$set_param(self$node_id, "vo2_ml_min",  self$vo2_ml_min,  TRUE)
        self$bus$set_param(self$node_id, "vco2_ml_min", self$vco2_ml_min, TRUE)
      }
    },
    update = function(dt=1) {
      # (optional) add slow drift or noise and republish
    }
  )
)