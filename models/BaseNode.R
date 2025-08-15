BaseNode <- R6Class(
  "BaseNode",
  public = list(
    node_id = NULL, bus = NULL, subscriptions = character(),
    initialize = function(node_id = NULL) {
      self$node_id <- node_id %||% paste0(tolower(class(self)[1L]), "_", as.integer(stats::runif(1, 1, 1e6)))
    },
    connect_bus = function(bus, node_id = NULL) {
      stopifnot(inherits(bus, "MessageBus")); self$bus <- bus; if (!is.null(node_id)) self$node_id <- node_id; invisible(TRUE)
    },
    set_param = function(name, value, notify = TRUE)  self$bus$set_param(self$node_id, name, value, notify),
    publish_params = function(named_list)             self$bus$set_params(self$node_id, named_list, notify = TRUE),
    get_param = function(scope, name, default = NULL) self$bus$get_param(scope, name, default),
    subscribe = function(pattern, callback, replay = TRUE) {
      id <- self$bus$subscribe(pattern, callback, replay); self$subscriptions <- c(self$subscriptions, id); invisible(id)
    },
    unsubscribe = function(id) {
      stopifnot(!is.null(self$bus))
      id <- as.character(id)
      self$bus$unsubscribe(id)
      self$subscriptions <- setdiff(self$subscriptions, id)
      invisible(TRUE)
    },

    unsubscribe_all = function() {
      stopifnot(!is.null(self$bus))
      for (id in self$subscriptions) self$bus$unsubscribe(id)
      self$subscriptions <- character()
      invisible(TRUE)
    },
    
    list_subscriptions = function() {
      stopifnot(!is.null(self$bus))
      self$bus$list_subscriptions(self$subscriptions)
    }
  )
)
