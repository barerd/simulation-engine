MessageBus <- R6Class(
  "MessageBus",
  public = list(
    initialize = function() {
      private$subs <- list(); private$next_id <- 0L
      private$retained <- new.env(parent = emptyenv())
      private$params   <- new.env(parent = emptyenv())
    },
    subscribe = function(pattern, callback, replay = FALSE) {
      private$next_id <- private$next_id + 1L; id <- as.character(private$next_id)
      private$subs[[id]] <- list(pattern = pattern, fn = callback)
      if (replay) for (tp in ls(private$retained, all.names = TRUE))
        if (grepl(pattern, tp, perl = TRUE))
          try(callback(tp, get(tp, envir = private$retained, inherits = FALSE)), silent = TRUE)
      id
    },
    publish = function(topic, message, retain = FALSE) {
      if (retain) assign(topic, message, envir = private$retained)
      for (sub in private$subs) if (!is.null(sub) && grepl(sub$pattern, topic, perl = TRUE))
        try(sub$fn(topic, message), silent = TRUE)
      invisible(TRUE)
    },
    set_param = function(scope, name, value, notify = TRUE) {
      assign(paste0(scope, "::", name), value, envir = private$params)
      if (notify) self$publish(sprintf("state/%s/%s", scope, name), list(name = name, value = value), retain = TRUE)
      invisible(TRUE)
    },
    set_params = function(scope, named_list, notify = TRUE) {
      stopifnot(is.list(named_list), !is.null(names(named_list)))
      for (nm in names(named_list)) self$set_param(scope, nm, named_list[[nm]], notify = FALSE)
      if (notify) {
        self$publish(sprintf("state/%s", scope), named_list, retain = TRUE)
        for (nm in names(named_list))
          self$publish(sprintf("state/%s/%s", scope, nm), list(name = nm, value = named_list[[nm]]), retain = TRUE)
      }
      invisible(TRUE)
    },
    get_param = function(scope, name, default = NULL) {
      key <- paste0(scope, "::", name)
      if (exists(key, envir = private$params, inherits = FALSE))
        get(key, envir = private$params, inherits = FALSE) else default
    },
    # Get all parameters for a scope
    get_state = function(scope) {
      if (is.null(scope)) return(NULL)
      
      # Get all keys that start with "scope::"
      prefix <- paste0(scope, "::")
      all_keys <- ls(private$params, all.names = TRUE)
      scope_keys <- all_keys[startsWith(all_keys, prefix)]
      
      if (length(scope_keys) == 0) return(list())
      
      # Extract parameter names and values
      result <- list()
      for (key in scope_keys) {
        param_name <- substring(key, nchar(prefix) + 1)
        result[[param_name]] <- get(key, envir = private$params, inherits = FALSE)
      }
      
      result
    },
    unsubscribe = function(id) {
      id <- as.character(id)
      private$subs[[id]] <- NULL
      invisible(TRUE)
    },
    list_subscriptions = function(filter_ids = NULL) {
      ids <- names(private$subs)
      rows <- lapply(ids, function(id) {
        s <- private$subs[[id]]
        if (is.null(s)) return(NULL)
        list(id = id, pattern = s$pattern)
      })
      rows <- Filter(Negate(is.null), rows)
      
      df <- if (length(rows)) {
        do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
      } else {
        data.frame(id = character(), pattern = character(), stringsAsFactors = FALSE)
      }
      
      if (!is.null(filter_ids)) {
        filter_ids <- as.character(filter_ids)
        df <- df[df$id %in% filter_ids, , drop = FALSE]
      }
      rownames(df) <- NULL
      df
    }
  ),
  # Add active binding for state access
  active = list(
    state = function() {
      # Return all parameters organized by scope
      all_keys <- ls(private$params, all.names = TRUE)
      
      if (length(all_keys) == 0) return(list())
      
      # Parse scope::name format
      scopes <- list()
      for (key in all_keys) {
        parts <- strsplit(key, "::", fixed = TRUE)[[1]]
        if (length(parts) == 2) {
          scope_name <- parts[1]
          param_name <- parts[2]
          value <- get(key, envir = private$params, inherits = FALSE)
          
          if (is.null(scopes[[scope_name]])) {
            scopes[[scope_name]] <- list()
          }
          scopes[[scope_name]][[param_name]] <- value
        }
      }
      
      scopes
    }
  ),
  private = list(subs = NULL, next_id = 0L, retained = NULL, params = NULL)
)