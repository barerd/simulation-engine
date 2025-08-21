# ---- Stream primitives ----------------------------------------------------
make_stream <- function(flow_L_min, fr_named_list) {
  fr <- lapply(fr_named_list, function(x) as.numeric(x %||% 0))
  s  <- sum(unlist(fr), na.rm = TRUE)
  if (is.finite(s) && s > 0 && abs(s - 1) > 1e-9) {
    for (k in names(fr)) fr[[k]] <- fr[[k]] / s
  }
  list(flow_L_min = as.numeric(flow_L_min %||% 0), fr = fr)
}

mix_streams <- function(stream_list) {
  if (!length(stream_list)) return(make_stream(0, list()))
  # flow‑weighted mean over the UNION of gas keys
  keys <- unique(unlist(lapply(stream_list, function(s) names(s$fr %||% list()))))
  flows <- vapply(stream_list, function(s) as.numeric(s$flow_L_min %||% 0), numeric(1))
  tot_flow <- sum(flows, na.rm = TRUE)
  if (!is.finite(tot_flow) || tot_flow <= 0) {
    # no flow: just average fractions (still use UNION)
    out <- setNames(vector("list", length(keys)), keys)
    for (k in keys) {
      vals <- vapply(stream_list, function(s) as.numeric(s$fr[[k]] %||% 0), numeric(1))
      out[[k]] <- mean(vals, na.rm = TRUE)
    }
    return(make_stream(0, out))
  }
  out <- setNames(vector("list", length(keys)), keys)
  for (k in keys) {
    num <- 0
    for (i in seq_along(stream_list)) {
      num <- num + flows[i] * as.numeric(stream_list[[i]]$fr[[k]] %||% 0)
    }
    out[[k]] <- num / tot_flow
  }
  make_stream(tot_flow, out)
}

lowpass_stream <- function(prev, target, tau_s, dt_s) {
  prev  <- prev   %||% make_stream(0, list())
  target<- target %||% make_stream(0, list())
  a <- 1 - exp(-as.numeric(dt_s) / max(1e-6, as.numeric(tau_s)))
  keys <- unique(c(names(prev$fr %||% list()), names(target$fr %||% list())))
  out_fr <- setNames(vector("list", length(keys)), keys)
  for (k in keys) {
    p <- as.numeric(prev$fr[[k]]   %||% 0)
    t <- as.numeric(target$fr[[k]] %||% 0)
    out_fr[[k]] <- p + (t - p) * a
  }
  # flow can also relax toward target’s flow to avoid discontinuities
  flow <- as.numeric(prev$flow_L_min %||% 0) + 
    (as.numeric(target$flow_L_min %||% 0) - as.numeric(prev$flow_L_min %||% 0)) * a
  make_stream(flow, out_fr)
}