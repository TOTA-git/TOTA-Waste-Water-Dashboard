# Shared helpers used by both data_refresh.R (the scheduled data-refresh
# job) and global.R (the running app, which only ever reads the cache
# that job produces).

CACHE_DIR <- "data/cache"

# --- Atomic writes -----------------------------------------------------
# Write to a temp file in the same directory, then rename over the
# target. A refresh that dies partway through writing never leaves a
# truncated/corrupt cache file behind - readers always see either the
# previous good file or a fully-written new one.
atomic_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp")
  readr::write_csv(df, tmp)
  file.rename(tmp, path)
}

# --- Cache reads (used by global.R) -------------------------------------
# Error clearly instead of silently starting the app with no data, so a
# missing/empty cache is obvious rather than surfacing as a confusing
# downstream plotting error.
read_cache_csv <- function(name, ...) {
  path <- file.path(CACHE_DIR, paste0(name, ".csv"))
  if (!file.exists(path)) {
    stop(
      "Cache file '", path, "' not found. Run data_refresh.R at least ",
      "once before starting the app."
    )
  }
  readr::read_csv(path, show_col_types = FALSE, ...)
}

# --- Refresh logging ------------------------------------------------------
# One row per dataset per refresh attempt, so refresh_log.csv
# builds a history of what succeeded/failed.
log_refresh <- function(name, status, message = NA_character_, n_rows = NA_integer_) {
  dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
  log_path <- file.path(CACHE_DIR, "refresh_log.csv")
  entry <- data.frame(
    dataset = name,
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    status = status,
    n_rows = n_rows,
    message = message,
    stringsAsFactors = FALSE
  )
  readr::write_csv(entry, log_path, append = file.exists(log_path))
}

# --- Clear Log -----------------------------------------------------------
#Clears refresh_log.csv before each refresh
clear_refresh_log <- function() {
  dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
  
  log_path <- file.path(CACHE_DIR, "refresh_log.csv")
  
  if (file.exists(log_path)) {
    file.remove(log_path)
  }
}
