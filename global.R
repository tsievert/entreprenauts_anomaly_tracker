## -------- global.R --------

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(purrr)
  library(tibble)
  library(DT)
  library(viridisLite)
  library(shinyBS)
  library(shinyvalidate)
  library(ggplot2)
  library(bslib) # NEW: theming runtime + Bootswatch
  library(thematic) # NEW: auto-theming for ggplot
})

# Optional: enable reactlog during development
if (requireNamespace("reactlog", quietly = TRUE)) {
  try(reactlog::reactlog_enable(), silent = TRUE)
}

# ---------- App-wide themes (Bootstrap v3 to keep shinyBS styling consistent) ----------
light_theme <- bslib::bs_theme(version = 3, bootswatch = "flatly")
dark_theme <- bslib::bs_theme(version = 3, bootswatch = "darkly")

# ---------- Small utils ----------

`%||%` <- function(a, b) if (!is.null(a)) a else b

safe_NROW <- function(x) {
  if (is.null(x)) 0L else nrow(x)
}

safe_seq <- function(lo, hi) {
  lo <- as.integer(lo)
  hi <- as.integer(hi)
  if (is.na(lo) || is.na(hi) || lo > hi) integer(0) else seq.int(lo, hi)
}

clamp_radius <- function(x, fallback = 1L) {
  r <- suppressWarnings(as.integer(x))
  if (length(r) == 0L || is.na(r) || r < 1L) fallback else r[[1L]]
}

# Summed-area table (integral image) for fast counts over rectangles
build_sat <- function(logical_mat) {
  m <- matrix(as.integer(logical_mat), nrow = nrow(logical_mat), ncol = ncol(logical_mat))
  cs1 <- apply(m, 2, cumsum)
  cs2 <- apply(cs1, 1, cumsum)
  t(cs2)
}

# Sum over rectangle [r1..r2, c1..c2], inclusive, 1-indexed
rect_sum <- function(sat, r1, c1, r2, c2) {
  if (r1 > r2 || c1 > c2) {
    return(0L)
  }
  a <- sat[r2, c2]
  b <- if (r1 > 1L) sat[r1 - 1L, c2] else 0
  c <- if (c1 > 1L) sat[r2, c1 - 1L] else 0
  d <- if (r1 > 1L && c1 > 1L) sat[r1 - 1L, c1 - 1L] else 0
  as.integer(a - b - c + d)
}

# ---------- Logging helpers (pure data-frame API) ----------

.new_log_df <- function() {
  tibble::tibble(
    time             = character(),
    grid             = character(),
    action           = character(),
    long             = numeric(),
    lat              = numeric(),
    radiusName       = character(),
    radiusVal        = numeric(),
    outcome          = character(),
    direction        = character(),
    stage            = character(),
    ratio            = numeric(),
    remaining        = numeric(),
    cellsChecked     = numeric(),
    distanceFromLast = numeric()
  )
}

.coerce_log_row <- function(row) {
  # row: list or data.frame with some of the expected names
  if (is.data.frame(row)) {
    row_list <- as.list(row[1L, , drop = TRUE])
  } else {
    row_list <- as.list(row)
  }

  get_chr <- function(name, default = NA_character_) {
    val <- row_list[[name]]
    if (is.null(val) || length(val) == 0L || is.na(val)) default else as.character(val[[1L]])
  }

  get_num <- function(name, default = NA_real_) {
    val <- row_list[[name]]
    if (is.null(val) || length(val) == 0L || is.na(val)) {
      default
    } else {
      as.numeric(val[[1L]])
    }
  }

  tibble::tibble(
    time             = get_chr("time", format(Sys.time(), "%Y-%m-%d %H:%M:%OS6")),
    grid             = get_chr("grid"),
    action           = get_chr("action", ""),
    long             = get_num("long"),
    lat              = get_num("lat"),
    radiusName       = get_chr("radiusName"),
    radiusVal        = get_num("radiusVal"),
    outcome          = get_chr("outcome"),
    direction        = get_chr("direction"),
    stage            = get_chr("stage"),
    ratio            = get_num("ratio"),
    remaining        = get_num("remaining"),
    cellsChecked     = get_num("cellsChecked"),
    distanceFromLast = get_num("distanceFromLast")
  )
}

log_event_df <- function(log_df, row) {
  base <- if (is.null(log_df)) .new_log_df() else log_df
  new_row <- .coerce_log_row(row)
  dplyr::bind_rows(base, new_row)
}

# ---------- Viridis-based color helpers ----------

get_viridis_colors <- function(
  palette = c("magma", "inferno", "plasma", "viridis", "cividis"),
  n = 8L
) {
  palette <- match.arg(palette)
  n <- as.integer(n)
  if (n < 1L) n <- 1L
  viridisLite::viridis(n, option = palette)
}

default_color_state <- function() {
  pal_name <- "magma"
  cols <- get_viridis_colors(pal_name, 8L)

  list(
    palette    = pal_name,
    possible   = cols[[1L]],
    tested     = cols[[2L]],
    hit        = cols[[3L]],
    miss       = cols[[4L]],
    suggestion = cols[[5L]],
    albs       = cols[[6L]],
    impossible = cols[[7L]],
    gridLines  = cols[[8L]]
  )
}

# Normalization and color utilities used by server (pure functions)

normalize_color_state <- function(cs) {
  base <- default_color_state()
  pal <- base$palette %||% "magma"

  cols_base <- get_viridis_colors(pal, 8L)

  if (is.null(base$possible) || !nzchar(base$possible)) base$possible <- cols_base[[1L]]
  if (is.null(base$tested) || !nzchar(base$tested)) base$tested <- cols_base[[2L]]
  if (is.null(base$hit) || !nzchar(base$hit)) base$hit <- cols_base[[3L]]
  if (is.null(base$miss) || !nzchar(base$miss)) base$miss <- cols_base[[4L]]
  if (is.null(base$suggestion) || !nzchar(base$suggestion)) base$suggestion <- cols_base[[5L]]
  if (is.null(base$albs) || !nzchar(base$albs)) base$albs <- cols_base[[6L]]
  if (is.null(base$impossible) || !nzchar(base$impossible)) base$impossible <- cols_base[[7L]]
  if (is.null(base$gridLines) || !nzchar(base$gridLines)) base$gridLines <- cols_base[[8L]]

  if (is.null(cs) || !is.list(cs)) {
    return(base)
  }

  legacy <- identical(cs$possible, "#FFFFFF") &&
    identical(cs$tested, "#BBBBBB") &&
    identical(cs$hit, "#00FF00") &&
    identical(cs$miss, "#FF0000") &&
    identical(cs$suggestion, "#FFFF00") &&
    identical(cs$albs, "#4444FF")

  if (is.null(cs$palette) || isTRUE(legacy)) {
    return(base)
  }

  out <- base
  out$palette <- cs$palette %||% pal

  for (nm in c(
    "possible", "tested", "hit", "miss",
    "suggestion", "albs", "impossible", "gridLines"
  )) {
    val <- cs[[nm]]
    if (!is.null(val) && !is.na(val) && nzchar(val)) {
      out[[nm]] <- val
    }
  }

  out
}

# True shuffle helper – random shades and order
shuffle_viridis_colors <- function(palette, k) {
  k <- max(1L, as.integer(k))
  base <- get_viridis_colors(palette, max(32L, k * 4L))
  if (length(base) < k) {
    base <- rep(base, length.out = k)
  }
  sample(base, size = k, replace = FALSE)
}

safe_color <- function(x, default) {
  val <- x
  if (is.null(val) || is.na(val) || !nzchar(val)) {
    return(default)
  }
  ok <- tryCatch(
    {
      grDevices::col2rgb(val)
      TRUE
    },
    error = function(e) FALSE
  )
  if (ok) val else default
}

# ---------- Persistence ----------

STATE_FILE <- "my_grids_state.RDS"

saveState <- function(file = STATE_FILE, rv) {
  saved <- list(
    grids       = rv$grids,
    radiiDF     = rv$radiiDF,
    color_state = rv$color_state,
    darkMode    = rv$darkMode %||% FALSE # NEW: persist dark mode preference
  )
  saveRDS(saved, file)
}

loadState <- function(file = STATE_FILE) {
  if (!file.exists(file)) {
    return(NULL)
  }
  out <- tryCatch(readRDS(file), error = function(e) NULL)
  if (!is.list(out)) {
    return(NULL)
  }
  out
}

# ---------- Path generators (radius-aware lawnmower) ----------

generateRadiusAwareLawnmower <- function(nr, nc, R,
                                         albsDone = FALSE,
                                         albsLat = NA_integer_,
                                         albsLong = NA_integer_,
                                         albsRad = NA_integer_,
                                         allowPartial = TRUE) {
  nr <- as.integer(nr)
  nc <- as.integer(nc)
  R <- suppressWarnings(as.integer(R))
  if (is.na(R) || R < 0L) R <- 0L

  if (is.na(nr) || is.na(nc) || nr < 1L || nc < 1L) {
    return(matrix(nrow = 0L, ncol = 2L))
  }

  step <- 2L * R + 1L
  if (step < 1L) step <- 1L

  # base center region: whole grid
  r_min <- 1L
  r_max <- nr
  c_min <- 1L
  c_max <- nc

  # restrict to ALBS bounding box if active
  if (isTRUE(albsDone) &&
    !is.na(albsLat) &&
    !is.na(albsLong) &&
    !is.na(albsRad)) {
    albsLat <- as.integer(albsLat)
    albsLong <- as.integer(albsLong)
    albsRad <- as.integer(albsRad)

    r_min <- max(r_min, albsLat - albsRad)
    r_max <- min(r_max, albsLat + albsRad)
    c_min <- max(c_min, albsLong - albsRad)
    c_max <- min(c_max, albsLong + albsRad)
  }

  # if we require full coverage, shrink away from grid borders by R
  if (!allowPartial && R > 0L) {
    r_min <- max(r_min, R + 1L)
    r_max <- min(r_max, nr - R)
    c_min <- max(c_min, R + 1L)
    c_max <- min(c_max, nc - R)
  }

  if (r_min > r_max || c_min > c_max) {
    return(matrix(nrow = 0L, ncol = 2L))
  }

  centerRows <- seq.int(r_min, r_max, by = step)
  centerCols <- seq.int(c_min, c_max, by = step)

  # ensure last row/col are included
  if (length(centerRows) == 0L || tail(centerRows, 1L) != r_max) {
    centerRows <- c(centerRows, r_max)
  }
  if (length(centerCols) == 0L || tail(centerCols, 1L) != c_max) {
    centerCols <- c(centerCols, c_max)
  }

  centerRows <- sort(unique(centerRows))
  centerCols <- sort(unique(centerCols))

  if (!length(centerRows) || !length(centerCols)) {
    return(matrix(nrow = 0L, ncol = 2L))
  }

  coords <- vector("list", length(centerRows) * length(centerCols))
  idx <- 1L

  for (i in seq_along(centerRows)) {
    r <- centerRows[[i]]
    cols <- if (i %% 2L == 1L) centerCols else rev(centerCols)
    for (c in cols) {
      coords[[idx]] <- c(r, c)
      idx <- idx + 1L
    }
  }

  do.call(rbind, coords)
}

# ---------- Coordinate helpers ----------

# For ggplot we work directly in row-space and reverse the y-scale,
# so this is effectively "round to nearest row index".
y_to_row <- function(y, nr) {
  as.integer(round(y))
}

# ---------- Direction helpers (radius-limited line / sector) ----------

direction_line_mask <- function(nr, nc, cx, cy, dir, R) {
  if (is.null(dir) || is.na(dir) || !nzchar(dir)) {
    return(NULL)
  }

  nr <- as.integer(nr)
  nc <- as.integer(nc)
  cx <- suppressWarnings(as.integer(cx))
  cy <- suppressWarnings(as.integer(cy))

  if (is.na(cx) || is.na(cy) || is.na(nr) || is.na(nc) ||
    cx < 1L || cy < 1L || cx > nc || cy > nr) {
    return(NULL)
  }

  d <- toupper(as.character(dir[[1L]]))
  mask <- matrix(FALSE, nrow = nr, ncol = nc)

  # BELOW: only the drop cell itself
  if (identical(d, "BELOW")) {
    mask[cy, cx] <- TRUE
    return(mask)
  }

  R <- suppressWarnings(as.integer(R))
  if (is.na(R) || R <= 0L) {
    return(NULL)
  }

  # --- axis-only sectors for cardinals (exclude center) ---
  if (identical(d, "N")) {
    r1 <- max(1L, cy - R)
    r2 <- cy - 1L
    if (r1 <= r2) mask[r1:r2, cx] <- TRUE
    return(mask)
  }
  if (identical(d, "S")) {
    r1 <- cy + 1L
    r2 <- min(nr, cy + R)
    if (r1 <= r2) mask[r1:r2, cx] <- TRUE
    return(mask)
  }
  if (identical(d, "E")) {
    c1 <- cx + 1L
    c2 <- min(nc, cx + R)
    if (c1 <= c2) mask[cy, c1:c2] <- TRUE
    return(mask)
  }
  if (identical(d, "W")) {
    c1 <- max(1L, cx - R)
    c2 <- cx - 1L
    if (c1 <= c2) mask[cy, c1:c2] <- TRUE
    return(mask)
  }

  # --- diagonals: strict open quadrant (exclude center and axes) ---
  if (identical(d, "NE")) {
    r1 <- max(1L, cy - R)
    r2 <- cy - 1L
    c1 <- cx + 1L
    c2 <- min(nc, cx + R)
    if (r1 <= r2 && c1 <= c2) mask[r1:r2, c1:c2] <- TRUE
    return(mask)
  }
  if (identical(d, "NW")) {
    r1 <- max(1L, cy - R)
    r2 <- cy - 1L
    c1 <- max(1L, cx - R)
    c2 <- cx - 1L
    if (r1 <= r2 && c1 <= c2) mask[r1:r2, c1:c2] <- TRUE
    return(mask)
  }
  if (identical(d, "SE")) {
    r1 <- cy + 1L
    r2 <- min(nr, cy + R)
    c1 <- cx + 1L
    c2 <- min(nc, cx + R)
    if (r1 <= r2 && c1 <= c2) mask[r1:r2, c1:c2] <- TRUE
    return(mask)
  }
  if (identical(d, "SW")) {
    r1 <- cy + 1L
    r2 <- min(nr, cy + R)
    c1 <- max(1L, cx - R)
    c2 <- cx - 1L
    if (r1 <= r2 && c1 <= c2) mask[r1:r2, c1:c2] <- TRUE
    return(mask)
  }

  NULL
}

constrain_grid_by_direction <- function(gr, cx, cy, dir, R) {
  if (is.null(dir) || is.na(dir) || !nzchar(dir)) {
    return(gr)
  }

  nr <- gr$nr
  nc <- gr$nc
  if (is.null(nr) || is.null(nc) || is.na(nr) || is.na(nc)) {
    return(gr)
  }

  mask <- direction_line_mask(nr, nc, cx, cy, dir, R)
  if (is.null(mask)) {
    return(gr)
  }

  if (!any(mask)) {
    gr$possible[, ] <- FALSE
    return(gr)
  }

  gr$possible <- gr$possible & mask
  gr
}

# ---------- Suggestion logic (pure function) ----------

suggest_next_center <- function(
  gr, R,
  prefer_no_overlap = FALSE,
  allow_partial = TRUE
) {
  nr <- gr$nr
  nc <- gr$nc
  if (is.null(nr) || is.null(nc) || is.na(nr) || is.na(nc) || nr < 1L || nc < 1L) {
    return(NULL)
  }

  R <- as.integer(R)
  if (is.na(R) || R < 0L) {
    R <- 0L
  }

  possible <- gr$possible
  hitMask <- gr$hitMask

  if (!is.matrix(possible) || !is.matrix(hitMask)) {
    return(NULL)
  }

  untested <- possible & !hitMask
  if (!any(untested)) {
    return(NULL)
  }

  tested_possible <- possible & hitMask

  sat_untested <- build_sat(untested)
  sat_tested <- build_sat(tested_possible)

  coords <- generateRadiusAwareLawnmower(
    nr           = nr,
    nc           = nc,
    R            = R,
    albsDone     = gr$albsDone,
    albsLat      = gr$albsLat,
    albsLong     = gr$albsLong,
    albsRad      = gr$albsRad,
    allowPartial = allow_partial
  )

  if (nrow(coords) == 0L) {
    return(NULL)
  }

  n_cand <- nrow(coords)
  new_cells <- integer(n_cand)
  overlap_cells <- integer(n_cand)
  dist_sq <- numeric(n_cand)

  last <- gr$lastSearch
  last_lat <- NA_integer_
  last_long <- NA_integer_
  if (!is.null(last)) {
    if (!is.null(last$lat)) last_lat <- suppressWarnings(as.integer(last$lat))
    if (!is.null(last$long)) last_long <- suppressWarnings(as.integer(last$long))
  }

  for (i in seq_len(n_cand)) {
    cy <- coords[i, 1L]
    cx <- coords[i, 2L]

    r1 <- max(1L, cy - R)
    r2 <- min(nr, cy + R)
    c1 <- max(1L, cx - R)
    c2 <- min(nc, cx + R)

    if (r1 > r2 || c1 > c2) {
      new_cells[i] <- 0L
      overlap_cells[i] <- 0L
      dist_sq[i] <- NA_real_
    } else {
      new_cells[i] <- rect_sum(sat_untested, r1, c1, r2, c2)
      overlap_cells[i] <- rect_sum(sat_tested, r1, c1, r2, c2)

      if (!is.na(last_lat) && !is.na(last_long)) {
        dx <- as.numeric(cx - last_long)
        dy <- as.numeric(cy - last_lat)
        dist_sq[i] <- dx * dx + dy * dy
      } else {
        dist_sq[i] <- NA_real_
      }
    }
  }

  valid <- new_cells > 0L
  if (!any(valid)) {
    return(NULL)
  }

  cand_idx <- which(valid)

  if (isTRUE(prefer_no_overlap)) {
    no_overlap <- cand_idx[overlap_cells[cand_idx] == 0L]
    if (length(no_overlap) > 0L) {
      cand_idx <- no_overlap
    }
  }

  # 1) maximize new_cells
  max_new <- max(new_cells[cand_idx])
  best <- cand_idx[new_cells[cand_idx] == max_new]

  # 2) minimize overlap_cells
  if (length(best) > 1L) {
    overlaps <- overlap_cells[best]
    min_overlap <- min(overlaps)
    best <- best[overlaps == min_overlap]
  }

  # 3) minimize distance to last drop (squared)
  if (length(best) > 1L) {
    d <- dist_sq[best]
    d[is.na(d)] <- Inf
    min_d <- min(d)
    best <- best[d == min_d]
  }

  if (length(best) == 0L) {
    return(NULL)
  }

  idx <- best[[1L]]
  list(
    lat  = coords[idx, 1L],
    long = coords[idx, 2L]
  )
}

# ---------- Grid helpers ----------

new_grid <- function(id, nr, nc) {
  list(
    id         = id,
    nr         = as.integer(nr),
    nc         = as.integer(nc),
    possible   = matrix(TRUE, nrow = nr, ncol = nc),
    hitMask    = matrix(FALSE, nrow = nr, ncol = nc),
    hasHit     = FALSE,
    lastSearch = NULL,
    albsDone   = FALSE,
    albsLat    = NA_integer_,
    albsLong   = NA_integer_,
    albsRad    = NA_integer_,
    log        = .new_log_df(),
    history    = list()
  )
}

sorted_grid_names <- function(grids) {
  sort(names(grids), method = "radix")
}

# Normalize a grid loaded from possibly older RDS structures
normalize_grid <- function(gr, id = NA_character_) {
  if (!is.list(gr)) gr <- list()

  # ID
  if (is.null(gr$id)) {
    gr$id <- as.character(id %||% "grid")
  }

  # Possible matrix
  if (is.null(gr$possible) || !is.matrix(gr$possible)) {
    nr0 <- gr$nr %||% 50L
    nc0 <- gr$nc %||% 50L
    nr0 <- as.integer(nr0)
    if (is.na(nr0) || nr0 <= 0L) nr0 <- 50L
    nc0 <- as.integer(nc0)
    if (is.na(nc0) || nc0 <= 0L) nc0 <- 50L
    gr$possible <- matrix(TRUE, nrow = nr0, ncol = nc0)
  }
  nr <- nrow(gr$possible)
  nc <- ncol(gr$possible)
  gr$nr <- as.integer(nr)
  gr$nc <- as.integer(nc)

  # hitMask
  if (is.null(gr$hitMask) || !is.matrix(gr$hitMask) ||
    nrow(gr$hitMask) != nr || ncol(gr$hitMask) != nc) {
    gr$hitMask <- matrix(FALSE, nrow = nr, ncol = nc)
  }

  # flags & ALBS
  if (is.null(gr$hasHit)) gr$hasHit <- FALSE
  if (is.null(gr$lastSearch)) gr$lastSearch <- NULL
  if (is.null(gr$albsDone)) gr$albsDone <- FALSE

  for (nm in c("albsLat", "albsLong", "albsRad")) {
    if (is.null(gr[[nm]])) gr[[nm]] <- NA_integer_
  }

  # log
  if (is.null(gr$log) || !is.data.frame(gr$log)) {
    gr$log <- .new_log_df()
  } else {
    cols_target <- names(.new_log_df())
    missing_cols <- setdiff(cols_target, names(gr$log))
    if (length(missing_cols) > 0L) {
      for (nm in missing_cols) {
        gr$log[[nm]] <- NA
      }
    }
    gr$log <- gr$log[, cols_target, drop = FALSE]
  }

  # history (for undo)
  if (is.null(gr$history) || !is.list(gr$history)) {
    gr$history <- list()
  }

  gr
}

# ---------- Grid history helpers (undo support) ----------

grid_push_history <- function(gr, max_depth = 50L) {
  hist <- gr$history
  if (is.null(hist) || !is.list(hist)) {
    hist <- list()
  }

  snapshot <- list(
    possible   = gr$possible,
    hitMask    = gr$hitMask,
    hasHit     = gr$hasHit,
    lastSearch = gr$lastSearch,
    albsDone   = gr$albsDone,
    albsLat    = gr$albsLat,
    albsLong   = gr$albsLong,
    albsRad    = gr$albsRad
  )

  hist <- c(list(snapshot), hist)
  if (length(hist) > max_depth) {
    hist <- hist[seq_len(max_depth)]
  }

  gr$history <- hist
  gr
}

grid_undo <- function(gr) {
  hist <- gr$history
  if (is.null(hist) || length(hist) == 0L) {
    return(gr)
  }

  snapshot <- hist[[1L]]
  rest <- hist[-1L]

  fields <- c(
    "possible", "hitMask", "hasHit", "lastSearch",
    "albsDone", "albsLat", "albsLong", "albsRad"
  )

  for (nm in fields) {
    if (!is.null(snapshot[[nm]])) {
      gr[[nm]] <- snapshot[[nm]]
    }
  }

  gr$history <- rest
  gr
}
