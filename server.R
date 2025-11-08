### ------ server.R -----------

server <- function(input, output, session) {
  # ---------- Initial state (from RDS or defaults) ----------

  state <- loadState()

  default_radii <- tibble::tibble(
    name = c("RudimentaryProbe", "BasicProbe", "AdvancedProbe"),
    R    = c(1L, 2L, 3L)
  )

  grids0 <- list()
  radii0 <- default_radii
  color0 <- normalize_color_state(if (!is.null(state)) state$color_state else NULL)

  if (!is.null(state)) {
    if (!is.null(state$radiiDF)) radii0 <- state$radiiDF
    if (!is.null(state$grids)) grids0 <- state$grids
  }

  if (length(grids0) > 0L) {
    for (nm in names(grids0)) {
      grids0[[nm]] <- normalize_grid(grids0[[nm]], id = nm)
    }
  }

  if (length(grids0) == 0L) {
    grids0[["gridA"]] <- new_grid("gridA", nr = 50L, nc = 50L)
  }

  rv <- reactiveValues(
    grids       = grids0,
    radiiDF     = radii0,
    color_state = color0
  )

  # ---------- Helpers ----------

  radius_of <- function(name) {
    df <- rv$radiiDF
    if (is.null(df) || nrow(df) == 0L) {
      return(1L)
    }
    m <- df$name == name
    r <- df$R[m]
    if (length(r) == 0L || is.na(r[[1L]])) 1L else as.integer(r[[1L]])
  }

  current_grid_id <- reactive({
    gid <- input$gridID
    if (!is.null(gid) && gid %in% names(rv$grids)) {
      gid
    } else {
      if (length(rv$grids) > 0L) {
        sorted_grid_names(rv$grids)[[1L]]
      } else {
        NULL
      }
    }
  })

  current_grid <- reactive({
    gid <- current_grid_id()
    if (is.null(gid)) {
      return(NULL)
    }
    gr <- rv$grids[[gid]]
    if (is.null(gr)) {
      return(NULL)
    }
    gr <- normalize_grid(gr, id = gid)
    rv$grids[[gid]] <- gr
    gr
  })

  current_radius_name <- reactive({
    df <- rv$radiiDF
    if (is.null(df) || nrow(df) == 0L) {
      return(NA_character_)
    }
    nm <- input$radiusName
    if (is.null(nm) || is.na(nm) || !nzchar(nm) || !(nm %in% df$name)) {
      df$name[[1L]]
    } else {
      nm
    }
  })

  # ---------- Persist core state ----------
  # Save whenever rv or any input changes (aggressive auto-save).
  observe({
    rv_snapshot <- reactiveValuesToList(rv)
    input_snapshot <- reactiveValuesToList(input)
    try(saveState(STATE_FILE, rv), silent = TRUE)
  })

  # ---------- UI initialization ----------

  # Main grid selector (Search tab)
  observe({
    gids <- sorted_grid_names(rv$grids)
    sel <- current_grid_id()
    if (!is.null(sel) && !(sel %in% gids) && length(gids) > 0L) {
      sel <- gids[[1L]]
    }
    updateSelectInput(session, "gridID", choices = gids, selected = sel)
  })

  # Delete-grid selector (Grids tab)
  observe({
    gids <- sorted_grid_names(rv$grids)
    cur <- input$deleteGridID
    if (!is.null(cur) && cur %in% gids) {
      sel <- cur
    } else if (length(gids) > 0L) {
      sel <- gids[[1L]]
    } else {
      sel <- NULL
    }
    updateSelectInput(session, "deleteGridID", choices = gids, selected = sel)
  })

  # Sync probe choices
  observe({
    df <- rv$radiiDF
    if (is.null(df) || nrow(df) == 0L) {
      return()
    }

    labels <- sprintf("%s (%d)", df$name, df$R)
    choices <- stats::setNames(df$name, labels)

    updateSelectInput(
      session,
      "radiusName",
      choices  = choices,
      selected = current_radius_name()
    )
  })

  # Sync colors/palette from rv$color_state to UI
  observe({
    cs <- rv$color_state
    pal <- cs$palette %||% "magma"

    updateSelectInput(session, "viridisPalette", selected = pal)

    updateTextInput(session, "colPossible", value = cs$possible %||% "#FFFFFF")
    updateTextInput(session, "colTested", value = cs$tested %||% "#BBBBBB")
    updateTextInput(session, "colHit", value = cs$hit %||% "#00FF00")
    updateTextInput(session, "colMiss", value = cs$miss %||% "#FF0000")
    updateTextInput(session, "colSuggestion", value = cs$suggestion %||% "#FFFF00")
    updateTextInput(session, "colALBS", value = cs$albs %||% "#4444FF")
    updateTextInput(session, "colImpossible", value = cs$impossible %||% "#000000")
    updateTextInput(session, "colGridLines", value = cs$gridLines %||% "#777777")
  })

  # ---------- Radii table & editing ----------

  output$radiiTable <- renderTable(
    {
      rv$radiiDF
    },
    digits = 0
  )

  observeEvent(input$addRadius, {
    nm <- trimws(input$newRadiusName %||% "")
    R <- clamp_radius(input$newRadiusValue, fallback = NA_integer_)

    if (nm == "" || is.na(R)) {
      showNotification("Provide a probe name and a positive radius.", type = "error")
      return()
    }

    df <- rv$radiiDF
    if (nm %in% df$name) {
      df$R[df$name == nm] <- R
    } else {
      df <- dplyr::bind_rows(df, tibble::tibble(name = nm, R = R))
    }
    rv$radiiDF <- df
    updateTextInput(session, "newRadiusName", value = "")
    updateNumericInput(session, "newRadiusValue", value = NA_real_)
  })

  # ---------- ALBS status & auto-collapse ----------

  output$albsStatus <- renderText({
    gr <- current_grid()
    if (is.null(gr)) {
      return("No grid selected.")
    }
    if (!isTRUE(gr$albsDone)) {
      "ALBS: not applied."
    } else {
      sprintf(
        "ALBS active: center (Long (X)=%d, Lat (Y)=%d), radius %d.",
        gr$albsLong, gr$albsLat, gr$albsRad
      )
    }
  })

  observe({
    gr <- current_grid()
    if (is.null(gr)) {
      return()
    }
    show <- !isTRUE(gr$albsDone)
    updateCheckboxInput(session, "showALBS", value = show)

    if (!is.na(gr$albsLat) && !is.na(gr$albsLong) && !is.na(gr$albsRad)) {
      updateNumericInput(session, "albsLat", value = gr$albsLat)
      updateNumericInput(session, "albsLong", value = gr$albsLong)
      updateNumericInput(session, "albsRadius", value = gr$albsRad)
    }
  })

  observeEvent(input$applyALBS, {
    gid <- current_grid_id()
    gr <- current_grid()
    if (is.null(gid) || is.null(gr)) {
      showNotification("No grid selected.", type = "error")
      return()
    }

    nr <- gr$nr
    nc <- gr$nc

    cx <- as.integer(input$albsLong)
    cy <- as.integer(input$albsLat)
    rad <- clamp_radius(input$albsRadius, fallback = NA_integer_)

    if (is.na(cx) || is.na(cy) || is.na(rad)) {
      showNotification("Provide ALBS center (Long (X), Lat (Y)) and radius.", type = "error")
      return()
    }

    r1 <- max(1L, cy - rad)
    r2 <- min(nr, cy + rad)
    c1 <- max(1L, cx - rad)
    c2 <- min(nc, cx + rad)

    mask <- matrix(FALSE, nrow = nr, ncol = nc)
    if (r1 <= r2 && c1 <= c2) {
      mask[r1:r2, c1:c2] <- TRUE
    }

    gr$possible <- gr$possible & mask
    gr$albsDone <- TRUE
    gr$albsLat <- cy
    gr$albsLong <- cx
    gr$albsRad <- rad

    gr$hitMask <- gr$hitMask & gr$possible

    rv$grids[[gid]] <- gr

    updateCheckboxInput(session, "showALBS", value = FALSE)

    showNotification(sprintf("ALBS applied to grid '%s'.", gid), type = "message")
  })

  observeEvent(input$clearALBS, {
    gid <- current_grid_id()
    gr <- current_grid()
    if (is.null(gid) || is.null(gr)) {
      return()
    }

    gr$albsDone <- FALSE
    gr$albsLat <- NA_integer_
    gr$albsLong <- NA_integer_
    gr$albsRad <- NA_integer_

    rv$grids[[gid]] <- gr
    updateCheckboxInput(session, "showALBS", value = TRUE)
  })

  # ---------- Grid creation / deletion / import / export ----------

  observeEvent(input$createGrid, {
    id <- trimws(input$newGridID %||% "")
    if (id == "") {
      showNotification("Provide a grid ID.", type = "error")
      return()
    }
    nr <- as.integer(input$newGridRows)
    nc <- as.integer(input$newGridCols)
    if (is.na(nr) || is.na(nc) || nr < 5L || nc < 5L) {
      showNotification("Grid dimensions must be >= 5.", type = "error")
      return()
    }
    if (id %in% names(rv$grids)) {
      showNotification("Grid ID already exists.", type = "error")
      return()
    }
    rv$grids[[id]] <- new_grid(id, nr, nc)
    updateTextInput(session, "newGridID", value = "")
    updateSelectInput(session, "gridID", selected = id)
  })

  observeEvent(input$deleteGrid, {
    id <- input$deleteGridID
    gids <- names(rv$grids)

    if (is.null(id) || !nzchar(id) || !(id %in% gids)) {
      showNotification("No grid selected to delete.", type = "error")
      return()
    }

    if (length(gids) <= 1L) {
      showNotification("Cannot delete the last remaining grid.", type = "error")
      return()
    }

    rv$grids[[id]] <- NULL

    showNotification(sprintf("Grid '%s' deleted.", id), type = "message")
  })

  observeEvent(input$importState, {
    f <- input$importState
    if (is.null(f)) {
      return()
    }
    saved <- tryCatch(readRDS(f$datapath), error = function(e) NULL)
    if (is.null(saved)) {
      showNotification("Failed to read RDS file.", type = "error")
      return()
    }
    if (!is.null(saved$grids)) {
      grids_import <- saved$grids
      if (length(grids_import) > 0L) {
        for (nm in names(grids_import)) {
          grids_import[[nm]] <- normalize_grid(grids_import[[nm]], id = nm)
        }
      }
      rv$grids <- grids_import
    }
    rv$radiiDF <- saved$radiiDF %||% rv$radiiDF
    rv$color_state <- normalize_color_state(saved$color_state %||% rv$color_state)
    showNotification("State imported.", type = "message")
  })

  output$downloadState <- downloadHandler(
    filename = function() {
      paste0("search_state_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".RDS")
    },
    content = function(file) {
      saveState(file, rv)
    }
  )

  # ---------- Grids overview ----------

  output$gridsTable <- DT::renderDT({
    if (length(rv$grids) == 0L) {
      return(DT::datatable(data.frame(Message = "No grids defined yet."), rownames = FALSE))
    }

    df <- purrr::map_dfr(
      rv$grids,
      function(gr) {
        gr <- normalize_grid(gr, id = gr$id %||% NA_character_)
        remaining <- sum(gr$possible & !gr$hitMask)
        tibble::tibble(
          grid   = gr$id,
          rows   = gr$nr,
          cols   = gr$nc,
          ALBS   = if (isTRUE(gr$albsDone)) "yes" else "no",
          remain = remaining
        )
      }
    )

    DT::datatable(df, rownames = FALSE, options = list(pageLength = 10L))
  })

  # ---------- Color management ----------

  observeEvent(input$viridisPalette, {
    pal <- input$viridisPalette %||% (rv$color_state$palette %||% "magma")
    cs <- rv$color_state
    cs$palette <- pal
    rv$color_state <- cs
  })

  observeEvent(input$shufflePalette, {
    pal <- input$viridisPalette %||% (rv$color_state$palette %||% "magma")
    cols <- shuffle_viridis_colors(pal, 8L)

    cs <- rv$color_state
    cs$palette <- pal
    cs$possible <- cols[[1L]]
    cs$tested <- cols[[2L]]
    cs$hit <- cols[[3L]]
    cs$miss <- cols[[4L]]
    cs$suggestion <- cols[[5L]]
    cs$albs <- cols[[6L]]
    cs$impossible <- cols[[7L]]
    cs$gridLines <- cols[[8L]]
    rv$color_state <- cs
  })

  observeEvent(input$colPossible, {
    cs <- rv$color_state
    pal <- cs$palette %||% "magma"
    def <- get_viridis_colors(pal, 8L)[[1L]]
    cs$possible <- safe_color(input$colPossible, cs$possible %||% def)
    rv$color_state <- cs
  })
  observeEvent(input$colTested, {
    cs <- rv$color_state
    pal <- cs$palette %||% "magma"
    def <- get_viridis_colors(pal, 8L)[[2L]]
    cs$tested <- safe_color(input$colTested, cs$tested %||% def)
    rv$color_state <- cs
  })
  observeEvent(input$colHit, {
    cs <- rv$color_state
    pal <- cs$palette %||% "magma"
    def <- get_viridis_colors(pal, 8L)[[3L]]
    cs$hit <- safe_color(input$colHit, cs$hit %||% def)
    rv$color_state <- cs
  })
  observeEvent(input$colMiss, {
    cs <- rv$color_state
    pal <- cs$palette %||% "magma"
    def <- get_viridis_colors(pal, 8L)[[4L]]
    cs$miss <- safe_color(input$colMiss, cs$miss %||% def)
    rv$color_state <- cs
  })
  observeEvent(input$colSuggestion, {
    cs <- rv$color_state
    pal <- cs$palette %||% "magma"
    def <- get_viridis_colors(pal, 8L)[[5L]]
    cs$suggestion <- safe_color(input$colSuggestion, cs$suggestion %||% def)
    rv$color_state <- cs
  })
  observeEvent(input$colALBS, {
    cs <- rv$color_state
    pal <- cs$palette %||% "magma"
    def <- get_viridis_colors(pal, 8L)[[6L]]
    cs$albs <- safe_color(input$colALBS, cs$albs %||% def)
    rv$color_state <- cs
  })
  observeEvent(input$colImpossible, {
    cs <- rv$color_state
    pal <- cs$palette %||% "magma"
    def <- get_viridis_colors(pal, 8L)[[7L]]
    cs$impossible <- safe_color(input$colImpossible, cs$impossible %||% def)
    rv$color_state <- cs
  })
  observeEvent(input$colGridLines, {
    cs <- rv$color_state
    pal <- cs$palette %||% "magma"
    def <- get_viridis_colors(pal, 8L)[[8L]]
    cs$gridLines <- safe_color(input$colGridLines, cs$gridLines %||% def)
    rv$color_state <- cs
  })

  output$colorPreviewPlot <- renderPlot({
    cs <- rv$color_state
    pal <- cs$palette %||% "magma"
    cols_def <- get_viridis_colors(pal, 8L)

    cols <- c(
      possible   = safe_color(cs$possible, cols_def[[1L]]),
      tested     = safe_color(cs$tested, cols_def[[2L]]),
      hit        = safe_color(cs$hit, cols_def[[3L]]),
      miss       = safe_color(cs$miss, cols_def[[4L]]),
      suggestion = safe_color(cs$suggestion, cols_def[[5L]]),
      albs       = safe_color(cs$albs, cols_def[[6L]]),
      impossible = safe_color(cs$impossible, cols_def[[7L]]),
      gridLines  = safe_color(cs$gridLines, cols_def[[8L]])
    )

    n <- length(cols)
    par(mar = c(2, 2, 3, 1))
    plot(0, 0,
      type = "n",
      xlim = c(0, n), ylim = c(0, 1),
      axes = FALSE, xlab = "", ylab = ""
    )
    i <- 0
    for (nm in names(cols)) {
      rect(i, 0, i + 1, 1, col = cols[[nm]], border = NA)
      text(i + 0.5, 0.5, labels = nm, cex = 0.8)
      i <- i + 1
    }
    title("Current color roles")
  })

  # ---------- Suggestion logic wiring ----------

  suggestion <- reactive({
    gr <- current_grid()
    if (is.null(gr)) {
      return(NULL)
    }
    nm <- current_radius_name()
    if (is.null(nm) || is.na(nm)) {
      return(NULL)
    }
    R <- radius_of(nm)
    if (is.na(R) || R < 0L) R <- 0L

    prefer_no_overlap <- isTRUE(input$preferNoOverlap)
    allow_partial <- isTRUE(input$allowPartial)

    suggest_next_center(
      gr,
      R,
      prefer_no_overlap = prefer_no_overlap,
      allow_partial     = allow_partial
    )
  })

  observeEvent(suggestion(),
    {
      s <- suggestion()
      if (is.null(s)) {
        return()
      }
      if (is.na(input$dropLong) || is.na(input$dropLat)) {
        updateNumericInput(session, "dropLong", value = s$long)
        updateNumericInput(session, "dropLat", value = s$lat)
      }
    },
    ignoreNULL = TRUE
  )

  observeEvent(input$useSuggestion, {
    s <- suggestion()
    if (is.null(s)) {
      return()
    }
    updateNumericInput(session, "dropLong", value = s$long)
    updateNumericInput(session, "dropLat", value = s$lat)
  })

  output$suggestedDropTop <- renderText({
    gr <- current_grid()
    if (is.null(gr)) {
      return("Suggested drop: none available")
    }

    mask <- gr$possible & !gr$hitMask
    idx <- which(mask, arr.ind = TRUE)
    n <- nrow(idx)

    if (n == 0L) {
      return("Suggested drop: none available")
    }

    if (n == 1L) {
      x <- idx[1, "col"]
      y <- idx[1, "row"]
      return(sprintf(
        "Suggested drop: %d Long (X), %d Lat (Y) (final remaining candidate)",
        x, y
      ))
    }

    s <- suggestion()
    if (is.null(s)) {
      return("Suggested drop: none available")
    }
    sprintf("Suggested drop: %d Long (X), %d Lat (Y)", s$long, s$lat)
  })

  # Constraint summary line – only when debug overlay is enabled
  output$constraintSummary <- renderText({
    if (!isTRUE(input$debugOverlay)) {
      return("")
    }

    gr <- current_grid()
    if (is.null(gr)) {
      return("")
    }

    remaining <- sum(gr$possible & !gr$hitMask)

    lg <- gr$log
    if (is.null(lg) || !is.data.frame(lg) || nrow(lg) == 0L) {
      return(sprintf("Constraints: no directional hits yet. Candidates: %d.", remaining))
    }

    hits_idx <- lg$action == "Drop" &
      !is.na(lg$outcome) & lg$outcome == "hit" &
      !is.na(lg$direction) & lg$direction != ""

    if (!any(hits_idx)) {
      return(sprintf("Constraints: no directional hits yet. Candidates: %d.", remaining))
    }

    dirs <- sort(unique(lg$direction[hits_idx]))
    n_hits <- sum(hits_idx)

    sprintf(
      "Constraints: %d directional hit(s) [%s]. Candidates: %d.",
      n_hits,
      paste(dirs, collapse = ", "),
      remaining
    )
  })

  # ---------- Remaining cells summary ----------

  output$remainingCellsTop <- renderText({
    gr <- current_grid()
    if (is.null(gr)) {
      return("No grid selected.")
    }
    remaining <- sum(gr$possible & !gr$hitMask)
    sprintf("Remaining candidate cells: %d", remaining)
  })

  output$remainingCoordsTop <- renderText({
    gr <- current_grid()
    if (is.null(gr) || !isTRUE(gr$hasHit)) {
      return("")
    }
    mask <- gr$possible & !gr$hitMask
    idx <- which(mask, arr.ind = TRUE)
    n <- nrow(idx)
    if (n == 0L) {
      return("No remaining candidate coordinates (all cells tested).")
    }
    X <- idx[, "col"]
    Y <- idx[, "row"]
    if (n == 1L) {
      return(sprintf(
        "Final remaining candidate coordinate for grid '%s': Long (X)=%d, Lat (Y)=%d",
        gr$id, X[[1L]], Y[[1L]]
      ))
    }
    max_show <- 400L
    show_idx <- seq_len(min(n, max_show))
    parts <- sprintf("(Long (X)=%d, Lat (Y)=%d)", X[show_idx], Y[show_idx])
    header <- sprintf(
      "Remaining candidate coordinates for grid '%s' (%d total):",
      gr$id, n
    )
    if (n > max_show) {
      tail_msg <- sprintf("... and %d more.", n - max_show)
      paste0(header, "\n", paste(parts, collapse = ", "), "\n", tail_msg)
    } else {
      paste0(header, "\n", paste(parts, collapse = ", "))
    }
  })

  # ---------- Grid plot (ggplot2) ----------

  output$gridPlot <- renderPlot({
    gr <- current_grid()
    if (is.null(gr)) {
      ggplot() +
        theme_void() +
        annotate("text", 0, 0, label = "No grid selected.")
      return()
    }

    nr <- gr$nr
    nc <- gr$nc
    if (is.null(nr) || is.null(nc) || is.na(nr) || is.na(nc) || nr < 1L || nc < 1L) {
      ggplot() +
        theme_void() +
        annotate("text", 0, 0, label = "Invalid grid dimensions.")
      return()
    }

    r_view1 <- 1L
    r_view2 <- nr
    c_view1 <- 1L
    c_view2 <- nc

    if (isTRUE(gr$albsDone) &&
      !is.na(gr$albsLat) &&
      !is.na(gr$albsLong) &&
      !is.na(gr$albsRad)) {
      cx_albs <- gr$albsLong
      cy_albs <- gr$albsLat
      rad_albs <- gr$albsRad

      r1 <- max(1L, cy_albs - rad_albs)
      r2 <- min(nr, cy_albs + rad_albs)
      c1 <- max(1L, cx_albs - rad_albs)
      c2 <- min(nc, cx_albs + rad_albs)

      if (r1 <= r2 && c1 <= c2) {
        r_view1 <- r1
        r_view2 <- r2
        c_view1 <- c1
        c_view2 <- c2
      }
    }

    cs <- rv$color_state
    pal <- cs$palette %||% "magma"
    cols_def <- get_viridis_colors(pal, 8L)

    col_possible <- safe_color(cs$possible, cols_def[[1L]])
    col_tested <- safe_color(cs$tested, cols_def[[2L]])
    col_hit <- safe_color(cs$hit, cols_def[[3L]])
    col_miss <- safe_color(cs$miss, cols_def[[4L]])
    col_suggestion <- safe_color(cs$suggestion, cols_def[[5L]])
    col_albs <- safe_color(cs$albs, cols_def[[6L]])
    col_impossible <- safe_color(cs$impossible, cols_def[[7L]])
    col_gridLines <- safe_color(cs$gridLines, cols_def[[8L]])

    possible <- gr$possible
    tested <- gr$hitMask

    rows <- r_view1:r_view2
    cols <- c_view1:c_view2

    df <- expand.grid(
      row = rows,
      col = cols
    )
    df$possible <- as.logical(possible[cbind(df$row, df$col)])
    df$tested <- as.logical(tested[cbind(df$row, df$col)])

    df$fill <- dplyr::case_when(
      !df$possible ~ "impossible",
      df$tested ~ "tested",
      TRUE ~ "possible"
    )

    fill_map <- c(
      possible   = col_possible,
      tested     = col_tested,
      impossible = col_impossible
    )

    p <- ggplot(df, aes(
      xmin = col - 0.5, xmax = col + 0.5,
      ymin = row - 0.5, ymax = row + 0.5,
      fill = fill
    )) +
      geom_rect(color = NA) +
      scale_fill_manual(values = fill_map, guide = "none") +
      scale_x_continuous(
        name   = "Long (X)",
        breaks = cols,
        limits = c(c_view1 - 0.5, c_view2 + 0.5),
        expand = c(0, 0)
      ) +
      scale_y_reverse(
        name   = "Lat (Y)",
        breaks = rows,
        limits = c(r_view1 - 0.5, r_view2 + 0.5),
        expand = c(0, 0)
      ) +
      coord_fixed() +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid = element_blank(),
        axis.text = element_text(color = "grey50", size = 8),
        axis.title = element_text(color = "grey40", size = 9),
        axis.ticks = element_line(color = "grey70"),
        axis.ticks.length = unit(2, "pt"),
        plot.margin = margin(5, 5, 5, 5),
        panel.border = element_rect(fill = NA, color = NA)
      )

    p <- p +
      geom_vline(
        xintercept = seq(c_view1 - 0.5, c_view2 + 0.5, by = 1),
        color = col_gridLines, linewidth = 0.2
      ) +
      geom_hline(
        yintercept = seq(r_view1 - 0.5, r_view2 + 0.5, by = 1),
        color = col_gridLines, linewidth = 0.2
      )

    # Debug overlay: candidate cells and direction line
    if (isTRUE(input$debugOverlay)) {
      candidates <- possible & !tested
      df$candidate <- as.logical(candidates[cbind(df$row, df$col)])

      cand_df <- df[df$candidate, , drop = FALSE]
      if (nrow(cand_df) > 0L) {
        p <- p + geom_rect(
          data = cand_df,
          inherit.aes = FALSE,
          aes(
            xmin = col - 0.5, xmax = col + 0.5,
            ymin = row - 0.5, ymax = row + 0.5
          ),
          fill = NA,
          color = col_suggestion,
          linewidth = 0.4
        )
      }

      last <- gr$lastSearch
      if (!is.null(last) && !is.null(last$dir) && nzchar(last$dir)) {
        R_line <- last$R %||% 0L
        line_mask <- direction_line_mask(
          nr  = nr,
          nc  = nc,
          cx  = last$long,
          cy  = last$lat,
          dir = last$dir,
          R   = R_line
        )
        if (!is.null(line_mask) && any(line_mask)) {
          idx_line <- which(line_mask, arr.ind = TRUE)
          df_line <- as.data.frame(idx_line)
          names(df_line) <- c("row", "col")

          df_line <- df_line[
            df_line$row >= r_view1 & df_line$row <= r_view2 &
              df_line$col >= c_view1 & df_line$col <= c_view2, ,
            drop = FALSE
          ]

          if (nrow(df_line) > 0L) {
            p <- p + geom_point(
              data = df_line,
              inherit.aes = FALSE,
              aes(x = col, y = row),
              shape = 4,
              size = 2,
              stroke = 0.7,
              color = col_hit
            )
          }
        }
      }
    }

    if (isTRUE(gr$albsDone) &&
      !is.na(gr$albsLat) &&
      !is.na(gr$albsLong) &&
      !is.na(gr$albsRad)) {
      cx <- gr$albsLong
      cy <- gr$albsLat
      rad <- gr$albsRad

      r1 <- max(1L, cy - rad)
      r2 <- min(nr, cy + rad)
      c1 <- max(1L, cx - rad)
      c2 <- min(nc, cx + rad)

      p <- p + annotate(
        "rect",
        xmin = c1 - 0.5,
        xmax = c2 + 0.5,
        ymin = r1 - 0.5,
        ymax = r2 + 0.5,
        fill = NA,
        color = col_albs,
        linewidth = 0.6
      )
    }

    s <- suggestion()
    if (!is.null(s)) {
      p <- p + annotate(
        "point",
        x = s$long,
        y = s$lat,
        shape = 21,
        size = 3,
        fill = col_suggestion,
        color = col_gridLines,
        stroke = 0.7
      )
    }

    pending_center <- NULL
    cx_input <- suppressWarnings(as.integer(input$dropLong))
    cy_input <- suppressWarnings(as.integer(input$dropLat))
    if (!is.null(cx_input) && !is.na(cx_input) &&
      !is.null(cy_input) && !is.na(cy_input) &&
      cx_input >= 1L && cy_input >= 1L &&
      cx_input <= nc && cy_input <= nr) {
      pending_center <- list(long = cx_input, lat = cy_input)
    } else if (!is.null(s)) {
      pending_center <- s
    }

    if (!is.null(pending_center)) {
      nm <- current_radius_name()
      if (!is.null(nm) && !is.na(nm) && nzchar(nm)) {
        R <- radius_of(nm)
        if (!is.na(R) && R >= 0L) {
          r1 <- max(1L, pending_center$lat - R)
          r2 <- min(nr, pending_center$lat + R)
          c1 <- max(1L, pending_center$long - R)
          c2 <- min(nc, pending_center$long + R)

          r1_view <- max(r1, r_view1)
          r2_view <- min(r2, r_view2)
          c1_view <- max(c1, c_view1)
          c2_view <- min(c2, c_view2)

          if (r1_view <= r2_view && c1_view <= c2_view) {
            p <- p + annotate(
              "rect",
              xmin = c1_view - 0.5,
              xmax = c2_view + 0.5,
              ymin = r1_view - 0.5,
              ymax = r2_view + 0.5,
              fill = NA,
              color = col_suggestion,
              linewidth = 0.9,
              linetype = "dashed"
            )
          }
        }
      }
    }

    p
  })

  # ---------- Click handler ----------

  observeEvent(input$gridClick, {
    gr <- current_grid()
    if (is.null(gr)) {
      return()
    }

    x <- input$gridClick$x
    y <- input$gridClick$y
    if (is.null(x) || is.null(y)) {
      return()
    }

    nr <- gr$nr
    nc <- gr$nc

    cx <- as.integer(round(x))
    cy <- y_to_row(y, nr)

    if (cx < 1L || cy < 1L || cx > nc || cy > nr) {
      return()
    }

    updateNumericInput(session, "dropLong", value = cx)
    updateNumericInput(session, "dropLat", value = cy)
  })

  # ---------- Move logging & undo ----------

  log_event_grid <- function(grid_id, row) {
    gr <- rv$grids[[grid_id]]
    gr <- normalize_grid(gr, id = grid_id)
    lg <- gr$log
    lg2 <- log_event_df(lg, row)
    gr$log <- lg2
    rv$grids[[grid_id]] <- gr
  }

  observeEvent(input$doDrop, {
    gid <- current_grid_id()
    gr <- current_grid()
    if (is.null(gid) || is.null(gr)) {
      showNotification("No grid selected.", type = "error")
      return()
    }

    cx <- as.integer(input$dropLong)
    cy <- as.integer(input$dropLat)
    if (is.na(cx) || is.na(cy)) {
      showNotification("Provide drop coordinates.", type = "error")
      return()
    }

    nm <- current_radius_name()
    if (is.null(nm) || is.na(nm)) {
      showNotification("No probe selected.", type = "error")
      return()
    }

    R <- radius_of(nm)
    if (is.na(R) || R < 0L) R <- 0L

    gr <- grid_push_history(gr)

    nr <- gr$nr
    nc <- gr$nc

    r1 <- max(1L, cy - R)
    r2 <- min(nr, cy + R)
    c1 <- max(1L, cx - R)
    c2 <- min(nc, cx + R)

    rows <- if (r1 <= r2) seq.int(r1, r2) else integer(0)
    cols <- if (c1 <= c2) seq.int(c1, c2) else integer(0)

    test_mask <- matrix(FALSE, nrow = nr, ncol = nc)
    if (length(rows) > 0L && length(cols) > 0L) {
      test_mask[rows, cols] <- TRUE
      area_eff <- length(rows) * length(cols)
    } else {
      area_eff <- 0L
    }

    remaining_before <- sum(gr$possible & !gr$hitMask)

    outcome <- input$outcome %||% "miss"
    dir <- if (identical(outcome, "hit")) input$pingDirection %||% "" else ""

    if (identical(outcome, "hit") && nzchar(dir)) {
      line_mask <- direction_line_mask(
        nr  = nr,
        nc  = nc,
        cx  = cx,
        cy  = cy,
        dir = dir,
        R   = R
      )
      if (!is.null(line_mask)) {
        to_mark <- test_mask & !line_mask
      } else {
        to_mark <- test_mask
      }
    } else {
      to_mark <- test_mask
    }

    gr$hitMask[to_mark] <- TRUE

    remaining_after <- sum(gr$possible & !gr$hitMask)
    cellsChecked <- area_eff
    ratio <- if (area_eff > 0L) (remaining_before - remaining_after) / area_eff else 0

    if (identical(outcome, "hit")) {
      gr$hasHit <- TRUE
    }

    dist_last <- NA_real_
    if (!is.null(gr$lastSearch)) {
      dx <- cx - gr$lastSearch$long
      dy <- cy - gr$lastSearch$lat
      dist_last <- sqrt(dx * dx + dy * dy)
    }

    if (identical(outcome, "hit")) {
      gr <- constrain_grid_by_direction(gr, cx, cy, dir, R)
    }

    gr$lastSearch <- list(
      lat  = cy,
      long = cx,
      R    = R,
      dir  = dir
    )

    rv$grids[[gid]] <- gr

    row <- list(
      grid             = gid,
      action           = "Drop",
      long             = cx,
      lat              = cy,
      radiusName       = nm,
      radiusVal        = R,
      outcome          = outcome,
      direction        = dir,
      stage            = if (ratio >= 1) "FULL" else "PARTIAL",
      ratio            = ratio,
      remaining        = remaining_after,
      cellsChecked     = cellsChecked,
      distanceFromLast = dist_last
    )
    log_event_grid(gid, row)
  })

  observeEvent(input$resetGrid, {
    gid <- current_grid_id()
    gr <- current_grid()
    if (is.null(gid) || is.null(gr)) {
      return()
    }

    gr$possible <- matrix(TRUE, nrow = gr$nr, ncol = gr$nc)
    gr$hitMask <- matrix(FALSE, nrow = gr$nr, ncol = gr$nc)
    gr$hasHit <- FALSE
    gr$lastSearch <- NULL
    gr$albsDone <- FALSE
    gr$albsLat <- NA_integer_
    gr$albsLong <- NA_integer_
    gr$albsRad <- NA_integer_

    gr$history <- list()

    gr$log <- log_event_df(gr$log, list(
      grid   = gid,
      action = "Reset"
    ))

    rv$grids[[gid]] <- gr
  })

  observeEvent(input$undoDrop, {
    gid <- current_grid_id()
    gr <- current_grid()
    if (is.null(gid) || is.null(gr)) {
      return()
    }

    if (is.null(gr$history) || length(gr$history) == 0L) {
      showNotification("No more drops to undo for this grid.", type = "message")
      return()
    }

    gr <- grid_undo(gr)
    rv$grids[[gid]] <- gr

    log_event_grid(gid, list(
      grid   = gid,
      action = "Undo"
    ))
  })

  # ---------- Move log tab ----------

  output$moveLogCurrentGridLabel <- renderText({
    gid <- current_grid_id()
    if (is.null(gid)) {
      "Move log (no grid selected)."
    } else {
      sprintf("Move log for grid '%s'", gid)
    }
  })

  output$moveLogTable <- DT::renderDT({
    gr <- current_grid()
    if (is.null(gr) || is.null(gr$log)) {
      df <- .new_log_df()
    } else {
      df <- gr$log
      if (!is.data.frame(df) || nrow(df) == 0L) {
        df <- .new_log_df()
      }
    }
    DT::datatable(df, options = list(pageLength = 15L))
  })
}
