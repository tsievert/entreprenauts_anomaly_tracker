## -------- ui.R --------

ui <- navbarPage(
  title = "Anomaly Tracker",
  id = "mainNav",
  theme = light_theme, # start light; server switches at runtime if needed
  header = tags$head(
    tags$style(HTML("
      h3, h4 {
        margin-top: 8px;
        margin-bottom: 6px;
      }
      .btn-wrap {
        white-space: normal;
      }
      /* tighten general vertical spacing inside wells */
      .well hr {
        margin-top: 8px;
        margin-bottom: 8px;
      }
      .well .form-group {
        margin-bottom: 8px;
      }
      /* tighter gaps + equal-ish height for bottom buttons */
      .drop-button-row .col-sm-4,
      .drop-button-row .col-md-4,
      .drop-button-row .col-lg-4,
      .drop-button-row .col-xs-4 {
        padding-left: 4px;
        padding-right: 4px;
      }
      .drop-button-row .btn {
        min-height: 48px;
        padding-top: 10px;
        padding-bottom: 10px;
        /* keep words intact; wrap only at spaces */
        white-space: normal !important;  /* override Bootstrap's nowrap */
        overflow-wrap: normal;           /* don't break within words */
        word-break: keep-all;            /* prevent mid-word breaks */
        hyphens: none;                   /* no auto hyphenation */
      }

      /* Ensure Shiny plot containers and the rendered <img> are transparent */
      #gridPlot, .shiny-plot-output { background: transparent !important; }
      .shiny-plot-output img { background: transparent !important; }

      /* =========================
         Selectize theming (selectInput)
         ========================= */

      /* Collapsed box (displayed value) */
      .selectize-control .selectize-input,
      .selectize-control .selectize-input.focus,
      .selectize-control.single .selectize-input.dropdown-active {
        background-color: var(--bs-body-bg) !important;
        color: var(--bs-body-color) !important;
        border-color: var(--bs-border-color, #495057) !important;
      }
      .selectize-control .selectize-input .item,
      .selectize-control .selectize-input input {
        color: var(--bs-body-color) !important;
      }
      .selectize-control.single .selectize-input::after {
        border-color: var(--bs-body-color) transparent transparent transparent !important;
      }

      /* OPEN DROPDOWN PANEL
         Force an opaque background using theme-scoped rules so it never renders transparent. */
      html[data-bs-theme='dark'] .selectize-dropdown,
      html[data-bs-theme='dark'] .selectize-dropdown .selectize-dropdown-content,
      html[data-bs-theme='dark'] .selectize-dropdown .optgroup,
      html[data-bs-theme='dark'] .selectize-dropdown .optgroup-header,
      html[data-bs-theme='dark'] .selectize-dropdown .option {
        background: #111 !important;   /* opaque dark */
        color: #eaeaea !important;
      }
      html[data-bs-theme='light'] .selectize-dropdown,
      html[data-bs-theme='light'] .selectize-dropdown .selectize-dropdown-content,
      html[data-bs-theme='light'] .selectize-dropdown .optgroup,
      html[data-bs-theme='light'] .selectize-dropdown .optgroup-header,
      html[data-bs-theme='light'] .selectize-dropdown .option {
        background: #ffffff !important; /* opaque light */
        color: #212529 !important;
      }

      /* Panel chrome: border, shadow, stacking */
      .selectize-dropdown {
        border: 1px solid var(--bs-border-color, #495057) !important;
        box-shadow: 0 6px 18px rgba(0,0,0,0.28) !important;
        z-index: 2000 !important; /* stay above content */
      }

      /* Active/hover option */
      html[data-bs-theme='dark'] .selectize-dropdown .option.active,
      html[data-bs-theme='dark'] .selectize-dropdown .option.selected,
      html[data-bs-theme='dark'] .selectize-dropdown .option:hover {
        background: rgba(255,255,255,0.08) !important;
        color: #eaeaea !important;
      }
      html[data-bs-theme='light'] .selectize-dropdown .option.active,
      html[data-bs-theme='light'] .selectize-dropdown .option.selected,
      html[data-bs-theme='light'] .selectize-dropdown .option:hover {
        background: #f2f2f2 !important;
        color: #212529 !important;
      }
    "))
  ),
  tabPanel(
    title = "Search",
    fluidRow(
      column(
        width = 4,
        wellPanel(
          h4("Active grid & probe"),
          selectInput("gridID", "Grid", choices = character(0)),
          selectInput("radiusName", "Probe type", choices = character(0)),
          hr(),
          bsCollapse(
            id = "dropControls",
            open = c("drop_coords", "drop_outcome"),
            bsCollapsePanel(
              "Drop coordinates",
              value = "drop_coords",
              # Long/Lat side by side, suggestion button full-width below
              fluidRow(
                column(
                  width = 6,
                  numericInput("dropLong", "Long (X)", value = NA_real_, min = 1)
                ),
                column(
                  width = 6,
                  numericInput("dropLat", "Lat (Y)", value = NA_real_, min = 1)
                )
              ),
              div(
                style = "margin-top: 10px;",
                actionButton(
                  "useSuggestion",
                  "Use suggested position",
                  class = "btn-block btn-wrap"
                )
              )
            ),
            bsCollapsePanel(
              "Outcome",
              value = "drop_outcome",
              radioButtons(
                "outcome",
                "Result of last drop",
                choices = c("Miss" = "miss", "Hit" = "hit"),
                inline = TRUE
              ),
              selectInput(
                "pingDirection",
                "Hit direction (if hit)",
                choices = c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "Below"),
                selected = "N"
              )
            )
          ),
          hr(),
          div(
            class = "drop-button-row",
            fluidRow(
              column(
                width = 4,
                actionButton(
                  "doDrop",
                  "Record drop",
                  class = "btn-primary btn-block btn-wrap"
                )
              ),
              column(
                width = 4,
                actionButton(
                  "resetGrid",
                  "Reset grid",
                  class = "btn-warning btn-block btn-wrap"
                )
              ),
              column(
                width = 4,
                actionButton(
                  "undoDrop",
                  "Undo last",
                  class = "btn-default btn-block btn-wrap"
                )
              )
            )
          )
        ),
        wellPanel(
          bsCollapse(
            id = "albsControls",
            open = "albs_constraints",
            bsCollapsePanel(
              "ALBS constraints",
              value = "albs_constraints",
              checkboxInput("showALBS", "Show ALBS controls", value = TRUE),
              bsTooltip(
                "showALBS",
                title = "Hide ALBS inputs after initial setup to free space. You can always re-open them.",
                placement = "right",
                trigger = "hover"
              ),
              conditionalPanel(
                condition = "input.showALBS",
                numericInput("albsLong", "ALBS center Long (X)", value = NA_real_, min = 1),
                numericInput("albsLat", "ALBS center Lat (Y)", value = NA_real_, min = 1),
                numericInput("albsRadius", "ALBS radius (grid cells)", value = NA_real_, min = 1),
                actionButton("applyALBS", "Apply / update ALBS window"),
                actionButton("clearALBS", "Clear ALBS", class = "btn-link")
              ),
              verbatimTextOutput("albsStatus")
            )
          )
        )
      ),
      column(
        width = 8,
        h4("Search status"),
        verbatimTextOutput("gridDimensions"),
        verbatimTextOutput("remainingCellsTop"),
        verbatimTextOutput("suggestedDropTop"),
        verbatimTextOutput("constraintSummary"),
        verbatimTextOutput("remainingCoordsTop"),
        # Responsive plot height: tracks viewport height
        div(
          style = "height: 70vh;",
          plotOutput(
            "gridPlot",
            hover = hoverOpts("gridHover", delay = 50, delayType = "debounce"),
            click = "gridClick",
            height = "100%"
          )
        )
      )
    )
  ),
  tabPanel(
    title = "Grids",
    fluidRow(
      column(
        width = 4,
        wellPanel(
          h4("Create grids"),
          textInput("newGridID", "New grid ID", value = "gridA"),
          numericInput("newGridCols", "Columns / Long (X)", value = 50, min = 5, step = 1),
          numericInput("newGridRows", "Rows / Lat (Y)", value = 50, min = 5, step = 1),
          actionButton("createGrid", "Add grid")
        ),
        wellPanel(
          h4("Delete grids"),
          selectInput("deleteGridID", "Grid to delete", choices = character(0)),
          actionButton("deleteGrid", "Delete selected grid", class = "btn-danger btn-block btn-wrap")
        ),
        wellPanel(
          h4("Import / export"),
          fileInput("importState", "Import state (.RDS)", accept = ".RDS"),
          downloadButton("downloadState", "Export current state")
        )
      ),
      column(
        width = 8,
        h4("Existing grids"),
        DTOutput("gridsTable")
      )
    )
  ),
  tabPanel(
    title = "Move log",
    fluidRow(
      column(
        width = 12,
        h4(textOutput("moveLogCurrentGridLabel")),
        DTOutput("moveLogTable")
      )
    )
  ),
  tabPanel(
    title = "Colors",
    fluidRow(
      column(
        width = 4,
        wellPanel(
          h4("Viridis palette"),
          selectInput(
            "viridisPalette",
            "Base palette",
            choices = c("magma", "inferno", "plasma", "viridis", "cividis")
          ),
          actionButton("shufflePalette", "Shuffle & jitter"),
          bsTooltip(
            "shufflePalette",
            title = "Shuffle the assignment and add slight jitter along the viridis gradient.",
            placement = "right",
            trigger = "hover"
          )
        ),
        wellPanel(
          h4("Grid colors"),
          textInput("colPossible", "Untested / possible cells", value = ""),
          textInput("colTested", "Tested cells", value = ""),
          textInput("colHit", "Remaining candidates after hit / hit markers / debug line", value = ""),
          textInput("colMiss", "Miss markers", value = ""),
          textInput("colSuggestion", "Suggested drop center / outline", value = ""),
          textInput("colALBS", "ALBS window outline", value = ""),
          textInput("colImpossible", "Impossible / excluded area", value = ""),
          textInput("colGridLines", "Grid lines", value = "")
        )
      ),
      column(
        width = 8,
        h4("Color preview"),
        plotOutput("colorPreviewPlot", height = "220px")
      )
    )
  ),
  tabPanel(
    title = "Settings",
    fluidRow(
      column(
        width = 6,
        wellPanel(
          h4("Pathing preferences"),
          checkboxInput("preferNoOverlap", "Prefer drops with no overlap", value = TRUE),
          checkboxInput("allowPartial", "Allow partial coverage at edges", value = TRUE),
          checkboxInput("wrapRows", "Wrap search when end of row reached", value = TRUE),
          checkboxInput("debugOverlay", "Show debug overlay (constraints & candidates)", value = FALSE),
          checkboxInput("showDropMarkers", "Show markers for previous drops", value = TRUE),
          tags$hr(),
          checkboxInput("darkMode", "Dark mode", value = FALSE)
        )
      ),
      column(
        width = 6,
        wellPanel(
          h4("Probe radii"),
          tableOutput("radiiTable"),
          br(),
          textInput("newRadiusName", "New probe name", value = ""),
          numericInput("newRadiusValue", "New probe radius (R)", value = NA_real_, min = 1),
          actionButton("addRadius", "Add / update probe")
        )
      )
    )
  ),
  tabPanel(
    title = "Help",
    fluidRow(
      column(
        width = 8,
        offset = 2,
        h3("Anomaly tracker"),
        p(
          "Use the Search tab to step through a grid using probes of different radii.",
          "The app suggests the next drop based on remaining untested cells."
        ),
        tags$ul(
          tags$li("Use the ALBS section to restrict the search to a sub-window."),
          tags$li("The Move log tab shows a detailed history of your drops for the currently selected grid."),
          tags$li("Use the Colors tab to customize the viridis-based color mapping and grid lines."),
          tags$li("Turn on the debug overlay in Settings to visualise constraints and candidate cells.")
        )
      )
    )
  )
)
