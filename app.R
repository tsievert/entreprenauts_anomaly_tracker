# -------- app.R --------
# Single, clean entry point so RStudio's "Run App" and shinytest2
# both see exactly one app in this directory.

source("global.R", local = TRUE) # helpers, state, globals (no ui/server here)
source("ui.R", local = TRUE) # defines `ui`
source("server.R", local = TRUE) # defines `server`

shinyApp(ui = ui, server = server)
