---
editor_options: 
  markdown: 
    wrap: 72
---

# Anomaly Tracker (The Entreprenauts helper)

Anomaly Tracker is a small R/Shiny app that helps you plan and track
searches on a grid for the game [**The
Entreprenauts**](https://www.theentreprenauts.com).

You can: - Create one or more “grids” (maps made of squares) - Drop
different “probes” on the grid - Mark each drop as a **hit** or
**miss**, with a direction - See which cells are still possible and get
a suggested next drop

It’s meant as a simple helper tool for players, not an official product.

> ⚠️ **Disclaimer**
>
> -   This is an **unofficial fan-made helper** for The Entreprenauts.
>     It is not affiliated with or endorsed by the creators of the
>     game.\
> -   A large part of this code was generated and refined using
>     **ChatGPT (OpenAI)** and then adjusted by a human. Use at your own
>     risk.

------------------------------------------------------------------------

## 🔌 Quick start (TL;DR)

1. Double-click the project file (for example
`entreprenauts_anomaly_tracker.Rproj`) to open the folder in
**RStudio**. 2. In the RStudio **Console**, run:

``` r
# First time only:
source("install_packages.R")
```
This will install all required packages before you start the app the first time.
Then:

``` r
# Every time you want to start the app:
library(shiny)
runApp()
```

3.  Your browser should open the **Anomaly Tracker** app.

If that works, you can stop reading here and just use the app. 🙂

------------------------------------------------------------------------

## What the app can do

-   **Search**
    -   Pick an active grid and a probe type (radius).
    -   Enter coordinates by hand or click on the grid, then use **“Use
        suggested position”**.
    -   Mark the result as **Hit** or **Miss**, and (for hits) choose a
        direction (N, NE, E, …, Below).
    -   The app shows:
        -   How many candidate cells are left.
        -   A suggested next drop location.
        -   (Optional) a **debug overlay** to visualise constraints and
            candidate cells.
-   **ALBS window (sub-area search)**
    -   Restrict the search to a smaller square region (center +
        radius).
    -   Turn ALBS on/off and see its status as a short text line.
-   **Grids**
    -   Create new grids with custom sizes (rows/columns).
    -   Delete grids (with a check so you can’t delete the last one).
    -   See an overview table: grid name, size, ALBS active or not, and
        remaining candidates.
-   **Move log**
    -   See a full history of your actions for the selected grid:
        -   Drops, resets, undo steps
        -   Coordinates, radius, hit/miss, direction, remaining cells,
            etc.
-   **Colours**
    -   Use a viridis-based colour scheme.
    -   Randomise/shuffle colours with one click.
    -   Customise colours for:
        -   Possible / tested / hit / miss cells
        -   Suggested drop, ALBS outline, impossible cells, grid lines
    -   See a small preview of all colour roles.
-   **Settings**
    -   Choose how the probe path is generated:
        -   Prefer no overlap
        -   Allow/forbid partial coverage at edges
        -   Wrap rows or not
    -   Add or change named probe radii (e.g. “RudimentaryProbe”,
        “BasicProbe”, etc.).
-   **Help**
    -   Short in-app explanation with tips on how to use each tab.

------------------------------------------------------------------------

## Requirements

You need:

-   **R** (version 4.x or newer is recommended)
-   **RStudio** (recommended, makes things much easier)
-   These R packages:
    -   `shiny`
    -   `dplyr`
    -   `purrr`
    -   `tibble`
    -   `DT`
    -   `viridisLite`
    -   `shinyBS`
    -   `shinyvalidate`
    -   `ggplot2`
    -   Optional for development: `reactlog`

If you’re not familiar with R, use the helper script instead of
installing packages one by one.

------------------------------------------------------------------------

## Installing required R packages (easy way)

This project includes a helper script: **`install_packages.R`**.

1.  Locate the **project file** in this folder, for example:\
    `AnomalyTracker.Rproj`

2.  Double-click that file to open the project in **RStudio**.

3.  In the RStudio **Console**, run:

    ``` r
    source("install_packages.R")
    ```

The script will:

-   Check which required packages are missing.
-   Install only the missing ones.
-   Print a message when everything is ready.

You usually only need to do this once on a given computer.

------------------------------------------------------------------------

## How to start the Shiny app (full version)

Once the packages are installed:

1.  Open the project by double-clicking the `.Rproj` file (if it’s not
    already open in RStudio).

2.  In the RStudio **Console**, type:

    ``` r
    library(shiny)
    runApp()
    ```

3.  Your default web browser should open the **Anomaly Tracker** app.

4.  Use the tabs (**Search**, **Grids**, **Move log**, **Colors**,
    **Settings**, **Help**) to interact with it.

------------------------------------------------------------------------

## Files in this project

-   `ui.R`\
    Shiny user interface (tabs: Search, Grids, Move log, Colors,
    Settings, Help).

-   `server.R`\
    Server logic:

    -   Manages grids and probes.
    -   Applies ALBS constraints.
    -   Computes the next suggested drop.
    -   Records the move log.
    -   Draws the grid plot and colour previews.

-   `global.R`\
    Shared helper functions and data structures:

    -   Grid creation and normalisation.
    -   Path generation and suggestion logic.
    -   Colour handling.
    -   State saving/loading.

-   `install_packages.R`\
    Helper script to install all required R packages with one command.

-   `my_grids_state_default.RDS` (optional)\
    Example or default starting state for grids and settings (if
    provided).

------------------------------------------------------------------------

## State saving

The app automatically saves its state to:

-   `my_grids_state.RDS` (in the project folder)

You can also:

-   **Export** the current state as an `.RDS` file (from the *Grids*
    tab).
-   **Import** a previously saved `.RDS` file to restore grids, probes,
    and colours.
