# Anomaly Tracker (The Entreprenauts helper)

Anomaly Tracker is a small R/Shiny app that helps you plan and track searches on a grid for the game **[The Entreprenauts](https://www.theentreprenauts.com)**.

You can:
- Create one or more “grids” (maps made of squares)
- Drop different “probes” on the grid
- Mark each drop as a **hit** or **miss**, with a direction
- See which cells are still possible and get a suggested next drop

It’s meant as a simple helper tool for players, not an official product.

> ⚠️ **Disclaimer**
>
> - This is an **unofficial fan-made helper** for The Entreprenauts. It is not affiliated with or endorsed by the creators of the game.  
> - A large part of this code was generated and refined using **ChatGPT (OpenAI)** and then adjusted by a human. Use at your own risk.

---

## What the app can do

- **Search**
  - Pick an active grid and a probe type (radius).
  - Enter coordinates by hand or click the grid and use **“Use suggested position”**.
  - Mark the result as **Hit** or **Miss**, and (for hits) choose a direction (N, NE, E, …, Below).
  - The app shows:
    - How many candidate cells are left.
    - A suggested next drop location.
    - (Optional) a **debug overlay** to visualise constraints and candidate cells.

- **ALBS window (sub-area search)**
  - Restrict the search to a smaller square region (center + radius).
  - Turn ALBS on/off and see its status as a short text line.

- **Grids**
  - Create new grids with custom sizes (rows/columns).
  - Delete grids (with a check so you can’t delete the last one).
  - See an overview table: grid name, size, ALBS active or not, and remaining candidates.

- **Move log**
  - See a full history of your actions for the selected grid:
    - Drops, resets, undo steps
    - Coordinates, radius, hit/miss, direction, remaining cells, etc.

- **Colours**
  - Use a viridis-based colour scheme.
  - Randomise/shuffle colours with one click.
  - Customise colours for:
    - Possible / tested / hit / miss cells
    - Suggested drop, ALBS outline, impossible cells, grid lines
  - See a small preview of all colour roles.

- **Settings**
  - Choose how the probe path is generated:
    - Prefer no overlap
    - Allow/forbid partial coverage at edges
    - Wrap rows or not
  - Add or change named probe radii (e.g. “RudimentaryProbe”, “BasicProbe”, etc.).

- **Help**
  - Short in-app explanation with tips on how to use each tab.

---

## Requirements

You need:

- R (version 4.x or newer is recommended)
- The following R packages:
  - `shiny`
  - `dplyr`
  - `purrr`
  - `tibble`
  - `DT`
  - `viridisLite`
  - `shinyBS`
  - `shinyvalidate`
  - `ggplot2`
  - Optional for development: `reactlog`

If you’re not familiar with R, use the helper script described below instead of installing manually.

---

## Installing required R packages (easy way)

This repo includes a small helper script: **`install_packages.R`**.

1. Open R or RStudio in the folder where the project lives (where `ui.R`, `server.R`, `global.R`, and `install_packages.R` are).
2. Run:

   ```r
   source("install_packages.R")
