# TOTA Wastewater Dashboard Maintenance

https://tota-waste-water-dashboard.share.connect.posit.cloud/


This document is intended to serve as a technical handover document for future TOTA staff responsible for maintaining or expanding the project.

---

## Table of Contents

* [How the Application Works](#how-the-application-works)
* [How the Data Refresh System Works](#how-the-data-refresh-system-works)
* [Running the Dashboard Locally](#running-the-dashboard-locally)
* [Secrets and variables for Sensitive URLs](#secrets-and-variables-for-sensitive-urls)
* [How to Update the Dashboard](#how-to-update-the-dashboard)
* [Adding New Data Sources](#adding-new-data-sources)
* [Troubleshooting](#troubleshooting)
* [Important Maintenance Notes](#important-maintenance-notes)
* [Recommended Handover Checklist](#recommended-handover-checklist)
* [Key Files at a Glance](#key-files-at-a-glance)
* [Maintainer Notes](#maintainer-notes)

---

# How the Application Works

The data is refreshed daily using a Github Action Workflow (refresh_data.yml). When the data is refresh it is pushed/committed to this repo, this triggers the Posit Connect Cloud to republish the dashboard with the new refreshed data. 

*Every time a commit is made in the repo, the dashboard is automatically republished.*

The dashboard is structured using standard R Shiny files.

## `app.R`

This is the main application entry point.

It loads the primary application components:

```r
source("global.R")
source("ui.R")
source("server.R")
source("R/data_helpers.R")

shinyApp(ui, server)
```

When running the application, `app.R` should normally be the starting point.

---

## `global.R`

`global.R` loads:

* Required R packages
* Helper functions
* Cached datasets
* Static datasets
* Pre-processing required before the dashboard starts

> **The live Shiny application does not directly call external APIs or scrape websites when the dashboard starts.**

Instead, `global.R` reads previously downloaded data from:

```text
data/cache/
```

This improves reliability and performance.

If a data refresh fails, the dashboard can continue running using the most recent successful cache.

---

## `ui.R`

`ui.R` defines the visual layout of the dashboard.

This includes:

* Sidebar navigation
* Dashboard tabs
* Text and explanations
* Plot and map output
* Interactive filters
* Boxes and information panels
* Images
* Footer
* Styling and responsive design

## `server.R`

`server.R` contains the dashboard logic.

This includes:

* Plot creation
* Value boxes 
* Maps
* Data transformations used during the user session
* Navigation between sections
* Dynamic dashboard content

Use `server.R` when changing how the dashboard behaves or how visualizations are calculated.

---

## `data_refresh.R`

`data_refresh.R` is responsible for downloading and updating external datasets.

The script:

1. Connects to each external data source.
2. Downloads or scrapes the data.
3. Processes the data.
4. Writes the result into `data/cache/`.
5. Records whether the refresh was successful or unsuccessful in refresh_log.csv.

Each dataset is refreshed independently.

This is intentional. If one data source fails, the remaining datasets can still refresh.

The previous cached version of the failed dataset is retained.

This means the dashboard can continue using the **last successful version** of each dataset.

---

## `R/data_helpers.R`

This file contains reusable helper functions.

Examples include functions used for:

* Reading cached data
* Writing cached data safely
* Logging refresh attempts

If adding repeated data-processing logic, consider placing the logic in this file rather than duplicating it across multiple scripts.

---

# How the Data Refresh System Works

The project uses a cached data architecture.

The general workflow is:

```text
External Data Source
        ↓
data_refresh.R
        ↓
Data Processing/Cleaning
        ↓
data/cache/
        ↓
global.R
        ↓
Shiny Dashboard

```

The dashboard itself reads from the cache rather than directly from external websites.

This is important because:

* External websites may temporarily fail.
* APIs may have rate limits.
* Scraping can be slow.
* Data sources may change unexpectedly.
* The dashboard should still work if refresh fails.

---

## Refreshing Data Manually

To manually run the refresh process:

On Github select "Actions" -> "Scheduled Data Refresh" -> "Run workflow"

Refresh usually takes 2-4 minutes. Once the refresh is done the dashboard will be automatically republished on Connect Cloud.

---

## Scheduled Refreshes

The repository includes a GitHub Actions workflow that can be used to run the data refresh automatically.

The scheduled workflow is located in:

```text
.github/workflows/
```

Future staff should check the workflow if data stops refreshing.

Possible issues include:

* Changes to external APIs
* Changes to website HTML
* R package installation failures
* GitHub Actions environment changes

---

# Running the Dashboard Locally

## Requirements

Install:

* R
* RStudio (recommended)
* Git

Clone the repository:

```bash
git clone https://github.com/TOTA-git/TOTA-Waste-Water-Dashboard.git
```

Open the project in RStudio.

---

## Required R Packages

The project uses packages including:

```r
library(ggplot2)
library(readr)
library(dplyr)
library(shiny)
library(bslib)
library(shinydashboard)
library(fresh)
library(bcmaps)
library(sf)
library(leaflet)
library(httr)
library(jsonlite)
library(plotly)
library(tidyr)
library(stringr)
library(scales)

```

Install any missing packages before running the application.

---

## Start the Application

Run:

```r
shiny::runApp()
```

Or open:

```text
app.R
```

and run the application from RStudio.

---

# Secrets and variables for Sensitive URLs

This repository is public.

For that reason, confidential or private data URLs should **not** be written directly into the code.

These variables should be stored as secrets in the environment where the scheduled refresh runs.

---

# How to Update the Dashboard

## Updating Dashboard Text

Edit:

```text
ui.R
```

Most public-facing text is written directly in the user interface.

After editing:

1. Run the dashboard locally.
2. Check the layout on desktop.
3. Check the layout on smaller screens/mobile.
4. Confirm that links work.

---

## Updating Visualizations

Most visualization logic is located in:

```text
server.R
```

Data preparation that must occur before the dashboard starts is generally located in:

```text
global.R
```

When updating a chart:

1. Identify the data object used by the chart.
2. Identify where that data object is created.
3. Update the chart code.
4. Run the application locally.
5. Test different filter and selection options.

---

## Updating Data Sources

For automatically refreshed datasets, edit:

```text
data_refresh.R
```

A new data source should generally follow this pattern:

```r
refresh_dataset(
  "dataset_name",
  function() {
    # Fetch and process data
  },
  function(df) {
    # Save data to cache
  }
)
```

Then load the cached file in:

```text
global.R
```

This keeps the application architecture consistent.

---

# Adding New Data Sources

When adding a new dataset, follow these steps.

## Step 1: Identify the source

Document:

* Organization providing the data
* URL
* API or download method
* Update frequency
* License or usage restrictions
* Important field names

---

## Step 2: Add the refresh logic

Add the download or API process to:

```text
data_refresh.R
```

Use the existing `refresh_dataset()` structure when possible.

---

## Step 3: Save to the cache

Save the processed dataset into:

```text
data/cache/
```

Use the existing helper functions when possible.

---

## Step 4: Load the dataset

Add the cached dataset to:

```text
global.R
```

Example:

```r
df_new_data <- read_cache_csv("new_data")
```

---

## Step 5: Create the visualization

Add:

* UI components to `ui.R`
* Server logic to `server.R`

---

## Step 6: Test failure scenarios

Before relying on a new data source, test what happens when:

* The source is unavailable.
* The API returns an error.
* The dataset contains no rows.
* Column names change.

The dashboard should ideally continue working with the previous successful dataset.

---

# Troubleshooting

## The Dashboard Starts but Data Looks Old

Check:

```text
data/cache/refresh_log.csv
```

Look for recent failures.

Then:

1. Run `data_refresh.R` manually.
2. Check which dataset failed.
3. Visit the original data source.
4. Check for API or website changes.

---

## The Dashboard Will Not Start

Check:

* Missing R packages
* Missing environment variables
* Missing cached files
* Incorrect file paths
* Changes to required data columns

Run the application from RStudio and review the console error.

---

# Important Maintenance Notes

## Do Not Remove Cached Data Without a Backup

The cache allows the dashboard to continue functioning when live data sources fail.

Before clearing:

```text
data/cache/
```

create a backup.

---

## Keep Static and Dynamic Data Separate

Static reference data should remain in:

```text
data/
```

Automatically refreshed data should remain in:

```text
data/cache/
```

This separation is important for maintaining the project.

---

## Do Not Put Sensitive URLs in Public Code

Use:

* Environment variables
* GitHub Secrets
* Configuration files excluded through `.gitignore`

Never commit private spreadsheet URLs or credentials.

---

# Recommended Handover Checklist

When responsibility for this project is transferred to a new staff member, they should be provided with access to:

* [ ] The GitHub repository
* [ ] Posit Connect Cloud - Both Accounts
* [ ] The insto dashboard group email
* [ ] Current dashboard URL https://tota-waste-water-dashboard.share.connect.posit.cloud/
* [ ] Documentation for TOTA's INSTO reporting requirements

---

# Key Files at a Glance

| File                 | Purpose                                            |
| -------------------- | -------------------------------------------------- |
| `app.R`              | Starts the Shiny application                       |
| `global.R`           | Loads packages and datasets before the app starts  |
| `ui.R`               | Defines dashboard layout and public-facing content |
| `server.R`           | Contains interactive dashboard logic               |
| `data_refresh.R`     | Downloads and updates external datasets            |
| `R/data_helpers.R`   | Reusable data and cache helper functions           |
| `data/cache/`        | Automatically refreshed datasets                   |
| `data/`              | Static reference data                              |
| `www/`               | Images and other web assets                        |
| `.github/workflows/` | Automated scheduled refresh workflow               |

---

# Maintainer Notes

Avoid adding live API calls directly into `global.R`, `server.R`, or the user interface unless there is a strong reason to do so.

The goal is for the dashboard to remain reliable even when external data sources are temporarily unavailable.

---

