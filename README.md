# Runoff Reduction Analysis for Combined Sewer Overflow Prevention

## Project Overview

This repo contains GIS analysis inputs and outputs, identifying spots for Nature Based Solutions to reduce rural catchment runoff and prevent combined sewer overflows.

## Repository Structure

-   **Input/** - Contains input datasets for the R script including:

    -   Digital Surface Models (DSM) - from https://environment.data.gov.uk/survey
    -   River network layer - from https://www.data.gov.uk/dataset/dc29160b-b163-4c6e-8817-f313229bcc23/os-open-rivers1
    -   Sewer Network Layer

-   **Output/** - Contains processed results from the R script:

    -   Generated flow accumulation map
    -   Flow direction output
    -   Topographic Wetness Index (TWI)
    -   Flow distance output

-   **ProjectE_script.R** - The main R script that performs hydrological modelling

-   **Screenshots/** - Visual outputs from the analysis (described below)

## Analysis Screenshots

### Hydrological Analysis Results

1. **Flow Accumulation**  
   Map showing where water builds up across the landscape.

2. **Flow Direction + Flow Distance**  
   Analysis of water flow paths with red circles marking suggested intervention sites.

3. **Flow Direction + Flow Distance (Another View)**  
   Same analysis from a different angle, highlighting potential intervention points.

4. **Flow Direction**  
   Visualisation showing which way water flows over the terrain.

5. **TWI + Flow Distance Intervention Areas**  
   Topographic Wetness Index combined with flow analysis. The grey areas show where we think interventions would work best.

6. **TWI + Flow Distance Intervention Areas (Another Angle)**  
   Another view of our proposed intervention areas.

7. **Topographic Wetness Index (TWI)**  
   Broad view of the TWI showing naturally wet areas where water tends to collect.

## How We Used GitHub

GitHub is a website that helps us collaborate on this project. It's basically a shared folder system with version control, which means:

-   We could all work on the same script without overwriting each other's work
-   Every change was tracked so we could go back to earlier versions if needed
-   We could leave comments on specific bits, if needed
-   All our maps and analysis results stayed organised in one place

It is really helpful for keeping track of who did what and making sure we were all working with the latest versions.
