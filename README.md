# Runoff Reduction Analysis for Combined Sewer Overflow Prevention

## Project Overview

This repo contains GIS analysis inputs and outputs, identifying spots for Nature Based Solutions to reduce rural catchment runoff and prevent combined sewer overflows.

## Repository Structure

-   **Input/** - Contains input datasets for the R script including:

    -   Digital Surface Models (DSM) - from https://environment.data.gov.uk/survey
    -   River network layer - from https://www.data.gov.uk/dataset/dc29160b-b163-4c6e-8817-f313229bcc23/os-open-rivers1
    -   Sewer Network Layer

-   **[Output]/** - Contains processed results from the R script:

    -   Generated flow accumulation map
    -   Flow direction output
    -   Flow distance output

-   **ProjectE_script.R** - The main R script that performs hydrological modelling

-   **TWI** - The Topograhic Wetness Index - Credit https://www.youtube.com/watch?v=Tl5heKj4FFU

## How We Used GitHub

GitHub is a website that helps us collaborate on this project. It's basically a shared folder system with version control, which means:

-   We could all work on the same script without overwriting each other's work
-   Every change was tracked so we could go back to earlier versions if needed
-   We could leave comments on specific bits, if needed
-   All our maps and analysis results stayed organised in one place

It is really helpful for keeping track of who did what and making sure we were all working with the latest versions.
