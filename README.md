# SMDB

This project contains the Soil Metabolome Database and the R code used to generate the User Interface and data analysis for the publication

## Structure
[README.md](README.md): Readme file with instructions on how to generate the UI for the SMDB

<ins>UI_Data</ins>: This file contains the SMDB data in the combined metadata and metabolite data format used to make the UI

<ins>App:</ins> Contains the R code used to generate the UI

## Accessing the SMDB

There are two ways to access the SMDB:

1. A UI run locally in R through a Shiny App. See the following section for instructions on how to generate the UI.
2. The SMDB may also be downloaded in Excel format and searched manually. The SMDB is available for download at doi.org/10.5683/SP4/OTBYI2 

## Running the SMDB UI

The SMDB UI was made through a Shiny App in R 4.4.2. The following sections describe how to setup the Shiny App locally in RStudio. All code is availabe in ()

