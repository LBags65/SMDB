## Libraries ---------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(tidyr)
library(shiny)
library(shinythemes)
library(data.table)
library(dplyr)
library(DT)
library(stringr)
library(shinylive)
library(httpuv)


## Setup in Database ------------------------------------------------------

#load in data
Website_DB <- read.csv("UI_Data", header=TRUE)

#renaming some columns for readability and setting as factor
Website_DB$Column <- factor(Website_DB$Column,
                            levels = c("CE-silica", "GC_5%_Diphenyl", "GC_PEG","LC_AEX", "LC_C18", "LC_HILIC", "NMR"),
                            labels = c("CE (silica)", "GC (5% Phenyl)", "GC (PEG)", "LC (AEX)", "LC (C18)", "LC (HILIC)", "NMR")
)

#make a few colnames look nicer
colnames(Website_DB)[colnames(Website_DB) == "Metabolite_Name"] <- "Metabolite"
colnames(Website_DB)[colnames(Website_DB) == "Identification_Method"] <- "Identification Method"
colnames(Website_DB)[colnames(Website_DB) == "Conf_level"] <- "Confidence Level"
colnames(Website_DB)[colnames(Website_DB) == "INCHIKEY"] <- "InChIKey"
colnames(Website_DB)[colnames(Website_DB) == "Extraction_Solvent"] <- "Extraction Solvent"
colnames(Website_DB)[colnames(Website_DB) == "Ionization_Source"] <- "Ionization Source"
colnames(Website_DB)[colnames(Website_DB) == "Ion_Mode"] <- "Ion Mode"
colnames(Website_DB)[colnames(Website_DB) == "Ion_Adducts"] <- "Adducts"
colnames(Website_DB)[colnames(Website_DB) == "Soil_Description"] <- "Soil Description"
colnames(Website_DB)[colnames(Website_DB) == "Site_description"] <- "Site Description"
colnames(Website_DB)[colnames(Website_DB) == "Solvent_Matrix"] <- "Extraction Matrix"

dt <- as.data.table(Website_DB)

dt[, InChIKey := toupper(str_trim(InChIKey))]
setkey(dt, InChIKey)

#API for pubchem structures
get_structure_url <- function(InChIKey) {
  if (is.null(InChIKey) || InChIKey == "") return(NULL)
  
  paste0(
    "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/InChIKey/",
    InChIKey,
    "/PNG"
  )
}


# UI -----
#setting columns to display in UI
display_cols <- c(
  "Metabolite",
  "Extraction Matrix", "Method", "Column", "Confidence Level",
  "Ionization Source","Ion Mode", "Adducts",
  "Soil Description","Site Description", "InChIKey","DOI"
)
{ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    
    tags$style(HTML("

      /* ================= BASE ================= */
      body {
        transform: scale(1.05);
        transform-origin: top left;
        width: 95%;
        font-size: 16px;
      }

      /* ================= INPUTS ================= */
      .form-control {
        font-size: 14px !important;
      }

      label {
        font-size: 14px !important;
        font-weight: 600;
      }

      /* ================= TABLE ================= */
      table.dataTable th,
      table.dataTable td {
        font-size: 14px !important;
        white-space: nowrap;
      }

      /* ================= LIPID DROPDOWN ================= */
      .selectize-dropdown {
        background: #ffffff !important;
        opacity: 1 !important;
        border: 1px solid #ccc !important;
        box-shadow: 0 6px 18px rgba(0,0,0,0.15) !important;
        z-index: 99999 !important;
      }

      .selectize-dropdown .option {
        background: #ffffff !important;
        color: #222 !important;
      }

      .selectize-dropdown .active {
        background: #1f3c88 !important;
        color: #ffffff !important;
      }

      .selectize-input {
        background: #ffffff !important;
        opacity: 1 !important;
      }

      /* ================= DETAILS ================= */
      .details-container {
        display: block !important;
        width: 100%;
        text-align: left !important;
        overflow: visible !important;
        max-height: none !important;
      }

      .details-row {
        display: block !important;
        padding: 4px 0;
        border-bottom: 1px solid #eee;
      }

      .details-key {
        font-weight: 600;
        display: block;
        margin-bottom: 2px;
      }

      .details-value {
        display: block;
        color: #333;
        word-break: break-word;
      }

      /* ================= TITLE ================= */
      .app-title {
        font-size: 30px !important;
        font-weight: 900;
      }

      /* ================= TOOLBAR ================= */
      .search-wrap {
        padding: 10px;
        background: #f7f9fc;
        border-bottom: 1px solid #ddd;
      }

      .toolbar {
        display: flex;
        justify-content: space-between;
        align-items: center;
        gap: 10px;
      }

    "))
  ),
  
  #HEADER 
  tags$div(
    style = "
      background: rgba(205, 133, 64, 0.5);
      color: #1a1a1a;
      padding: 14px;
      text-align: center;
      border-bottom: 1px solid #ddd;
    ",
    tags$div(class = "app-title", "Soil Metabolome Database")
  ),
  
  #SEARCH BAR 
  fluidRow(
    class = "search-wrap",
    
    column(
      12,
      
      div(
        class = "toolbar",
        
        div(
          style = "display:flex; gap:10px; align-items:center; flex-wrap:wrap;",
          
          radioButtons(
            "mode",
            NULL,
            choices = c("Metabolites" = "metabolites",
                        "Lipids" = "lipids"),
            selected = "metabolites",
            inline = TRUE
          ),
          
          conditionalPanel(
            condition = "input.mode == 'metabolites'",
            
            div(
              style = "display:flex; gap:8px;",
              
              textInput("InChIKey", NULL,
                        placeholder = "Single InChIKey",
                        width = "260px"),
              
              textAreaInput("batch_keys", NULL,
                            placeholder = "Batch (one per line)",
                            rows = 1,
                            width = "260px"),
              
              actionButton("search", "Search", class = "btn-primary")
            )
          ),
          
          conditionalPanel(
            condition = "input.mode == 'lipids'",
            
            div(
              style = "display:flex; gap:8px;",
              
              selectizeInput(
                "lipid_class",
                NULL,
                choices = sort(unique(dt$Lipid_Class)),
                multiple = TRUE,
                width = "420px"
              ),
              
              actionButton("search", "Search", class = "btn-primary")
            )
          )
        ),
        
        downloadButton("download_data", "Download CSV")
      )
    )
  ),
  
  #MAIN 
  fluidRow(
    
    column(
      12,
      
      DTOutput("results"),
      
      tags$hr(),
      
      fluidRow(
        column(3, uiOutput("structure_ui")),
        column(9, div(class = "details-container", uiOutput("details")))
      )
    )
  )
)}

# Server ----
server <- function(input, output, session) {
  
  filtered <- eventReactive(input$search, {
    
    if (input$mode == "metabolites") {
      
      keys <- c()
      
      if (!is.null(input$InChIKey) && input$InChIKey != "")
        keys <- c(keys, input$InChIKey)
      
      if (!is.null(input$batch_keys) && input$batch_keys != "") {
        batch <- unlist(strsplit(input$batch_keys, "\n"))
        keys <- c(keys, batch)
      }
      
      keys <- toupper(str_trim(keys))
      dt[InChIKey %in% keys]
      
    } else {
      
      res <- copy(dt)
      
      if (!is.null(input$lipid_class) &&
          length(input$lipid_class) > 0) {
        res <- res[Lipid_Class %in% input$lipid_class]
      }
      
      res
    }
  })
  
  output$results <- renderDT({
    req(filtered())
    
    datatable(
      filtered()[, ..display_cols],
      selection = "single",
      rownames = FALSE,
      class = "stripe hover",
      options = list(
        pageLength = 12,
        scrollX = TRUE
      )
    )
  })
  
  output$structure_ui <- renderUI({
    req(input$results_rows_selected)
    
    row <- filtered()[input$results_rows_selected]
    
    tags$img(
      src = get_structure_url(row$InChIKey[1]),
      style = "width:100%; max-height:180px; object-fit:contain;"
    )
  })
  
  output$details <- renderUI({
    req(input$results_rows_selected)
    
    row <- as.data.frame(filtered()[input$results_rows_selected])
    
    row[] <- lapply(row, function(x) {
      if (is.factor(x)) x <- as.character(x)
      x[is.na(x)] <- ""
      x
    })
    
    lapply(names(row), function(nm) {
      tags$div(
        class = "details-row",
        tags$div(class = "details-key", nm),
        tags$div(class = "details-value", row[[nm]][1])
      )
    })
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("soil_metabolome_", Sys.Date(), ".csv")
    },
    content = function(file) {
      fwrite(filtered(), file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)



