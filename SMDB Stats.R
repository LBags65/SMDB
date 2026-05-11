# Load Data ---------------------------------------------------------------
## Libraries ---------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(leaflet)
library(readxl)
library(ggbreak)
library(rcrossref)
library(ggtext)
library(RColorBrewer)
library(stringr)
library(data.table)
library(ggpattern)
library(ggrepel)
library(rstatix)
library(ggpubr)
library(forcats)
library(shiny)
library(DT)
library(stringr)
library(bslib)
library(clipr)
library(writexl)
library(shinythemes)
library(bs4Dash)
## Setup in Database ------------------------------------------------------

SMDB_Metadata <- read_excel("C:/Users/lbagd/OneDrive - University of Toronto/Soil Metabolome Database/Database Documents/Soil Metabolome Database.xlsx", 
                                       sheet = "1. Metadata")



#Load in metabolites
SMDB_Locations <- read_excel("C:/Users/lbagd/OneDrive - University of Toronto/Soil Metabolome Database/Database Documents/Soil Metabolome Database.xlsx", 
  sheet = "2. Location Data")

#Load in Locations
Soil_Metabolome_Database <- read_excel("C:/Users/lbagd/OneDrive - University of Toronto/Soil Metabolome Database/Database Documents/Soil Metabolome Database.xlsx", 
                                            sheet = "3. Metabolites", col_types = c("text", 
                                                                                           "text", "numeric", "numeric", "numeric", 
                                                                                           "numeric", "text", "text", "text", 
                                                                                           "text", "numeric", "numeric", "numeric", 
                                                                                           "text", "numeric", "numeric", "numeric", 
                                                                                           "numeric", "numeric", "numeric", 
                                                                                           "numeric", "text", "text", "numeric", 
                                                                                           "text", "text", "text", "text", "text", 
                                                                                           "text", "text", "text", "text", "text", 
                                                                                           "text", "text", "numeric", "numeric", 
                                                                                           "numeric", "numeric", "numeric", 
                                                                                           "numeric", "numeric", "numeric", 
                                                                                           "numeric", "numeric", "numeric", 
                                                                                           "numeric", "numeric", "text", "text", 
                                                                                           "text", "text", "text", "text", "text", 
                                                                                           "text", "text", "text", "text", "text", 
                                                                                           "text", "text", "text", "text", "text", 
                                                                                           "text", "text", "text"))

#Set factors
Soil_Metabolome_Database$Identification_Method <- as.factor(Soil_Metabolome_Database$Identification_Method) #MS or NMR
Soil_Metabolome_Database$Metabolite_Lipid <- as.factor(Soil_Metabolome_Database$Metabolite_Lipid) #Metabolite or Lipid
Soil_Metabolome_Database$Conf_level <- as.factor(Soil_Metabolome_Database$Conf_level) #1 or 2

Soil_Metabolome_Database$Solvent_Matrix[Soil_Metabolome_Database$Solvent_Matrix == "NA"] <- NA
Soil_Metabolome_Database$Solvent_Matrix <- as.factor(Soil_Metabolome_Database$Solvent_Matrix) #simplified solvents

Soil_Metabolome_Database$Method <- as.factor(Soil_Metabolome_Database$Method) 
Soil_Metabolome_Database$Column <- as.factor(Soil_Metabolome_Database$Column) 
Soil_Metabolome_Database$Ionization_Source <- as.factor(Soil_Metabolome_Database$Ionization_Source) 
Soil_Metabolome_Database$Ion_Mode <- as.factor(Soil_Metabolome_Database$Ion_Mode) 

#Check factors here
table(Soil_Metabolome_Database$Identification_Method)

#Clean up element counts
Element_cols <- c("C", "H", "As", "B", "Br","Cl", "F", "I", "N", "O","P", "S", "Si")
Soil_Metabolome_Database[Element_cols][Soil_Metabolome_Database[Element_cols] == 0] <- NA

#Setting up chemical classifications

#Superclass 
{
table(Soil_Metabolome_Database$Superclass)

Soil_Metabolome_Database$Superclass <- as.factor(Soil_Metabolome_Database$Superclass) #Set as factor

#set superclass colors
Superclass_colors <- {c(
  "Alkaloids and derivatives" = "firebrick3",
  "Benzenoids" = "#7570b3",
  "Homogeneous non-metal compounds" = "steelblue4",
  "Hydrocarbon derivatives" = "khaki3",
  "Hydrocarbons" = "khaki2",
  "Lignans, neolignans and related compounds" = "aquamarine4",
  "Lipids and lipid-like molecules" = "salmon4",
  "Mixed metal/non-metal compounds"= "grey30",
  "Nucleosides, nucleotides, and analogues" = "maroon",
  "Organic 1,3-dipolar compounds" = "lavender",
  "Organic acids and derivatives" = "darkseagreen",
  "Organic nitrogen compounds" = "coral3",
  "Organic oxygen compounds" = "#A8DADC",
  "Organohalogen compounds" = "purple3",
  "Organoheterocyclic compounds" = "tan2",
  "Organometallic compounds" = "steelblue1",
  "Organophosphorus compounds" = "red",
  "Organosulfur compounds" = "dodgerblue3",
  "Phenylpropanoids and polyketides" = "lightpink2",
  "Other" = "grey30"
  )}
}

# Graphs and Tables -------------------------------------------------------
## Totals ------------------------------------------------------------------
table(Soil_Metabolome_Database$Metabolite_Lipid) #total metabolites and lipids

unique_metabolites <- filter(Soil_Metabolome_Database, Metabolite_Lipid == "Metabolite") %>%
  distinct(INCHIKEY, .keep_all = TRUE) #unique metabolites

table(unique_metabolites$Superclass)

## Table and Graph of Superclass ------------------------------------------------------

#58 studies reported metabolites
Metabolite_DOIs <- Soil_Metabolome_Database %>%
  filter(Metabolite_Lipid == "Metabolite") %>%
  filter(!is.na(Superclass)) %>%
  distinct(DOI)

#13 studies reported lipids
Lipid_DOIs <- Soil_Metabolome_Database %>%
  filter(Metabolite_Lipid == "Lipid") %>%
  distinct(DOI)

#Distinct metabolite superclass counts
Superclass_Distinct <- Soil_Metabolome_Database %>%
  filter(Metabolite_Lipid == "Metabolite") %>%
  filter(!is.na(Superclass)) %>%
  filter(Superclass != "Unclassified") %>%
  distinct(INCHIKEY, .keep_all = TRUE) %>%
  group_by(Superclass) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(
    Percent = Count / sum(Count) * 100,
    Superclass = if_else(Percent < 1, "Other", Superclass)
  ) %>%
  group_by(Superclass) %>%
  summarise(
    Count = sum(Count),
    Percent = sum(Percent),
    .groups = "drop"
  ) %>%
  arrange(desc(Percent)) %>%
  mutate(
    Superclass = factor(Superclass, levels = Superclass)
  )

sum(Superclass_Distinct$Count)

Superclass_Distinct_total <- sum(Superclass_Distinct$Count)
Superclass_Distinct_subtitle <- "<span style='font-size:20pt; font-weight:normal; font-style:italic'>(n = 58, m = 4290)</span>"

#400 x 400
ggplot(Superclass_Distinct, aes(x = "", y = Percent, fill = Superclass)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = Superclass_colors) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Superclass",
    title = "Unique Metabolites",
    subtitle = Superclass_Distinct_subtitle
  ) +
  theme_void() +
  guides(
    fill = guide_legend(nrow = 5)
  ) +
  theme(
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 12),
    plot.subtitle = element_markdown(size = 12, hjust = 0.5, face = "bold"),
    legend.position = "none",
    plot.title = element_text(size = 20, hjust = 0.5 ,face = "bold")
  ) 

#All metabolite entries superclass
Superclass_All <- Soil_Metabolome_Database %>%
  filter(Metabolite_Lipid == "Metabolite") %>%
  filter(!is.na(Superclass)) %>%
  group_by(Superclass) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(
    Percent = Count / sum(Count) * 100,
    Superclass = if_else(Percent < 1, "Other", Superclass)
  ) %>%
  group_by(Superclass) %>%
  summarise(
    Count = sum(Count),
    Percent = sum(Percent),
    .groups = "drop"
  ) %>%
  arrange(desc(Percent)) %>%
  mutate(
    Superclass = factor(Superclass, levels = Superclass)
  )

sum(Superclass_All$Count)

Superclass_All_total <- sum(Superclass_All$Count)
Superclass_All_subtitle <- "<span style='font-size:20pt; font-weight:normal; font-style:italic'>(n=58, m = 7681)</span>"

#400x400
ggplot(Superclass_All, aes(x = "", y = Percent, fill = Superclass)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = Superclass_colors) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Superclass",
    title = "All Metabolites",
    subtitle = Superclass_All_subtitle
  ) +
  theme_void() +
  theme(
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 12),
    plot.subtitle = element_markdown(size = 12, hjust = 0.5, face = "bold"),
   # legend.position = "none",
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold")
  ) 



## Tables of Class and Subclass ------------------------------------------------------
Class <- Soil_Metabolome_Database %>%
  filter(Metabolite_Lipid == "Metabolite") %>%
  distinct(INCHIKEY, .keep_all = TRUE) %>%
  filter(!is.na(Class)) %>%
  group_by(Class) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  arrange(desc(Count)) %>%
  mutate(
    Percent = Count / sum(Count) * 100,
    Class = factor(Class, levels = Class) 
  )

subclass <- Soil_Metabolome_Database %>%
  filter(Metabolite_Lipid == "Metabolite") %>%
  distinct(INCHIKEY, .keep_all = TRUE) %>%
  filter(!is.na(subclass)) %>%
  group_by(subclass) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  arrange(desc(Count)) %>%
  mutate(
    Percent = Count / sum(Count) * 100,
    subclass = factor(subclass, levels = subclass) # keep bars ordered largest -> smallest
  )



## Table and Graph of Lipids -----------------------------------------------
Lipids <- Soil_Metabolome_Database %>%
  filter(Metabolite_Lipid == "Lipid") %>%
  filter(!is.na(Lipid_Class)) %>%
  group_by(Lipid_Class) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  arrange(desc(Count)) %>%
  mutate(
    Percent = Count / sum(Count) * 100,
    Lipid_Class = factor(Lipid_Class, levels = Lipid_Class) 
  )

sum(Lipids$Count)

LperDOI <- Soil_Metabolome_Database %>%
  filter(Metabolite_Lipid == "Lipid")
  group_by(DOI) 

table(LperDOI$DOI)

Lipids_Main <- Lipids %>%
  filter(Percent >= 1) 

sum(Lipids_Main$Count)

ggplot(Lipids_Main, aes(x = "", y = Percent, fill = Lipid_Class)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  labs(
    x = NULL,
    y = NULL,
    fill = "Lipid Class",
    title = "Major Lipid Classes"
  ) + 
  scale_fill_manual(values = c(
    "Ceramide" = alpha("orchid4", 0.6),
    "DGTS/DGTA" = alpha("chocolate4", 0.6),
    "Diacylglycerol" = alpha("hotpink", 0.6),
    "Phosphatidylcholine" = alpha("lemonchiffon3", 0.6),
    "Phosphatidylethanolamine" = alpha("brown", 0.6),
    "Phosphatidylglycerol" = alpha("salmon3", 0.6),
    "Phosphatidylinositol" = alpha("dodgerblue3", 0.6),
    "Triacylglycerol" = alpha("springgreen4", 0.6)
  )) +
  theme_void() +
  theme(
    legend.title = element_text(size = 15, face = "bold"),
    legend.text  = element_text(size = 12),
    #legend.position = "none",
    plot.title = element_text(size = 20, hjust = 0.5, vjust = -4,face = "bold")
  ) 


## Confidences -------------------------------------------------------------
Confidences <- Soil_Metabolome_Database %>%
  filter(Metabolite_Lipid == "Metabolite") %>% # just metabolites
  filter(!is.na(Conf_level)) %>%
  filter(!is.na(Superclass))

table(Confidences$Conf_level)

Confidences_Counts <- Confidences %>% #view commonly used matrices (get numbers for graph)
  group_by(Conf_level) %>%
  summarise(n_DOIs = n_distinct(DOI), .groups = "drop") %>%
  arrange(desc(n_DOIs))


Conf_Superclass <- Confidences %>%
  filter(!is.na(Superclass)) %>%
  group_by(Conf_level, Superclass) %>%
  summarise(Count = n(), .groups = "drop_last") %>%
  group_by(Conf_level) %>%
  mutate(Percent = Count / sum(Count) * 100) %>% 
  ungroup() %>%
  mutate(Superclass = as.character(Superclass)) %>%
  group_by(Conf_level) %>%
  mutate(
    Superclass = if_else(Percent < 1, "Other", Superclass)
  ) %>%
  ungroup() %>%
  group_by(Conf_level, Superclass) %>%
  summarise(
    Percent = sum(Percent),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  group_by(Conf_level) %>%
  arrange(Conf_level, desc(Percent)) %>%
  mutate(Superclass = factor(Superclass, levels = unique(Superclass))) %>%
  ungroup() 


ggplot(Conf_Superclass, aes(x = "", y = Percent, fill = Superclass)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  facet_wrap(~Conf_level,
             labeller = labeller(
               Conf_level = c(
                 "1" = "Level 1 IDs\n(n = 17, m = 1549)",
                 "2" = "Level 2 IDs\n(n = 46, m = 6025)"))) +
  scale_fill_manual(values = Superclass_colors) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Superclass"
  ) +
  theme_void() +
  guides(
    fill = guide_legend(nrow = 5)
  ) +
  theme(
    legend.title = element_text(size = 18, face = "bold"),
    legend.text  = element_text(size = 12),
    strip.text = element_text(size = 18, face = "bold", vjust = 1),
    legend.position = "none",
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"
    )
  )

Conf_subclass <- Confidences %>%
  filter(!is.na(subclass)) %>%
  group_by(Conf_level, subclass) %>%
  summarise(Count = n(), .groups = "drop_last") %>%
  group_by(Conf_level) %>%
  mutate(Percent = Count / sum(Count) * 100) %>% 
  ungroup() %>%
  mutate(subclass = as.character(subclass)) %>%
  group_by(Conf_level, subclass) %>%
  summarise(
    Percent = sum(Percent),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  group_by(Conf_level) %>%
  arrange(Conf_level, desc(Percent)) %>%
  mutate(subclass = factor(subclass, levels = unique(subclass))) %>%
  ungroup() 


## Extraction Conditions ---------------------------------------------------
Extractions <- Soil_Metabolome_Database %>%
  filter(Metabolite_Lipid == "Metabolite") %>% # just metabolites
  filter(!(is.na(Superclass))) %>%
  filter(!is.na(Solvent_Matrix)) %>%
  filter(Solvent_Matrix != "NA")

table(Extractions$Solvent_Matrix) # get metabolite numbers from here

Extractions_Counts <- Extractions %>% #view commonly used matrices (get numbers for graph)
  group_by(Solvent_Matrix) %>%
  summarise(n_DOIs = n_distinct(DOI), .groups = "drop") %>%
  arrange(desc(n_DOIs))

Extractions_Superclass <- Extractions %>%
  filter(Solvent_Matrix %in% c("MeOH/Water", "Water", "MeOH/IPA/Water", 
                               "Biphasic (Aqueous)", "MeOH", "Headspace")) %>%
  filter(!is.na(Superclass)) %>%
  group_by(Solvent_Matrix, Superclass) %>%
  summarise(Count = n(), .groups = "drop_last") %>%
  group_by(Solvent_Matrix) %>%
  mutate(Percent = Count / sum(Count) * 100) %>% 
  ungroup() %>%
  mutate(Superclass = as.character(Superclass)) %>%
  group_by(Solvent_Matrix) %>%
  mutate(
    Superclass = if_else(Percent < 1, "Other", Superclass)
  ) %>%
  ungroup() %>%
  group_by(Solvent_Matrix, Superclass) %>%
  summarise(
    Percent = sum(Percent),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  group_by(Solvent_Matrix) %>%
  arrange(Solvent_Matrix, desc(Percent)) %>%
  mutate(Superclass = factor(Superclass, levels = unique(Superclass))) %>%
  ungroup() 

Extractions_Superclass$Solvent_Matrix <- factor(
  Extractions_Superclass$Solvent_Matrix,
  levels = c(
    "MeOH/Water",
    "Water",
    "MeOH/IPA/Water",
    "Biphasic (Aqueous)",
    "MeOH",
    "Headspace"
  )
)


ggplot(Extractions_Superclass, aes(x = "", y = Percent, fill = Superclass)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  facet_wrap(~Solvent_Matrix,
             labeller = labeller(
               Solvent_Matrix = c(
                 "MeOH/Water" = "MeOH/Water<br><span style='font-size:12pt; font-weight:normal; font-style:italic'>(n = 19, m = 2747)</span>",
                 "Water" = "Water<br><span style='font-size:12pt; font-weight:normal; font-style:italic'>(n = 15, m = 1854)</span>",
                 "MeOH/IPA/Water" = "MeOH/IPA/Water<br><span style='font-size:12pt; font-weight:normal; font-style:italic'>(n = 5, m = 690)</span>",
                 "Biphasic (Aqueous)" = "Biphasic (Aqueous)<br><span style='font-size:12pt; font-weight:normal; font-style:italic'>(n = 3, m = 237)</span>",
                 "MeOH" = "MeOH<br><span style='font-size:12pt; font-weight:normal; font-style:italic'>(n = 3, m = 226)</span>",
                 "Headspace" = "Headspace<br><span style='font-size:12pt; font-weight:normal; font-style:italic'>(n = 2, m = 278)</span>"
               )
             )
  ) +
  scale_fill_manual(values = Superclass_colors) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Superclass"
  ) +
  guides(
    fill = guide_legend(nrow = 3)
  ) +
  theme_void() +
  theme(
    legend.title = element_text(size = 18, face = "bold"),
    legend.text  = element_text(size = 12),
    #legend.position = "none",
    strip.text = ggtext::element_markdown(size = 16, face = "bold"),
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold")
  )

Extractions_Superclass_Top_3 <- Extractions %>%
  filter(Solvent_Matrix %in% c("MeOH/Water", "Water", "MeOH/IPA/Water")) %>%
  filter(!is.na(Superclass)) %>%
  group_by(Solvent_Matrix, Superclass) %>%
  summarise(Count = n(), .groups = "drop_last") %>%
  group_by(Solvent_Matrix) %>%
  mutate(Percent = Count / sum(Count) * 100) %>% 
  ungroup() %>%
  mutate(Superclass = as.character(Superclass)) %>%
  group_by(Solvent_Matrix) %>%
  mutate(
    Superclass = if_else(Percent < 1, "Other", Superclass)
  ) %>%
  ungroup() %>%
  mutate(Solvent_Matrix = factor(Solvent_Matrix,
                                 levels = c("MeOH/Water", "Water", "MeOH/IPA/Water"))) %>%
  group_by(Solvent_Matrix, Superclass) %>%
  summarise(
    Percent = sum(Percent),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  group_by(Solvent_Matrix) %>%
  arrange(Solvent_Matrix, desc(Percent)) %>%
  mutate(Superclass = factor(Superclass, levels = unique(Superclass))) %>%
  ungroup()

Extractions_Superclass_Top_3$Solvent_Matrix <- factor(
  Extractions_Superclass_Top_3$Solvent_Matrix,
  levels = c(
    "MeOH/Water",
    "Water",
    "MeOH/IPA/Water"))

#600x300
ggplot(Extractions_Superclass_Top_3, aes(x = "", y = Percent, fill = Superclass)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  facet_wrap(~Solvent_Matrix,
             labeller = labeller(
               Solvent_Matrix = c(
                 "MeOH/Water" = "MeOH/Water<br><span style='font-size:12pt; font-weight:normal; font-style:italic'>(n = 19, m = 2747)</span>",
                 "Water" = "Water<br><span style='font-size:12pt; font-weight:normal; font-style:italic'>(n = 15, m = 1854)</span>",
                 "MeOH/IPA/Water" = "MeOH/IPA/Water<br><span style='font-size:12pt; font-weight:normal; font-style:italic'>(n = 5, m = 690)</span>"
               )
             )
  ) +
  scale_fill_manual(values = Superclass_colors) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Superclass"
  ) +
  theme_void() +
  theme(
    legend.title = element_text(size = 18, face = "bold"),
    legend.text  = element_text(size = 12),
    #legend.position = "none",
    strip.text = ggtext::element_markdown(size = 16, face = "bold"),
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold")
  )


labels_named <- c(
  "MeOH/Water" = "MeOH/Water<br><span style='font-size:10pt; font-style:italic'>(n = 19, m = 2747)</span>",
  "Water" = "Water<br><span style='font-size:10pt; font-style:italic'>(n = 15, m = 1865)</span>",
  "MeOH/IPA/Water" = "MeOH/IPA/Water<br><span style='font-size:10pt; font-style:italic'>(n = 5, m = 690)</span>",
  "Biphasic (Aqueous)" = "Biphasic (Aqueous)<br><span style='font-size:10pt; font-style:italic'>(n = 3, m = 237)</span>",
  "MeOH" = "MeOH<br><span style='font-size:10pt; font-style:italic'>(n = 3, m = 226)</span>",
  "Headspace" = "Headspace<br><span style='font-size:10pt; font-style:italic'>(n = 2, m = 278)</span>"
)

ggplot(Extractions_Superclass, aes(x = Solvent_Matrix, y = Percent, fill = Superclass)) +
  geom_col(width = 0.7) +
  scale_x_discrete(labels = labels_named) +
  scale_fill_manual(values = Superclass_colors) +
  labs(
    x = NULL,
    y = "Percent",
    fill = "Superclass"
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.title = element_text(size = 18, face = "bold"),
    legend.text  = element_text(size = 12),
    legend.position = "none",
    axis.text.x = ggtext::element_markdown(size = 14, face = "bold"),
    axis.text.y = element_text(size = 12)
  )


## MS ----------------------------------------------------------------------
MS <- Soil_Metabolome_Database %>%
  filter(Metabolite_Lipid == "Metabolite") %>% # just metabolites
  filter(!is.na(Method)) %>%
  filter(Method != "NA") %>%
  filter(!is.na(Superclass)) %>%
  mutate(Method = if_else(Method == "LC/IMS/MS", "LC/MS", Method)) #grouping LC/IMS/MS with LC/MS

table(MS$Method) # get metabolite numbers from here

MS_Counts <- MS %>% #view commonly used matrices (get numbers for graph)
  group_by(Method) %>%
  summarise(n_DOIs = n_distinct(DOI), .groups = "drop") %>%
  arrange(desc(n_DOIs))

MS_Superclass <- MS %>%
  filter(Method %in% c("LC/MS", "GC/MS", "NMR")) %>%
  filter(!is.na(Superclass)) %>%
  group_by(Method, Superclass) %>%
  summarise(Count = n(), .groups = "drop_last") %>%
  group_by(Method) %>%
  mutate(Percent = Count / sum(Count) * 100) %>% 
  ungroup() %>%
  mutate(Superclass = as.character(Superclass)) %>%
  group_by(Method) %>%
  mutate(
    Superclass = if_else(Percent < 1, "Other", Superclass)
  ) %>%
  ungroup() %>%
  group_by(Method, Superclass) %>%
  summarise(
    Percent = sum(Percent),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  group_by(Method) %>%
  arrange(Method, desc(Percent)) %>%
  mutate(Superclass = factor(Superclass, levels = unique(Superclass))) %>%
  ungroup() 

MS_Superclass$Method <- factor(
  MS_Superclass$Method,
  levels = c(
    "LC/MS",
    "GC/MS",
    "NMR"))

ggplot(MS_Superclass, aes(x = "", y = Percent, fill = Superclass)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  facet_wrap(~Method,
             labeller = labeller(
               Method = c(
                 "LC/MS" = "LC/MS<br><span style='font-size:12pt; font-weight:normal; font-style:italic'>(n = 34, m = 4864)</span>",
                 "GC/MS" = "GC/MS<br><span style='font-size:12pt; font-weight:normal; font-style:italic'>(n = 26, m = 2643)</span>",
                 "NMR" = "NMR<br><span style='font-size:12pt; font-weight:normal; font-style:italic'>(n = 4, m = 107)</span>")
             )
  ) +
  scale_fill_manual(values = Superclass_colors) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Superclass"
  ) +
  theme_void() +
  guides(
    fill = guide_legend(nrow = 5)
  ) +
  theme(
    legend.title = element_text(size = 18, face = "bold"),
    legend.text  = element_text(size = 12),
    legend.position = "none",
    strip.text = ggtext::element_markdown(size = 16, face = "bold"),
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold")
  )

## MS/Column ---------------------------------------------------------------
MS_Column <- Soil_Metabolome_Database %>%
  filter(Metabolite_Lipid == "Metabolite") %>% # just metabolites
  filter(!(is.na(Superclass))) %>%
  filter(!is.na(Column)) %>%
  filter(Column != "NA")


table(MS_Column$Column) # get metabolite numbers from here

MS_Column_Counts <- MS_Column %>% #view commonly used matrices (get numbers for graph)
  group_by(Column) %>%
  summarise(n_DOIs = n_distinct(DOI), .groups = "drop") %>%
  arrange(desc(n_DOIs))

MS_Column_Superclass <- MS_Column %>%
  filter(Column %in% c("GC_5%_Diphenyl", "LC_C18", "LC_HILIC")) %>% #choose major ones
  filter(!is.na(Superclass)) %>%
  group_by(Column, Superclass) %>%
  summarise(Count = n(), .groups = "drop_last") %>%
  group_by(Column) %>%
  mutate(Percent = Count / sum(Count) * 100) %>% 
  ungroup() %>%
  mutate(Superclass = as.character(Superclass)) %>%
  group_by(Column) %>%
  mutate(
    Superclass = if_else(Percent < 1, "Other", Superclass)
  ) %>%
  ungroup() %>%
  group_by(Column, Superclass) %>%
  summarise(
    Percent = sum(Percent),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  group_by(Column) %>%
  arrange(Column, desc(Percent)) %>%
  mutate(Superclass = factor(Superclass, levels = unique(Superclass))) %>%
  ungroup() 

MS_Column_Superclass$Column <- factor(
  MS_Column_Superclass$Column,
  levels = c(
    "GC_5%_Diphenyl",
    "LC_C18",
    "LC_HILIC"
  )
)

#600x300
ggplot(MS_Column_Superclass, aes(x = "", y = Percent, fill = Superclass)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  facet_wrap(~Column,
             labeller = labeller(
               Column = c(
                 "GC_5%_Diphenyl" = "GC (5% Phenyl)<br><span style='font-size:12pt; font-weight:normal; font-style:italic'>(n = 21, m = 2062)</span>",
                 "LC_C18" = "LC (C18)<br><span style='font-size:12pt; font-weight:normal; font-style:italic'>(n = 24, m = 3425)</span>",
                 "LC_HILIC" = "LC (HILIC)<br><span style='font-size:12pt; font-weight:normal; font-style:italic'>(n = 12, m = 1366)</span>"
               )
             )
  ) +
  scale_fill_manual(values = Superclass_colors) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Superclass"
  ) +
  theme_void() +
 # guides(
    #fill = guide_legend(nrow = 6)
  #) +
  theme(
    legend.title = element_text(size = 18, face = "bold"),
    legend.text  = element_text(size = 12),
    legend.position = "none",
    strip.text = ggtext::element_markdown(size = 16, face = "bold"),
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold")
  )

MS_Column_Class <- MS_Column %>%
  filter(Column %in% c("GC_5%_Diphenyl", "LC_C18", "LC_HILIC")) %>% #choose major ones
  filter(!is.na(Class)) %>%
  group_by(Column, Class) %>%
  summarise(Count = n(), .groups = "drop_last") %>%
  group_by(Column) %>%
  mutate(Percent = Count / sum(Count) * 100) %>% 
  ungroup() %>%
  mutate(Class = as.character(Class)) %>%
  group_by(Column) %>%
  mutate(
    Class = if_else(Percent < 1, "Other", Class)
  ) %>%
  ungroup() %>%
  group_by(Column, Class) %>%
  summarise(
    Percent = sum(Percent),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  group_by(Column) %>%
  arrange(Column, desc(Percent)) %>%
  mutate(Class = factor(Class, levels = unique(Class))) %>%
  ungroup() 

MS_Column_subclass <- MS_Column %>%
  filter(Column %in% c("GC_5%_Diphenyl", "LC_C18", "LC_HILIC")) %>% #choose major ones
  filter(!is.na(subclass)) %>%
  group_by(Column, subclass) %>%
  summarise(Count = n(), .groups = "drop_last") %>%
  group_by(Column) %>%
  mutate(Percent = Count / sum(Count) * 100) %>% 
  ungroup() %>%
  mutate(subclass = as.character(subclass)) %>%
  group_by(Column) %>%
  mutate(
    subclass = if_else(Percent < 1, "Other", subclass)
  ) %>%
  ungroup() %>%
  group_by(Column, subclass) %>%
  summarise(
    Percent = sum(Percent),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  group_by(Column) %>%
  arrange(Column, desc(Percent)) %>%
  mutate(subclass = factor(subclass, levels = unique(subclass))) %>%
  ungroup() 

## Ion Mode -------------------------------------------------------------
Ion_Mode <- Soil_Metabolome_Database %>%
  filter(Metabolite_Lipid == "Metabolite") %>% # just metabolites
  filter(!is.na(Ion_Mode)) %>%
  filter(!is.na(Superclass)) %>% #only classified compounds
  filter(Ion_Mode != "+/-") 

table(Ion_Mode$Ion_Mode)

Ion_Mode_Superclass <- Ion_Mode %>%
  group_by(Ion_Mode, Superclass) %>%
  summarise(Count = n(), .groups = "drop_last") %>%
  group_by(Ion_Mode) %>%
  mutate(Percent = Count / sum(Count) * 100) %>% 
  ungroup() %>%
  mutate(Superclass = as.character(Superclass)) %>%
  group_by(Ion_Mode) %>%
  mutate(
    Superclass = if_else(Percent < 1, "Other", Superclass)
  ) %>%
  ungroup() %>%
  group_by(Ion_Mode, Superclass) %>%
  summarise(
    Percent = sum(Percent),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  group_by(Ion_Mode) %>%
  arrange(Ion_Mode, desc(Percent)) %>%
  mutate(Superclass = factor(Superclass, levels = unique(Superclass))) %>%
  ungroup() 

Ion_Mode_Count <- Soil_Metabolome_Database %>%
  filter(Metabolite_Lipid == "Metabolite") %>% # just metabolites
  filter(!is.na(Ion_Mode)) %>%
  filter(Ion_Mode != "+/-") %>%
  distinct(Ion_Mode, DOI) %>%
  group_by(Ion_Mode) %>%
  summarise(Count = n(), .groups = "drop_last") #gives # studies that used each ion mode
  

ggplot(Ion_Mode_Superclass, aes(x = "", y = Percent, fill = Superclass)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  facet_wrap(~Ion_Mode,
             labeller = labeller(
               Ion_Mode = c(
                 "-" = "Negative Mode<br><span style='font-size:12pt; font-weight:normal; font-style:italic'>(n = 20, m = 1344)</span>",
                 "+" = "Positive Mode<br><span style='font-size:12pt; font-weight:normal; font-style:italic'>(n = 38, m = 3605)</span>"))) +
  scale_fill_manual(values = Superclass_colors) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Superclass"
  ) +
  theme_void() +
  theme(
    legend.title = element_text(size = 18, face = "bold"),
    legend.text  = element_text(size = 12),
    strip.text = ggtext::element_markdown(size = 18, face = "bold", vjust = 1),
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold")
  )

Ion_Mode_LCMS_Superclass <- Ion_Mode %>%
  filter(Method == "LC/MS") %>%
  group_by(Ion_Mode, Superclass) %>%
  summarise(Count = n(), .groups = "drop_last") %>%
  group_by(Ion_Mode) %>%
  mutate(Percent = Count / sum(Count) * 100) %>% 
  ungroup() %>%
  mutate(Superclass = as.character(Superclass)) %>%
  group_by(Ion_Mode) %>%
  mutate(
    Superclass = if_else(Percent < 1, "Other", Superclass)
  ) %>%
  ungroup() %>%
  group_by(Ion_Mode, Superclass) %>%
  summarise(
    Percent = sum(Percent),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  group_by(Ion_Mode) %>%
  arrange(Ion_Mode, desc(Percent)) %>%
  mutate(Superclass = factor(Superclass, levels = unique(Superclass))) %>%
  ungroup() 


ggplot(Ion_Mode_LCMS_Superclass, aes(x = "", y = Percent, fill = Superclass)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  facet_wrap(~Ion_Mode,
             labeller = labeller(
               Ion_Mode = c(
                 "-" = "Negative Mode<br><span style='font-size:12pt; font-weight:normal; font-style:italic'>(n = 20, m = 1344)</span>",
                 "+" = "Positive Mode<br><span style='font-size:12pt; font-weight:normal; font-style:italic'>(n = 38, m = 3605)</span>"))) +
  scale_fill_manual(values = Superclass_colors) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Superclass"
  ) +
  theme_void() +
  theme(
    legend.title = element_text(size = 18, face = "bold"),
    legend.text  = element_text(size = 12),
    strip.text = ggtext::element_markdown(size = 18, face = "bold", vjust = 1),
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold")
  )

## MS/Column/Ion ---------------------------------------------------------------
Col_Ion <- Soil_Metabolome_Database %>%
  filter(Metabolite_Lipid == "Metabolite") %>% # just metabolites
  filter(!is.na(Ion_Mode)) %>%
  filter(!is.na(Superclass)) %>% #only classified compounds
  filter(Ion_Mode != "+/-") %>%
  filter(!is.na(Column)) %>%
  filter(Column != "NA") %>%
  filter(Column %in% c("GC_5%_Diphenyl", "LC_C18", "LC_HILIC")) %>%
  unite("Col_Ion", Column, Ion_Mode, sep = "_", remove = TRUE)

Col_Ion_totals <- Col_Ion_Superclass %>%
  group_by(Col_Ion) %>%
  summarise(
    Total_Count = sum(Count),
    .groups = "drop"
  )

Col_Ion_Superclass <- Col_Ion %>%
  group_by(Col_Ion, Superclass) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Col_Ion) %>%
  mutate(Percent = Count / sum(Count) * 100) %>%
  ungroup() %>%
  mutate(Superclass = if_else(Percent < 1, "Other", Superclass)) %>%
  group_by(Col_Ion, Superclass) %>%
  summarise(
    Count = sum(Count),
    Percent = sum(Percent),
    .groups = "drop"
  ) %>%
  group_by(Col_Ion) %>%
  arrange(Col_Ion, desc(Percent)) %>%
  mutate(Superclass = factor(Superclass, levels = unique(Superclass))) %>%
  ungroup()

Col_Ion_DOI<- Col_Ion %>%
  group_by(Col_Ion) %>%
  summarise(n_DOIs = n_distinct(DOI), .groups = "drop") %>%
  arrange(desc(n_DOIs))

Col_Ion_Superclass$Col_Ion <- factor(
  Col_Ion_Superclass$Col_Ion,
  levels = c(
    "GC_5%_Diphenyl_+",
    "LC_C18_+",
    "LC_C18_-",
    "LC_HILIC_+",
    "LC_HILIC_-"
  )
)

ggplot(Col_Ion_Superclass, aes(x = "", y = Percent, fill = Superclass)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = Superclass_colors) +
  facet_wrap(~Col_Ion,
             labeller = labeller(
               Col_Ion = c(
                 "GC_5%_Diphenyl_+" = "GC-(5% Phenyl)-(EI+)<br><span style='font-size:12pt; font-weight:normal; font-style:italic'>(n = 20, m = 1970)</span>",
                 "LC_C18_+" = "LC-C18-(ESI+)<br><span style='font-size:12pt; font-weight:normal; font-style:italic'>(n = 13, m = 1529)</span>",
                 "LC_C18_-" = "LC-C18-(ESI-)<br><span style='font-size:12pt; font-weight:normal; font-style:italic'>(n = 12, m = 1214)</span>",
                 "LC_HILIC_+" = "LC-HILIC-(ESI+)<br><span style='font-size:12pt; font-weight:normal; font-style:italic'>(n = 10, m = 743)</span>",
                 "LC_HILIC_-" = "LC-HILIC-(ESI-)<br><span style='font-size:12pt; font-weight:normal; font-style:italic'>(n = 10, m = 619)</span>"
               )
             )
  ) +
  theme_void() +
  labs(fill = "Superclass") +
  theme(
    legend.title = element_text(size = 18, face = "bold"),
    legend.text  = element_text(size = 12),
    #legend.position = "none",
    strip.text = ggtext::element_markdown(size = 16, face = "bold"),
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold")
  )

## Number of Metabolites per study -----------------------------------------
MperDOI <- Soil_Metabolome_Database %>%
  filter(Metabolite_Lipid == "Metabolite") %>%
  distinct(INCHIKEY, DOI) %>%
  group_by(DOI) %>%
  summarise(n_metabolites = n_distinct(INCHIKEY), .groups = "drop")

summary(MperDOI$n_metabolites)

ggplot(MperDOI, aes(x = n_metabolites)) +
  geom_histogram(
    breaks = seq(0, 600, 25),
    fill = "#3B7D23",
    color = "black"
  ) +
  geom_vline(
    xintercept = 128,
    linetype = "dashed",
    color = "black",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = 128,
    y = Inf,
    label = "Mean = 128",
    vjust = 4,
    hjust = -.1,
    color = "black",
    angle = 0,
    size = 7
  ) +
  labs(
    x = "Metabolites per Study",
    y = "Studies"
  ) +
  scale_x_continuous(
    breaks = seq(0, 600, 50)
  ) +
  scale_y_continuous(
    breaks = seq(0, 18, 2)
  ) +
  theme_bw(base_size = 15) +
  theme(
    panel.grid    = element_blank(),
    axis.title.x  = element_text(size = 20, vjust = 0.25),
    axis.text.x   = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y   = element_text(size = 14),
    axis.title.y  = element_text(size = 20, margin = margin(r = 7))
  )



## Chromatography ------------------------------------------------------
table(Soil_Metabolome_Database$Method)

Chomatography_colors <- c(
  "GC/MS" = "navajowhite3",
  "LC/MS" = "slateblue2",
  "NMR" = "palevioletred3",
  "CE/MS" = "darkolivegreen4",
  "LC/IMS/MS" = "lightblue",
  "AEC/MS" = "darkblue")

Chomatography <- Soil_Metabolome_Database %>%
  filter(!is.na(`Method`)) %>%          # remove NA values
  group_by(`Method`) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(desc(Count)) %>%
  mutate(
    Percent = Count / sum(Count) * 100,
    `Method` = factor(`Method`, levels = `Method`)
  )

ggplot(Chomatography, aes(x = "", y = Percent, fill = `Method`)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = Chomatography_colors) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Method"
  ) +
  theme_void() +
  theme(
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 12)
  )

## Van Krevelen ------------------------------------------------------------
Formulas <- Soil_Metabolome_Database %>%
  filter(Metabolite_Lipid == "Metabolite") %>%
  distinct(INCHIKEY, .keep_all = TRUE) %>%
  filter(!is.na(Formula)) %>%
  select(c(Formula:Si)) %>%
  mutate(across(C:Si, ~ replace(., is.na(.) | . == FALSE, 0))) %>%
  mutate(H_C = H/C) %>%
  mutate(O_C = O/C) %>%
  group_by(O_C, H_C) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(n)

ggplot(Formulas, aes(x = O_C, y = H_C, color = n)) +
  geom_point() +
  scale_color_gradient(
    low = "grey90",
    high = "black",
    limits = c(1, 15),
    oob = scales::squish, #cap at 15
    breaks = c(1, 5, 10, 15),
    labels = function(x) ifelse(x == 15, "15+", x),
    name = "Count"
  ) +
  theme_bw(base_size = 15) +
  labs(
    x = "O/C",
    y = "H/C",
    color = "Count"
  ) +
  xlim(0,3) +
  ylim(0,3) +
  theme(
    panel.grid    = element_blank(),
    axis.title.x  = element_text(size = 20, vjust = 0.25),
    axis.text.x   = element_text(size = 14, hjust = 1),
    axis.text.y   = element_text(size = 14),
    axis.title.y  = element_text(size = 20, margin = margin(r = 7))
  )

Formulas_Method <- Soil_Metabolome_Database %>%
  filter(Metabolite_Lipid == "Metabolite") %>%
  select(c(INCHIKEY,Formula, C,H,O,Method)) %>%
  filter(!Method %in% c("AEC/MS", "CE/MS", "C")) %>%
  mutate(Method = if_else(Method == "LC/IMS/MS", "LC/MS", Method)) %>% #grouping LC/IMS/MS with LC/MS
  filter(!is.na(Formula)) %>%
  mutate(across(C:O, ~ replace(., is.na(.) | . == FALSE, 0))) %>%
  mutate(H_C = H/C) %>%
  mutate(O_C = O/C) %>%
  group_by(Method, Formula) %>%
  summarise(
    C = first(C),
    H = first(H),
    O = first(O),
    H_C = first(H_C),
    O_C = first(O_C),
    count = n(),
    .groups = "drop"
  )

Formulas_Method$Method <- as.factor(Formulas_Method$Method)

table(Formulas_Method$Method)

ggplot(Formulas_Method, aes(x = O_C, y = H_C, color = Method, alpha = count)) +
  geom_point() +
  stat_ellipse(linewidth = 1.2) +
  theme_bw(base_size = 15) +
  labs(
    x = "O/C",
    y = "H/C",
    color = "Method",
    alpha = "Count"
  ) +
  xlim(0, 2) +
  ylim(0, 3) +
  scale_alpha(range = c(0.2, 1)) +  # keeps low counts visible but lighter
  theme(
    panel.grid    = element_blank(),
    axis.title.x  = element_text(size = 20, vjust = 0.25),
    axis.text.x   = element_text(size = 14, hjust = 1),
    axis.text.y   = element_text(size = 14),
    axis.title.y  = element_text(size = 20, margin = margin(r = 7))
  )

filter(Formulas_Method, Method == "GC/MS")

ggplot(filter(Formulas_Method, Method == "GC/MS"), aes(x = O_C, y = H_C, color = count)) +
  geom_point() +
  scale_color_gradient(
    low = "grey90",
    high = "black",
    limits = c(1, 15),
    oob = scales::squish, #cap at 15
    breaks = c(1, 5, 10, 15),
    labels = function(x) ifelse(x == 15, "15+", x),
    name = "Count"
  ) +
  #stat_ellipse(linewidth = 1.2) +
  theme_bw(base_size = 15) +
  labs(
    x = "O/C",
    y = "H/C",
    color = "Method",
    alpha = "Count"
  ) +
  xlim(0, 3) +
  ylim(0, 3) +
  theme(
    panel.grid    = element_blank(),
    axis.title.x  = element_text(size = 20, vjust = 0.25),
    axis.text.x   = element_text(size = 14, hjust = 1),
    axis.text.y   = element_text(size = 14),
    axis.title.y  = element_text(size = 20, margin = margin(r = 7))
  )

Formulas_Solvent <- Soil_Metabolome_Database %>%
  filter(Metabolite_Lipid == "Metabolite") %>%
  select(c(INCHIKEY,Formula, C,H,O,Solvent_Matrix)) %>%
  filter(!is.na(Formula)) %>%
  mutate(across(C:O, ~ replace(., is.na(.) | . == FALSE, 0))) %>%
  mutate(H_C = H/C) %>%
  mutate(O_C = O/C) %>%
  group_by(Solvent_Matrix, Formula) %>%
  summarise(
    C = first(C),
    H = first(H),
    O = first(O),
    H_C = first(H_C),
    O_C = first(O_C),
    count = n(),
    .groups = "drop"
  )

Formulas_Solvent_top <- Formulas_Solvent %>%
  filter(Solvent_Matrix %in% c("MeOH/Water", "Water", "MeOH/IPA/Water", "Biphasic (Aqueous)", "MeOH", "Headspace"))

ggplot(Formulas_Solvent_top, aes(x = O_C, y = H_C, color = Solvent_Matrix, alpha = count)) +
  geom_point() +
  stat_ellipse(linewidth = 1.2) +
  theme_bw(base_size = 15) +
  labs(
    x = "O/C",
    y = "H/C",
    color = "Method",
    alpha = "Count"
  ) +
  xlim(0, 2) +
  ylim(0, 3) +
  scale_alpha(range = c(0.2, 1)) +  # keeps low counts visible but lighter
  theme(
    panel.grid    = element_blank(),
    axis.title.x  = element_text(size = 20, vjust = 0.25),
    axis.text.x   = element_text(size = 14, hjust = 1),
    axis.text.y   = element_text(size = 14),
    axis.title.y  = element_text(size = 20, margin = margin(r = 7))
  )

Formulas_Solvent_uncounted <- Soil_Metabolome_Database %>%
  filter(Metabolite_Lipid == "Metabolite") %>%
  select(c(INCHIKEY,Formula, C,H,O,Solvent_Matrix)) %>%
  filter(!is.na(Formula)) %>%
  mutate(across(C:O, ~ replace(., is.na(.) | . == FALSE, 0))) %>%
  mutate(H_C = H/C) %>%
  mutate(O_C = O/C) %>%
  filter(Solvent_Matrix %in% c("MeOH/Water", "Water", "MeOH/IPA/Water")) %>%
  filter(!H_C %in% c("NaN", "Inf", "NA")) %>%
  filter(!O_C %in% c("NaN", "Inf", "NA"))
  
Formulas_Solvent_uncounted$Solvent_Matrix <- factor(
  Formulas_Solvent_uncounted$Solvent_Matrix,
  levels = c("MeOH/Water", "Water", "MeOH/IPA/Water")
)

ggplot(Formulas_Solvent_uncounted, aes(x = Solvent_Matrix, y = H_C, fill = Solvent_Matrix)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_manual(values = c(
    "MeOH/Water" = "#1b9e77",
    "Water" = "dodgerblue2",
    "MeOH/IPA/Water" = "#d95f02"
  )) +
  labs(
    x = "",
    y = "H/C"
  ) +
  theme(
    panel.grid    = element_blank(),
    legend.position = "none",
    axis.title.x  = element_text(size = 20, vjust = -1),
    axis.text.x   = element_text(size = 14),
    axis.text.y   = element_text(size = 14),
    axis.title.y  = element_text(size = 20, margin = margin(r = 7))
  )

ggplot(Formulas_Solvent_uncounted, aes(x = Solvent_Matrix, y = O_C, fill = Solvent_Matrix)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_manual(values = c(
    "MeOH/Water" = "#1b9e77",
    "Water" = "dodgerblue2",
    "MeOH/IPA/Water" = "#d95f02"
  )) +
  labs(
    x = "",
    y = "O/C"
  ) +
  theme(
    panel.grid    = element_blank(),
    legend.position = "none",
    axis.title.x  = element_text(size = 20, vjust = -1),
    axis.text.x   = element_text(size = 14),
    axis.text.y   = element_text(size = 14),
    axis.title.y  = element_text(size = 20, margin = margin(r = 7))
  )



# Metadata ----------------------------------------------------------------
## Publication Year --------------------------------------------------------
Pub_dates <- SMDB_Metadata[c("Year_Published")] %>%
  filter(!is.na(Year_Published)) %>%
  filter(Year_Published != "NA") %>%
  mutate(Year_Published = as.numeric(Year_Published)) %>%
  count(Year_Published) %>%
  complete(Year_Published = seq(min(Year_Published), max(Year_Published), by = 1),
           fill = list(n = 0))

total_pubs <- nrow(SMDB_Metadata) #this includes unpublished PIE metabolomics and internal SE

ggplot(Pub_dates, aes(x = Year_Published, y = n)) +
  geom_col(fill = "#7B904B", color = "black", width = 0.7) +
  labs(x = "Publication Date", y = "Studies") +
  scale_x_continuous(breaks = Pub_dates$Year_Published) +
  theme_bw() +
  theme(
    panel.grid    = element_blank(),
    axis.title.x  = element_text(size = 20, vjust = 0.25),
    axis.text.x   = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y   = element_text(size = 14),
    axis.title.y  = element_text(size = 20, margin = margin(r = 7))
  ) +
  annotate(
    "text",
    x = Inf, y = Inf,
    label = paste0("Sources reviewed = ", total_pubs),
    hjust = 1.5, vjust = 3,
    size = 7
  )

Pub_dates <- SMDB_Metadata %>%
  filter(!is.na(Year_Published), Year_Published != "NA") %>%
  mutate(
    Year_Published = as.numeric(Year_Published),
    Year_Group = ifelse(Year_Published < 2017, "< 2017", as.character(Year_Published))
  ) %>%
  count(Year_Group) %>%
  mutate(
    Year_Group = factor(
      Year_Group,
      levels = c("< 2017", as.character(2017:max(as.numeric(Year_Group[Year_Group != "< 2017"]))))
    )
  )

ggplot(Pub_dates, aes(x = Year_Group, y = n)) +
  geom_col(fill = "#3B7D23", color = "black", width = 0.7) +
  labs(x = "Publication Date", y = "Studies") +
  theme_bw() +
  theme(
    panel.grid    = element_blank(),
    axis.title.x  = element_text(size = 20, vjust = 0.25),
    axis.text.x   = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y   = element_text(size = 14),
    axis.title.y  = element_text(size = 20, margin = margin(r = 7))
  ) +
  annotate(
    "text",
    x = Inf, y = Inf,
    label = paste0("Sources reviewed = ", total_pubs),
    hjust = 1.6, vjust = 3,
    size = 7
  )
## Locations ---------------------------------------------------------------
SMDB_Locations_Map <- SMDB_Locations %>%
  filter(Location_Source != "Not provided") 

nrow(SMDB_Locations_Map) #number of locations across studies with provided location
nrow(SMDB_Locations) #number of locations across all studies

pal <- colorFactor("viridis", SMDB_Locations_Map$DOI)

leaflet(SMDB_Locations_Map) %>%
  addTiles() %>%
  addMarkers(
    ~Longitude, ~Latitude,
    popup = ~paste(
      "Lat:", Latitude,
      "<br>Lon:", Longitude,
      "<br>DOI:", DOI
    )) %>%
  addCircleMarkers(
    ~Longitude, ~Latitude,
    color = ~pal(DOI),
    fillColor = ~pal(DOI),
    radius = 8,
    fillOpacity = 0.6,
    stroke = FALSE
  )

leaflet(SMDB_Locations_Map) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    ~Longitude, ~Latitude,
    color = "white",
    weight = 1,
    fillColor = ~pal(DOI),
    radius = ~6,
    fillOpacity = 0.8,
    popup = ~paste0(
      "<b>Location</b><br>",
      "Lat: ", round(Latitude, 3), "<br>",
      "Lon: ", round(Longitude, 3), "<br>",
      "<b>DOI:</b> ", DOI
    )
  ) 

#Extraction techniques
Extraction_Techniques <- as.data.frame(unique(Soil_Metabolome_Database$`Extraction Solvent`))

#Confidence
Confidences <- Soil_Metabolome_Database %>%
  group_by(`Schymanski Conf level`) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(desc(Count))



##Studies Included ------------------------------------------------------
Included <- SMDB_Metadata %>%
  select(DOI,Included, Clear_Annotation, Full_Metabolite_List, Other_Issues)

included <- Included$Included == "Yes"
clear_only <- Included$Clear_Annotation == "No" & Included$Full_Metabolite_List == "Yes"
metabolite_only <- Included$Full_Metabolite_List == "No" & Included$Clear_Annotation == "Yes"
both_no <- Included$Clear_Annotation == "No" & Included$Full_Metabolite_List == "No"
other <- !(included | clear_only | metabolite_only | both_no)

counts <- c(
  Included = sum(included, na.rm = TRUE),
  Clear_Annotation_No_Only = sum(clear_only, na.rm = TRUE),
  Full_Metabolite_No_Only = sum(metabolite_only, na.rm = TRUE),
  Both_No = sum(both_no, na.rm = TRUE),
  Other_Issues = sum(other, na.rm = TRUE)
) %>%
  as.data.frame() %>%
  rownames_to_column("Reason")

colnames(counts)[c(2)] <- "Count"


counts$Reason <- factor(counts$Reason,
                        levels = c("Other_Issues", "Both_No", "Full_Metabolite_No_Only","Clear_Annotation_No_Only", "Included"),
                        labels = c("Other", "Annotation and \ndata availability", "Data availability", "Annotation", "Included")
)

counts$pattern <- ifelse(counts$Reason == "Unclear annotation and no data", "stripe", "none")


ggplot(counts, aes(x = Reason, y = Count, fill = Reason)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5), size = 8) +
  scale_fill_manual(values = c(
    "Included" = alpha("forestgreen", 0.6),
    "Annotation" = alpha("tan2", 0.6),
    "Data availability" = alpha("chocolate4", 0.6),
    "Annotation and \ndata availability" = alpha("red3", 0.6),
    "Other" = alpha("grey", 0.6)
  )) +
  labs(x = NULL, y = "Number of studies") +
  theme_bw() +
  geom_vline(xintercept = 4.5, linetype = "dashed", linewidth = 1) +
  annotate("text",
           x = 4.2, 
           y = 4.5 * 0.95,
           label = "Excluded",
           size = 6,
           hjust = -3) +
  annotate("text",
           x = 4.8, 
           y = 4.5 * 0.95,
           label = "Included",
           size = 6,
           hjust = -3.25) +

  theme(
    panel.grid    = element_blank(),
    axis.title.x  = element_text(size = 20, vjust = 0.25),
    axis.text.x   = element_text(size = 14, hjust = 1),
    axis.text.y   = element_text(size = 18),
    axis.title.y  = element_text(size = 20, margin = margin(r = 7))
  )
  
 


## Soil Types --------------------------------------------------------------
Soil_Types <- SMDB_Locations %>%
  select(Site_description) %>%
  filter(Site_description != "Unclassified") %>%
  table() %>%
  as.data.frame() %>%
  filter(Site_description != "NA") %>%
  arrange(Freq) %>%
  mutate(Site_description = str_to_title(Site_description))


ggplot(Soil_Types, aes(x = reorder(Site_description, Freq), y = Freq, fill = Site_description)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5), size = 5) +
  scale_fill_manual(values = c(
    "Forest" = alpha("forestgreen", 0.6),
    "Agricultural" = alpha("darkorange3", 0.6),
    "Arid" = alpha("chocolate4", 0.6),
    "Grassland" = alpha("olivedrab", 0.6),
    "Wetland" = alpha("brown", 0.6),
    "Permafrost" = alpha("steelblue1", 0.6),
    "Rainforest" = alpha("dodgerblue3", 0.6),
    "Tidal Wetland" = alpha("steelblue4", 0.6)
  )) +
  labs(x = NULL, y = "Soils") +
  theme_bw() +
  theme(
    panel.grid    = element_blank(),
    axis.title.x  = element_text(size = 16, vjust = 0.25),
    axis.text.x   = element_text(size = 14, hjust = 1),
    axis.text.y   = element_text(size = 14),
    axis.title.y  = element_text(size = 20, margin = margin(r = 7))
  )

Soil_Types2 <- Soil_Types %>%
  arrange(desc(Freq)) %>%
  mutate(
    fraction = Freq / sum(Freq),
    ymax = cumsum(fraction),
    ymin = lag(ymax, default = 0),
    label_pos = (ymax + ymin) / 2
  )

ggplot(Soil_Types2, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = Site_description)) +
  geom_rect(color = "white") +
  coord_polar(theta = "y", clip = "off") +
  xlim(c(2, 5.5)) +
  geom_text_repel(
    aes(
      x = 4.5,
      y = label_pos,
      label = paste0(Site_description, " (", Freq, ")")
    ),
    segment.size = 0.8,
    size = 5,
    direction = "y",
    hjust = 0
  ) +
  scale_fill_manual(values = c(
    "forest" = alpha("forestgreen", 0.6),
    "agricultural" = alpha("olivedrab", 0.6),
    "biocrust" = alpha("tan2", 0.6),
    "grassland" = alpha("red3", 0.6),
    "wetland" = alpha("brown", 0.6),
    "permafrost" = alpha("blue", 0.6),
    "rainforest" = alpha("springgreen1", 0.6),
    "tidal wetland" = alpha("steelblue4", 0.6)
  )) +
  theme_void() +
  theme(
    plot.margin = margin(10, 100, 10, 10),
    legend.position = "none"
  )
#Ion Mode----

Ion_colors <- c(
  "GC/MS_+" = "grey100",
  "LC/MS_+" = "black",
  "LC/MS_-" = "blue")

Ion_Mode <- Soil_Metabolome_Database %>%
  filter(Metabolite_Lipid == "Metabolite") %>%
  filter(!is.na(Superclass)) %>%
  mutate(Method = if_else(Method == "LC/IMS/MS", "LC/MS", Method)) %>%
  filter(Method != "CE/MS") %>%
  filter(Method != "AEC/MS") %>%
  filter(Ion_Mode != "+/-") %>%
  filter(!is.na(Ion_Mode)) %>%
  unite("Method_Ion", Method, Ion_Mode, sep = "_", remove = TRUE) %>%
  group_by(Method_Ion, Superclass) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Method_Ion) %>%
  mutate(Percent = Count / sum(Count) * 100) %>%
  ungroup() %>%
  mutate(Superclass = if_else(Percent < 1, "Other", Superclass)) %>%
  group_by(Method_Ion, Superclass) %>%
  summarise(
    Count = sum(Count),
    Percent = sum(Percent),
    .groups = "drop"
  ) %>%
  group_by(Method_Ion) %>%
  arrange(Method_Ion, desc(Percent)) %>%
  mutate(Superclass = factor(Superclass, levels = unique(Superclass))) %>%
  ungroup()

Method_Ion_totals <- Ion_Mode %>%
  group_by(Method_Ion) %>%
  summarise(
    Total_Count = sum(Count),
    .groups = "drop"
  )

Method_ion_doi <- Soil_Metabolome_Database %>%
  filter(Metabolite_Lipid == "Metabolite") %>%
  filter(!is.na(Superclass)) %>%
  mutate(Method = if_else(Method == "LC/IMS/MS", "LC/MS", Method)) %>%
  filter(Method != "CE/MS") %>%
  filter(Method != "AEC/MS") %>%
  filter(Ion_Mode != "+/-") %>%
  filter(!is.na(Ion_Mode)) %>%
  unite("Method_Ion", Method, Ion_Mode, sep = "_", remove = TRUE) %>%
  group_by(Method_Ion) %>%
  summarise(n_DOIs = n_distinct(DOI), .groups = "drop") %>%
  arrange(desc(n_DOIs))

Ion_Mode$Method_Ion <- factor(
  Ion_Mode$Method_Ion,
  levels = c("GC/MS_+", "LC/MS_+", "LC/MS_-")
)


ggplot(Ion_Mode, aes(x = "", y = Percent, fill = Superclass)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = Superclass_colors) +
  facet_wrap(~Method_Ion,
             labeller = labeller(
               Method_Ion = c(
                 "GC/MS_+" = "GC/MS (EI+)<br><span style='font-size:12pt; font-weight:normal; font-style:italic'>(n = 23, m = 2403)</span>",
                 "LC/MS_+" = "LC/MS (ESI+)<br><span style='font-size:12pt; font-weight:normal; font-style:italic'>(n = 21, m = 2272)</span>",
                 "LC/MS_-" = "LC/MS (ESI-)<br><span style='font-size:12pt; font-weight:normal; font-style:italic'>(n = 21, m = 1833)</span>"
               )
             )
  ) +
  theme_void() +
  labs(fill = "Superclass") +
  theme(
    legend.title = element_text(size = 18, face = "bold"),
    legend.text  = element_text(size = 12),
    legend.position = "none",
    strip.text = ggtext::element_markdown(size = 16, face = "bold"),
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold")
  )


Metabolites <- Soil_Metabolome_Database %>%
  distinct(INCHIKEY, .keep_all = TRUE) %>% 
  group_by(INCHIKEY) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(desc(Count))

Metabolites <- Soil_Metabolome_Database %>%
  group_by(INCHIKEY) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(desc(Count))


nrow(Soil_Metabolome_Database)
nrow(distinct(Soil_Metabolome_Database, INCHIKEY, .keep_all = TRUE)) 


Ion_Mode_subclass <- Soil_Metabolome_Database %>%
  filter(Metabolite_Lipid == "Metabolite") %>%
  filter(!is.na(subclass)) %>%
  mutate(Method = if_else(Method == "LC/IMS/MS", "LC/MS", Method)) %>%
  filter(Method != "CE/MS") %>%
  filter(Method != "AEC/MS") %>%
  filter(Ion_Mode != "+/-") %>%
  filter(!is.na(Ion_Mode)) %>%
  unite("Method_Ion", Method, Ion_Mode, sep = "_", remove = TRUE) %>%
  group_by(Method_Ion, subclass) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Method_Ion) %>%
  mutate(Percent = Count / sum(Count) * 100) %>%
  ungroup() %>%
  mutate(subclass = if_else(Percent < 1, "Other", subclass)) %>%
  group_by(Method_Ion, subclass) %>%
  summarise(
    Count = sum(Count),
    Percent = sum(Percent),
    .groups = "drop"
  ) %>%
  group_by(Method_Ion) %>%
  arrange(Method_Ion, desc(Percent)) %>%
  mutate(subclass = factor(subclass, levels = unique(subclass))) %>%
  ungroup()

#creating bins from -1 to 10^10 using sequence function seq()
bins <- seq(0, 1500, by = 5) 

mz_binned <- Soil_Metabolome_Database %>%
  filter(!is.na(`m/z`)) %>%                           # remove NA values
  mutate(
    mz_bin = cut(
      `m/z`,
      breaks = bins,
      include.lowest = TRUE,
      right = FALSE
    )
  ) %>%
  count(mz_bin)

ggplot(mz_binned, aes(x = mz_center, y = n)) +
  geom_col(width = 5, fill = "grey20") +
  scale_x_continuous(breaks = seq(0, 1500, by = 100)) +
  labs(x = "m/z", y = "Count") +
  theme_minimal() + theme(
    axis.line = element_line(color = "black", linewidth = 0.8),
    panel.grid = element_blank()
  ) + scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
  theme(
    axis.line.y   = element_line(color = "black", linewidth = 0.8),
    panel.grid    = element_blank(),
    axis.title.x  = element_text(size = 20),
    axis.text.x   = element_text(size = 18),
    axis.text.y   = element_text(size = 18), axis.title.y  = element_text(size = 20),

  )


#Locations----


leaflet(SMDB_Locations) %>%
     addTiles() %>%
     addMarkers(
         lng = ~Longitude,
         lat = ~Latitude,
      popup = ~paste("Lat:", Latitude, "<br>Lon:", Longitude, "<br>DOI:", DOI)
       )



doi_levels <- unique(SMDB_Locations$DOI)

SMDB_Locations$color <- doi_colors[SMDB_Locations$color]

doi_colors <- as.list(setNames(
  hcl(
    h = runif(length(doi_levels), 0, 360),
    c = 100,
    l = 65
  ),
  doi_levels
))

SMDB_Locations$color <- doi_colors[SMDB_Locations$DOI]
SMDB_Locations$color <- unlist(doi_colors[SMDB_Locations$DOI], use.names = FALSE)

nrow(SMDB_Locations)

leaflet(SMDB_Locations) %>%
  addTiles() %>%
  addMarkers(
    ~Longitude, ~Latitude,
    popup = ~paste(
      "Lat:", Latitude,
      "<br>Lon:", Longitude,
      "<br>DOI:", DOI
    )
  ) %>%
  addCircleMarkers(
    ~Longitude, ~Latitude,
    color = ~color,        # your DOI-based color column
    fillColor = ~color,
    radius = 8,
    fillOpacity = 0.6,
    stroke = FALSE
  )

leaflet(SMDB_Locations) %>%
  addTiles() %>%
  
  # DOI color layer (visual grouping)
  addCircleMarkers(
    lng = ~Longitude,
    lat = ~Latitude,
    fillColor = ~color,
    color = ~color,
    radius = 8,
    fillOpacity = 0.6,
    stroke = FALSE,
    group = "DOI groups"
  ) %>%
  
  # Pin layer (interaction)
  addMarkers(
    lng = ~Longitude,
    lat = ~Latitude,
    popup = ~paste(
      "<b>DOI:</b>", DOI,
      "<br><b>Lat:</b>", Latitude,
      "<br><b>Lon:</b>", Longitude
    ),
    group = "Locations"
  ) %>%
  
  # Layer toggle
  addLayersControl(
    overlayGroups = c("DOI groups", "Locations"),
    options = layersControlOptions(collapsed = FALSE)
  )

#Extraction techniques
Extraction_Techniques <- as.data.frame(unique(Soil_Metabolome_Database$`Extraction Solvent`))

#Confidence
Confidences <- Soil_Metabolome_Database %>%
  group_by(`Schymanski Conf level`) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(desc(Count))

#MWs-----
library(httr)
library(jsonlite)
library(webchem)

get_mw_safe <- function(inchikey){
  
  if(is.na(inchikey) || inchikey == "") return(NA_real_)
  
  url <- paste0(
    "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/inchikey/",
    inchikey,
    "/property/MolecularWeight/JSON"
  )
  
  tryCatch({
    
    res <- httr::GET(url)
    
    if(httr::status_code(res) != 200) return(NA_real_)
    
    data <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
    
    mw <- data$PropertyTable$Properties$MolecularWeight
    
    as.numeric(mw[1])   # take first result only
    
  }, error = function(e) NA_real_)
}


unique_inchikeys <- Soil_Metabolome_Database %>%
  filter(Metabolite_Lipid == "Metabolite") %>%
  distinct(INCHIKEY) %>%
  pull(INCHIKEY)

mw_values <- vapply(unique_inchikeys, get_mw_safe, numeric(1))# Optionally filter out NAs

mw_lookup <- data.frame(
  InChIKey = unique_inchikeys,
  MolecularWeight = mw_values
)


#creating bins from -1 to 10^10 using sequence function seq()
bins <- seq(0, 1500, by = 25) 

MW_binned <- mw_lookup %>%
  filter(!is.na(MolecularWeight)) %>%
  mutate(
    bin_start = floor(MolecularWeight / 25) * 25,
    bin_center = bin_start + 12.5
  ) %>%
  count(bin_center)

ggplot(MW_binned, aes(x = bin_center, y = n)) +
  geom_col(width = 25, fill = "#3B7D23", color = "black") +
  scale_x_continuous(
    breaks = seq(0, 1200, 100), 
    limits = c(0, 1200)
  ) +
  labs(
    x = "Molecular Weight (g/mol)",
    y = "Metabolites"
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_text(size = 20, vjust = 0.25),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1.1),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 20, margin = margin(r = 7))
  )

ggplot(MW_binned, aes(x = mz_bin, y = n)) +
  geom_col(fill = "black") +
  labs(
    x = "Molecular Weight (g/mol)",
    y = "Number of Compounds"
  ) +
  theme_bw()

#Extraction things----
coord_polar(theta = "y", clip = "off")

#check extraction solvents
table(Soil_Metabolome_Database$`Solvent Matrix`)

Solvent_Matrices <- Soil_Metabolome_Database %>%
  distinct(Source_DOI, `Solvent Matrix`) %>%      # remove duplicate pairs
  group_by(`Solvent Matrix`) %>%
  summarise(n_dois = n_distinct(Source_DOI)) %>%
  arrange(desc(n_dois))


pie_data <- Soil_Metabolome_Database %>%
  filter(`Extraction Solvent` != "variety of extraction conditions") %>%
  filter(`Extraction Solvent` != "NA") %>%
  filter(`Extraction Solvent` != "7:3 Water/Acetonitrile or 1:1 water/methanol") %>%
  filter(`Extraction Solvent` != "DMSO or 7:3 Water/Acetonitrile or 1:1 water/methanol") %>%
  filter(`Extraction Solvent` != "Water or 2.5% chloroform in water (v/v)") %>%
  count(`Extraction Solvent`, Superclass) %>%
  group_by(`Extraction Solvent`) %>%
  mutate(prop = n / sum(n))

pie_data <- Soil_Metabolome_Database %>%
  filter(!is.na(Superclass))  %>%
  distinct(`Source DOI`, INCHIKEY, .keep_all = TRUE) %>%
  filter(`Solvent Matrix` != "NA") %>%
  count(`Solvent Matrix`, Superclass) %>%
  group_by(`Solvent Matrix`) %>%
  mutate(prop = n / sum(n)) %>%
  filter(!is.na(Superclass)) 

Solvent_order <- c("MeOH/Water", 
              "Water", 
              "Biphasic (Aqueous)", 
              "Headspace", 
              "Biphasic (Chloroform)",
              "Biphasic (MBTE)",
              "MPLEx (All combined)",
              "MeOH/IPA/Water",
              "Aqueous NaCl",
              "MeOH",
              "ACN/IPA/Water",
              "Aqueous Formic Acid/MeOH/ACN",
              "MeOH/DCM",
              "MeOH/DCM/EtOAc/ACN"
              )

pie_data$`Solvent Matrix` <- factor(
  pie_data$`Solvent Matrix`,
  levels = Solvent_order
)

doi_counts <- Soil_Metabolome_Database %>%
  distinct(`Source DOI`, `Solvent Matrix`) %>%
  count(`Solvent Matrix`, name = "n_doi")

facet_labels <- doi_counts %>%
  mutate(
    solvent_label = str_replace_all(`Solvent Matrix`, "(/| )", "\\1\n"),
    label = paste0(solvent_label, "\n(n = ", n_doi, ")")
  ) %>%
  select(`Solvent Matrix`, label) %>%
  deframe()

ggplot(pie_data, aes(x = "", y = prop, fill = Superclass)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  facet_wrap(
    ~ `Solvent Matrix`,
    ncol = 5,
    labeller = labeller(`Solvent Matrix` = facet_labels)
  ) +
  scale_fill_manual(values = class_colors) +
  labs(fill = "Superclass") +
  theme_void() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.position = "none",
    plot.margin = margin(0, 0, 0, 0)
  )

#ion source
pie_data_Ion <- Soil_Metabolome_Database %>%
  filter(!is.na(Superclass))  %>%
  distinct(`Source DOI`, INCHIKEY, .keep_all = TRUE) %>%
  filter(`Ionization Source` != "NA") %>%
  count(`Ionization Source`, Superclass) %>%
  group_by(`Ionization Source`) %>%
  mutate(prop = n / sum(n))

doi_ionization <- Soil_Metabolome_Database %>%
  distinct(`Source DOI`, `Ionization Source`) %>%
  count(`Ionization Source`, name = "n_doi")

facet_labels_Ion <- doi_ionization %>%
  mutate(
    solvent_label = str_replace_all(`Ionization Source`, "(/| )", "\\1\n"),
    label = paste0(solvent_label, "\n(n = ", n_doi, ")")
  ) %>%
  select(`Ionization Source`, label) %>%
  deframe()


ggplot(pie_data_Ion, aes(x = "", y = prop, fill = Superclass)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  facet_wrap(
    ~ `Ionization Source`,
    ncol = 3,
    labeller = labeller(`Ionization Source` = facet_labels_Ion)
  ) +
  scale_fill_manual(values = class_colors) +
  labs(fill = "Superclass") +
  theme_void() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.position = "right",
    plot.margin = margin(0, 0, 0, 0)
  )


#method
pie_data_Method <- Soil_Metabolome_Database %>%
  filter(!is.na(Superclass))  %>%
  distinct(`Source DOI`, INCHIKEY, .keep_all = TRUE) %>%
  filter(`Method` != "NA") %>%
  count(`Method`, Superclass) %>%
  group_by(`Method`) %>%
  mutate(prop = n / sum(n))

doi_Method <- Soil_Metabolome_Database %>%
  distinct(`Source DOI`, `Method`) %>%
  count(`Method`, name = "n_doi")

facet_labels_Method <- doi_Method %>%
  mutate(
    solvent_label = str_replace_all(`Method`, "(/| )", "\\1\n"),
    label = paste0(solvent_label, "\n(n = ", n_doi, ")")
  ) %>%
  select(`Method`, label) %>%
  deframe()


ggplot(pie_data_Method, aes(x = "", y = prop, fill = Superclass)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  facet_wrap(
    ~ `Method`,
    ncol = 3,
    labeller = labeller(`Method` = facet_labels_Method)
  ) +
  scale_fill_manual(values = class_colors) +
  labs(fill = "Superclass") +
  theme_void() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.position = "right",
    plot.margin = margin(0, 0, 0, 0)
  )

#column
pie_data_Column <- Soil_Metabolome_Database %>%
  filter(!is.na(Superclass))  %>%
  distinct(`Source DOI`, INCHIKEY, .keep_all = TRUE) %>%
  filter(`ColumnMethod` != "NA") %>%
  count(`ColumnMethod`, Superclass) %>%
  group_by(`ColumnMethod`) %>%
  mutate(prop = n / sum(n))

doi_Column <- Soil_Metabolome_Database %>%
  distinct(`Source DOI`, `ColumnMethod`) %>%
  count(`ColumnMethod`, name = "n_doi")

facet_labels_Column <- doi_Column %>%
  mutate(
    solvent_label = str_replace_all(`ColumnMethod`, "(/| )", "\\1\n"),
    label = paste0(solvent_label, "\n(n = ", n_doi, ")")
  ) %>%
  select(`ColumnMethod`, label) %>%
  deframe()


ggplot(pie_data_Column, aes(x = "", y = prop, fill = Superclass)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  facet_wrap(
    ~ `ColumnMethod`,
    ncol = 3,
    labeller = labeller(`ColumnMethod` = facet_labels_Column)
  ) +
  scale_fill_manual(values = class_colors) +
  labs(fill = "Superclass") +
  theme_void() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.position = "right",
    plot.margin = margin(0, 0, 0, 0)
  )

unique(Soil_Metabolome_Database$`Source DOI`)

#inchis----
unique_doi <- filter(Soil_Metabolome_Database, Metabolite_Lipid == "Metabolite") %>%
  distinct(DOI)

InChIKeY_DOI <- Soil_Metabolome_Database %>%
  filter(Metabolite_Lipid == "Metabolite") %>%
  filter(!is.na(INCHIKEY))  %>%
  distinct(DOI, INCHIKEY, .keep_all = TRUE) %>%
  count(INCHIKEY, name = "freq") %>%
  arrange(desc(freq)) %>%
  mutate(rank = row_number()) %>%
  mutate(percentage = 100*freq/nrow(unique_doi))

#500x400
ggplot(InChIKeY_DOI, aes(x = rank, y = percentage)) +
  geom_line(color = "black", linewidth = 1) +
  theme_bw(base_size = 15) +
  theme(
    panel.grid = element_blank(),
      axis.title.x  = element_text(size = 20, vjust = 0.25),
      axis.text.x   = element_text(size = 14, hjust = 1),
      axis.text.y   = element_text(size = 14),
      axis.title.y  = element_text(size = 20, margin = margin(r = 7))
  ) +
  labs(
    x = "Unique Metabolites",
    y = "Percent of Studies"
  ) +
  ylim(0, 60)


top_inchi <- InChIKeY_DOI %>%
  filter(percentage > 10) %>% 
  merge(unique_metabolites[28:57], by = "INCHIKEY")


subclass_top_inchi <- top_inchi %>%
  filter(!is.na(subclass)) %>%
  group_by(subclass) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  arrange(desc(Count)) %>%
  mutate(
    Percent = Count / sum(Count) * 100,
    subclass = factor(subclass, levels = subclass) # keep bars ordered largest -> smallest
  )


ggplot(subclass_top_inchi, aes(x = "", y = Percent, fill = subclass)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  labs(
    x = NULL,
    y = NULL,
    fill = "subclass"
  ) +
  labs(fill = "Subclass") +
  theme_void() +
  theme(
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 12)
  )


Superclass_top_inchi <- top_inchi %>%
  filter(!is.na(Superclass)) %>%
  group_by(Superclass) %>%
  summarise(Count = n(), .groups = "drop_last") %>%
  mutate(Percent = Count / sum(Count) * 100) %>% 
  mutate(Superclass = as.character(Superclass)) %>%
  mutate(Superclass = if_else(Percent < 1, "Other", Superclass)) %>%
  group_by(Superclass) %>%
  summarise(
    Count = sum(Count),
    Percent = sum(Percent),
    .groups = "drop"
  ) %>%
  mutate(Superclass = factor(Superclass, levels = c("Organic acids and derivatives",
                                                    "Organic oxygen compounds",
                                                    "Lipids and lipid-like molecules",
                                                    "Organoheterocyclic compounds",
                                                    "Nucleosides, nucleotides, and analogues",
                                                    "Benzenoids",
                                                    "Hydrocarbons",
                                                    "Phenylpropanoids and polyketides",
                                                    "Other"))) #reorder


ggplot(Superclass_top_inchi, aes(x = "", y = Percent, fill = Superclass)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = Superclass_colors) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Superclass"
  ) +
  theme_void() +
  theme(
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 12),
    legend.position = "none"
  )


test <- Soil_Metabolome_Database %>%
  filter(Metabolite_Lipid == "Metabolite") %>%
  distinct(DOI, Extraction_Solvent, Column, INCHIKEY, .keep_all = TRUE)

test2<- Soil_Metabolome_Database %>%
  group_by(DOI, Extraction_Solvent, Column) %>%
  summarise(Metabolites = list(unique(INCHIKEY)), .groups = "drop")

# Prep data for website ---------------------------------------------------
Website_Locations <- SMDB_Locations %>%
  unite("coordinates", Latitude, Longitude, sep = ",", remove = TRUE) %>%
  select(c(DOI,Location_Source, coordinates, Site_description)) %>%
  group_by(DOI) %>%
  summarise(
    
    coordinates = paste(
      unique(na.omit(coordinates)),
      collapse = "; "
    ),
    
    Site_description = paste(
      unique(na.omit(Site_description)),
      collapse = "; "
    ),
    
    Location_Source = paste(
      unique(na.omit(Location_Source)),
      collapse = "; "
    ),
    
    across(
      -c(coordinates, Site_description, Location_Source),
      ~ {
        x <- na.omit(.x)
        if(length(x) == 0) "" else first(x)
      }
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    across(
      everything(),
      ~ replace(.x, .x %in% c("", "NA,NA"), NA)
    )
  )

results <- SMDB_Metadata$DOI[!SMDB_Metadata$DOI %in% SMDB_Locations$DOI]

Website_DB <- Soil_Metabolome_Database %>%
  inner_join(SMDB_Metadata, by = "DOI") %>%
  inner_join(Website_Locations, by = "DOI")

#write_xlsx(Website_DB, "filename.xlsx")

Website_DB$Column <- factor(Website_DB$Column,
                        levels = c("CE-silica", "GC_5%_Diphenyl", "GC_PEG","LC_AEX", "LC_C18", "LC_HILIC", "NMR"),
                        labels = c("CE (silica)", "GC (5% Phenyl)", "GC (PEG)", "LC (AEX)", "LC (C18)", "LC (HILIC)", "NMR")
)

#make a few colnames look prettier
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

# Website! ----------------------------------------------------------------

# ---- LOAD + PREP DATA 
# replace with your actual file
df <- Website_DB

dt <- as.data.table(df)

# clean InChIKey
dt[, InChIKey := toupper(str_trim(InChIKey))]

# optional: set key for fast lookup
setkey(dt, InChIKey)

get_structure_url <- function(InChIKey) {
  if (is.null(InChIKey) || InChIKey == "") return(NULL)
  
  paste0(
    "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/InChIKey/",
    InChIKey,
    "/PNG"
  )
}

hidden_cols <- c(
  "Classification",
  "Annotation",
  "Metabolite_Lipid",
  "Column",
  "Methods"
)

display_cols <- c("DOI","Metabolite", "Identification Method", "Confidence Level","InChIKey","Extraction Solvent", "Method", "Column", "Ionization Source","Ion Mode", "Adducts", "Soil Description","Site Description")

# ---- UI 
ui <- page_sidebar(
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font_size = "13px"   
  ),
  
  # ---- HEADER (smaller) 
  tags$div(
    style = "
      background: #CD853F;
      color: white;
      padding: 10px;
      border-radius: 0 0 10px 10px;
      margin-bottom: 8px;
      text-align: center;
    ",
    tags$h4("Soil Metabolite Database", style = "margin:0; font-size:18px;")
  ),
  
  sidebar = sidebar(
    open = TRUE,
    width = 300,   # 👈 narrower sidebar
    
    # ---- MODE 
    radioButtons(
      "mode",
      "Type",
      choices = c("Metabolites" = "metabolites",
                  "Lipids" = "lipids"),
      selected = "metabolites"
    ),
    
    tags$hr(style = "margin:6px 0;"),
    
    # ---- METABOLITES 
    conditionalPanel(
      condition = "input.mode == 'metabolites'",
      
      textInput(
        "InChIKey",
        "InChIKey",
        placeholder = "BSYNRYMUTXBXSQ-UHFFFAOYSA-N"
      ),
      
      textAreaInput(
        "batch_keys",
        "Batch (one per line)",
        placeholder = "KEY1\nKEY2",
        rows = 4
      )
    ),
    
    # ---- LIPIDS 
    conditionalPanel(
      condition = "input.mode == 'lipids'",
      
      selectizeInput(
        "lipid_class",
        "Lipid class",
        choices = sort(unique(dt$Lipid_Class)),
        multiple = TRUE
      ),
      
      checkboxInput(
        "select_all_lipids",
        "Select all",
        value = FALSE
      )
    ),
    
    actionButton("search", "Search", class = "btn-primary"),
    
    tags$hr(style = "margin:6px 0;"),
    
    uiOutput("structure_ui"),
    
    downloadButton("download_data", "CSV")
  ),
  
  tagList(
  
  card(
    full_screen = TRUE,
    style = "margin-bottom:10px;",
    card_header("Database Entries"),
    DTOutput("results"),
    height = "45vh"
  ),
  
  card(
    full_screen = TRUE,
    card_header("Entry Details"),
    uiOutput("details")
  )
)
)

# ---- SERVER 
server <- function(input, output, session) {
  
  # ---- FILTER 
  filtered <- eventReactive(input$search, {
    
    # ---------------- METABOLITES 
    if (input$mode == "metabolites") {
      
      keys <- c()
      
      if (!is.null(input$InChIKey) && input$InChIKey != "") {
        keys <- c(keys, input$InChIKey)
      }
      
      if (!is.null(input$batch_keys) && input$batch_keys != "") {
        batch <- unlist(strsplit(input$batch_keys, "\n"))
        keys <- c(keys, batch)
      }
      
      keys <- toupper(str_trim(keys))
      keys <- keys[keys != ""]
      
      dt[InChIKey %in% keys]
    }
    
    # ---------------- LIPIDS 
    else {
      
      res <- copy(dt)
      
      if (!isTRUE(input$select_all_lipids) &&
          !is.null(input$lipid_class) &&
          length(input$lipid_class) > 0) {
        
        res <- res[Lipid_Class %in% input$lipid_class]
      }
      
      res
    }
  })
  
  # ---- RESULTS TABLE 
  output$results <- renderDT({
    req(filtered())
    
    datatable(
      filtered()[, ..display_cols],
      selection = "single",
      rownames = FALSE,
      class = "compact stripe hover",
      options = list(
        pageLength = 8,
        scrollX = TRUE
      )
    )
  })
  
  # ---- STRUCTURE 
  output$structure_ui <- renderUI({
    
    req(input$mode == "metabolites")
    req(filtered())
    req(input$results_rows_selected)
    
    row <- filtered()[input$results_rows_selected]
    req(nrow(row) > 0)
    
    tags$div(
      tags$h5(style="font-size:12px;", "Structure"),
      
      tags$img(
        src = get_structure_url(row$InChIKey[1]),
        style = "
          width:100%;
          border-radius:8px;
          border:1px solid #ccc;
        "
      )
    )
  })
  
  # ---- ENTRY DETAILS (FIXED FACTOR ISSUE) 
  output$details <- renderUI({
    req(input$results_rows_selected)
    
    row <- filtered()[input$results_rows_selected]
    row <- as.data.frame(row)
    
    # SAFE CLEANING (NO FACTOR ERROR)
    row[] <- lapply(row, function(x) {
      if (is.factor(x)) x <- as.character(x)
      x[is.na(x) | x == ""] <- ""
      x
    })
    
    tags$div(
      style = "
        font-size:12px;
        padding: 8px;
        border-radius: 10px;
        background: #f8f9ff;
        border: 1px solid #ddd;
        max-height: 420px;
        overflow-y: auto;
      ",
      
      lapply(names(row), function(nm) {
        tags$div(
          style = "
            display:flex;
            justify-content:space-between;
            padding:3px 0;
            border-bottom:1px solid #eee;
          ",
          tags$div(style="font-weight:600; width:40%;", nm),
          tags$div(style="width:60%;", as.character(row[[nm]][1]))
        )
      })
    )
  })
  
  # ---- DOWNLOAD 
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("metabolite_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      fwrite(filtered(), file)
    }
  )
}

# ---- RUN 
shinyApp(ui, server)



library(shiny)
library(shinythemes)
library(data.table)
library(dplyr)
library(DT)
library(stringr)

# ================= DATA =================
df <- Website_DB
dt <- as.data.table(df)

dt[, InChIKey := toupper(str_trim(InChIKey))]
setkey(dt, InChIKey)

get_structure_url <- function(InChIKey) {
  if (is.null(InChIKey) || InChIKey == "") return(NULL)
  
  paste0(
    "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/InChIKey/",
    InChIKey,
    "/PNG"
  )
}

display_cols <- c(
  "Metabolite",
  "Extraction Matrix", "Method", "Column", "Confidence Level",
  "Ionization Source","Ion Mode", "Adducts",
  "Soil Description","Site Description", "InChIKey","DOI"
)

# ================= UI =================
ui <- fluidPage(
  
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
  
  # ================= HEADER (ONLY CHANGE IS HERE) =================
  tags$div(
    style = "
      background: rgba(205, 133, 64, 0.5);  /* ← UPDATED COLOR ONLY */
      color: #1a1a1a;
      padding: 14px;
      text-align: center;
      border-bottom: 1px solid #ddd;
    ",
    tags$div(class = "app-title", "Soil Metabolome Database")
  ),
  
  # ================= SEARCH BAR (UNCHANGED) =================
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
  
  # ================= MAIN =================
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
)

# ================= SERVER =================
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

# ================= RUN =================
shinyApp(ui, server)