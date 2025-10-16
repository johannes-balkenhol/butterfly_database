# Load libraries
library(readxl)
library(writexl)
library(dplyr)
library(lubridate)

# === 1. Read original Excel data ===
db <- read_excel("data/butterfly_database.xlsx", col_types = c(
  "text",     # suborder
  "text",     # family
  "text",     # genus
  "text",     # species
  "text",     # genus_and_species
  "text",     # german_name
  "text",     # location
  "text",     # land
  "text",     # GPS_data
  "text",     # elevation (may convert below)
  "date",     # D_M_Y
  "text",     # date_accuracy
  "numeric",  # total_number_of_this_species
  "text",     # FFH_directive
  "text",     # red_list_Bavaria
  "text",     # breeding
  "text",     # collector
  "text"     # notes_of_the_digitizer
))


# === 2. Add persistent identifiers ===
# === 2. Add persistent identifiers ===
db <- db %>%
  mutate(PID = sprintf("ID%05d", row_number())) %>%   # Only store the raw PID
  relocate(PID, .before = suborder)


# === 3. Optional: improve data types ===
# Elevation as numeric if possible
db$elevation <- suppressWarnings(as.numeric(db$elevation))
# GPS data as character or split into lat/lon if needed later

# === 4. Save to new Excel file ===
write_xlsx(db, "data/butterfly_database_with_pid.xlsx")

# === 5. Update read-in code for new file ===
# Use this from now on:
db <- read_excel("data/butterfly_database_with_pid.xlsx", col_types = c(
  "text",     # PID
  "text",     # suborder
  "text",     # family
  "text",     # genus
  "text",     # species
  "text",     # genus_and_species
  "text",     # german_name
  "text",     # location
  "text",     # land
  "text",     # GPS_data
  "numeric",  # elevation
  "date",     # D_M_Y
  "text",     # date_accuracy
  "numeric",  # total_number_of_this_species
  "text",     # FFH_directive
  "text",     # red_list_Bavaria
  "text",     # breeding
  "text",     # collector
  "text"     # notes_of_the_digitizer
))
