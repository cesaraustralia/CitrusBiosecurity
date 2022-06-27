library(tidyverse)
library(leaflet)
library(geodata)
library(terra)
library(sf)


# reading/downloading data ------------------------------------------------
# test a species
gbif_data <- geodata::sp_occurrence(genus = "Diaphorina",
                                    species = "citri")

sp_coords <- gbif_data %>%
  dplyr::select(lon, lat, status = occurrenceStatus,
                country, species, genus, family) %>%
  drop_na(lon, lat)

sp_points <- st_as_sf(sp_coords, coords = c("lon", "lat"))
# bg_mask <- st_read("data/background_mask.gpkg")

leaflet() %>%
  addTiles() %>%
  # addPolygons(opacity = 0.4, color = NA) %>%
  addCircleMarkers(
    data = sp_points,
    radius = 4,
    stroke = FALSE,
    color = "red",
    label = ~species,
    fillOpacity = 0.4
  )



# modelling ---------------------------------------------------------------





