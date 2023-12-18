library(tidyverse)
library(sf)
library(terra)
library(lubridate)
library(leaflet)
library(galah)
library(eks)
library(mapview)

range01 <- function(x){(x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))}

# load demographic data (SA2 level):
aus_SA2 <- read_sf("data/2021_Census/SA2_2021_AUST_GDA2020.shp")

sf_use_s2(F)
aus <- st_union(aus_SA2)
sf_use_s2(T)

aus_SA2_demographics <- read_csv("data/2021_Census/2021 Census GCP Statistical Area 2 for AUS/2021Census_G08_AUST_SA2.csv") %>%
  mutate(SA2_CODE21 = str_remove(SA2_CODE_2021, "SA2"))

col_demographics <- names(aus_SA2_demographics)

col_demographics <- col_demographics[str_detect(col_demographics, "_Tot_resp")]

aus_SA2_demographics <- aus_SA2_demographics %>%
  dplyr::select(SA2_CODE21, col_demographics)

names(aus_SA2_demographics) <- str_remove(names(aus_SA2_demographics), "_Tot_resp")

aus_SA2_demographics <- aus_SA2 %>% left_join(aus_SA2_demographics)

# load tourism 2016 data and convert to 2021 SA2
tourism_2016 <- read_csv("data/2021_Census/tourism_2016.csv") %>%
  left_join(read_csv("data/2021_Census/CG_SA2_2016_SA2_2021.csv")) %>%
  dplyr::select(SA2_CODE_2021, SA2_NAME_2021, establishments, rooms) %>%
  rename(SA2_CODE21 = SA2_CODE_2021,
         SA2_NAME21 = SA2_NAME_2021) %>%
  drop_na(SA2_CODE21)

# interpolate number of rooms for SAs with missing values based on number of establishments
library(mgcv)

gam_tourism <- gam(rooms ~ s(establishments, k = 4),
                   data = tourism_2016)

tourism_2016[which(is.na(tourism_2016$rooms)),4] <- round(predict(gam_tourism, tourism_2016 %>% filter(is.na(rooms))), 0)

aus_SA2_tourism <- aus_SA2 %>%
  left_join(tourism_2016) %>%
  mutate(rooms = replace_na(rooms, 0))

# load port of entry data:

# roads <- exp((log(0.5)/1000)*rast("output_maps/roads_prox.tif")) #1 km from road
# 
# seaport_prox_aus <- lapply(dir("ports", full.names = T),
#                            function(x){
#                              extract(exp((log(0.5)/200)*rast(x)/1000)*roads, vect(aus_SA2 %>% st_transform(3112)), fun = mean)
#                            }
# ) #200 km from seaport
# 
# seaport_prox_aus <- as.data.frame(do.call(cbind, lapply(seaport_prox_aus, function(x) x[,2])))
# names(seaport_prox_aus) <- tools::file_path_sans_ext(dir("ports"))
# 
# aus_SA2_seaports <- bind_cols(aus_SA2, seaport_prox_aus)
# 
# aus_SA2_seaports %>% write_sf("output_maps/aus_SA2_seaports.gpkg")

aus_SA2_seaports <- read_sf("output_maps/aus_SA2_seaports.gpkg")

# airport_prox_aus <- lapply(c("Adelaide", "Brisbane", "Cairns", "Darwin", "Melbourne", "Perth", "Sydney"),
#                            function(x){
#                              extract(exp((log(0.5)/200)*rast(paste0("output_maps/", x, "_prox.tif"))/1000) %>% project("WGS84"), vect(aus_SA2), fun = mean)
#                            }
# ) #200 km from airport
# 
# airport_prox_aus <- as.data.frame(do.call(cbind, lapply(airport_prox_aus, function(x) x[,2])))
# names(airport_prox_aus) <- c("Adelaide", "Brisbane", "Cairns", "Darwin", "Melbourne", "Perth", "Sydney")
# 
# aus_SA2_airports <- bind_cols(aus_SA2, airport_prox_aus)
# 
# aus_SA2_airports %>% write_sf("output_maps/aus_SA2_airports.gpkg")

aus_SA2_airports <- read_sf("output_maps/aus_SA2_airports.gpkg")

#### Pathways ####

### Pathway 1: Air passengers ###

## 1.1: International visitors ##
travel_short <- read_csv("data/Overseas_travel/travel_short.csv") %>% mutate(type = "Short-term visitors")
travel_res <- read_csv("data/Overseas_travel/travel_res.csv") %>% mutate(type = "Returning residents")

reasons <- read_csv("data/Overseas_travel/reason_of_stay.csv")
reasons <- reasons %>%
  mutate_at(2:8, function(x) x/reasons$Total)

travel_short <- travel_short %>% left_join(reasons %>% select(Date, `Visiting friends/relatives`))

travel_res <- travel_res %>%
  bind_rows(travel_short %>%
              mutate_at(2:70, function(x) round(x*travel_short$`Visiting friends/relatives`, 0)) %>%
              select(-`Visiting friends/relatives`) %>%
              mutate(type = "Visiting friends/family"))

travel_short <- travel_short %>%
  mutate_at(2:70, function(x) round(x*(1-travel_short$`Visiting friends/relatives`), 0))

travel <- bind_rows(travel_short, travel_res) %>%
  mutate(Date = my(Date, tz = NULL),
         Month = month(Date),
         Year = year(Date))

travel_states <- read_csv("data/Overseas_travel/travel_states.csv") %>%
  mutate(Date = my(Date, tz = NULL),
         Month = month(Date),
         Year = year(Date))

ports_of_entry <- read_csv("data/Overseas_travel/ports_of_entry.csv") %>%
  st_as_sf(coords = c("X", "Y"), crs = st_crs(aus_SA2))

flights <- read_csv("data/Overseas_travel/flights.csv") %>%
  mutate(Date = my(Date, tz = NULL),
         Month = month(Date),
         Year = year(Date))

# get world map to filter data
worldmap <- geodata::world(path = "data/")
cabi_loc <- sf::read_sf("data/CABI_diaphorina_citri.shp")
cabi_counties <- worldmap[vect(cabi_loc), ]

# annual_residents <- flights %>%
#   filter(`In/Out` == "I",
#          Australian %in% filter(ports_of_entry, Type == "Air")$Port) %>%
#   group_by(Year, Month, Australian, `Port Country`) %>%
#   summarise(AllFlights = sum(AllFlights),
#             MaxSeats = sum(MaxSeats)) %>%
#   mutate(country = case_when(`Port Country` %in% c("Malaysia", "Brunei", "Indonesia", "Philippines", "Thailand", "Singapore", "Vietnam", "East Timor") ~ "SE_Asia",
#                              `Port Country` %in% c("Hong Kong (SAR)", "Japan", "Korea", "Taiwan", "China") ~ "NE_Asia",
#                              `Port Country` %in% c("India", "Sri Lanka") ~ "S_Asia")) %>%
#   drop_na(country) %>%
#   select(-c(`Port Country`, AllFlights)) %>%
#   filter(Year < 2020,
#          !(country == "S_Asia" & Australian %in% c("Adelaide", "Darwin", "Perth"))) %>%
#   group_by(Year, country, Australian) %>%
#   summarise(MaxSeats = sum(MaxSeats)) %>%
#   ungroup() %>%
#   group_by(country, Australian) %>%
#   summarise(seats = mean(MaxSeats)) %>%
#   ungroup() %>%
#   group_by(country) %>%
#   mutate(seats = seats/sum(seats)) %>%
#   ungroup() %>%
#   group_by(country, Australian) %>%
#   summarise(seats = mean(seats)) %>% left_join(travel %>%
#                                                  filter(Year < 2020) %>%
#                                                  gather(country, visitors, -c(Date, Month, Year, type)) %>%
#                                                  filter(country %in% c("NE_Asia_tot", "SE_Asia_tot", "S_Asia_tot")) %>%
#                                                  group_by(Year, type, country) %>%
#                                                  filter(type == "Returning residents") %>%
#                                                  summarise(max_residents = sum(visitors)) %>%
#                                                  ungroup() %>%
#                                                  group_by(country) %>%
#                                                  summarise(residents = mean(max_residents)) %>%
#                                                  mutate(country = str_remove(country, "_tot"))) %>%
#   drop_na() %>%
#   mutate(annual_residents = seats * residents) %>%
#   select(country, Australian, annual_residents) %>%
#   rename(origin = country,
#          port_of_entry = Australian) %>%
#   full_join(tibble(origin = rep(c("NE_Asia", "SE_Asia", "S_Asia"), each = length(as.character(ports_of_entry %>% filter(Type == "Air") %>% pull(Port)))),
#                    port_of_entry = rep(as.character(ports_of_entry %>% filter(Type == "Air") %>% pull(Port)), 3))) %>%
#   mutate(annual_residents = replace_na(annual_residents, 0))

annual_visitors <- flights %>%
  filter(`In/Out` == "I",
         Australian %in% filter(ports_of_entry, Type == "Air")$Port,
         `Port Country` %in% cabi_counties$NAME_0 | `Service Country` %in% cabi_counties$NAME_0) %>%
  group_by(Year, Month, Australian) %>%
  summarise(AllFlights = sum(AllFlights),
            MaxSeats = sum(MaxSeats)) %>%
  select(-AllFlights) %>%
  filter(Year == 2019) %>%
  group_by(Year, Australian) %>%
  summarise(MaxSeats = sum(MaxSeats)) %>%
  ungroup() %>%
  group_by(Australian) %>%
  summarise(seats = mean(MaxSeats)) %>%
  mutate(seats_prop = seats/sum(seats)) %>%
  mutate(visitors = as.numeric(travel %>%
                                 filter(Year == 2019) %>%
                                 gather(country, visitors, -c(Date, Month, Year, type)) %>%
                                 filter(country %in%
                                          c("Bangladesh",
                                            "Brazil",
                                            "Cambodia",
                                            "China",
                                            "Colombia",
                                            "India",
                                            "Indonesia",
                                            "Iran",
                                            "Israel",
                                            "Japan",
                                            "Malaysia",
                                            "Mexico",
                                            "Oceania_other",
                                            "PNG",
                                            "Pakistain",
                                            "SS_Africa_other",
                                            "Taiwan",
                                            "Thailand",
                                            "UAE",
                                            "USA",
                                            "Vietnam")) %>%
                                 group_by(Year, type) %>%
                                 filter(type == "Short-term visitors") %>%
                                 ungroup() %>%
                                 group_by(type) %>%
                                 summarise(visitors = sum(visitors)) %>%
                                 pull(visitors))) %>%
  mutate(annual_visitors = seats_prop * visitors) %>%
  select(Australian, annual_visitors) %>%
  rename(port_of_entry = Australian) %>%
  mutate(annual_visitors = replace_na(annual_visitors, 0))

p_1_1 <- list()
for(i in c("Adelaide", "Brisbane", "Cairns", "Darwin", "Melbourne", "Perth", "Sydney")) {
  passengers <- sum(annual_visitors %>% filter(port_of_entry == i) %>% pull(annual_visitors))
  
  p_1_1[[i]] <- replace_na(as.numeric((as_tibble(aus_SA2_airports)[,which(names(aus_SA2_airports) == i)] * as_tibble(aus_SA2_tourism)$rooms / sum(as_tibble(aus_SA2_airports)[,which(names(aus_SA2_airports) == i)] * as_tibble(aus_SA2_tourism)$rooms, na.rm = T) * passengers)[,1]), 0)
}
p_1_1 <- rowSums(do.call(cbind, p_1_1))

p_1_1 <- range01(replace_na(p_1_1, 0))

## 1.2.1: Returning residents - China ##
passengers <- travel %>%
  filter(Year == 2019) %>%
  gather(country, visitors, -c(Date, Month, Year, type)) %>%
  filter(type == "Returning residents",
         country == "China") %>%
  group_by(Year,type) %>%
  summarise(max_visitors = sum(visitors)) %>%
  pull(max_visitors)

p_1_2_1 <- (as.numeric(aus_SA2_demographics$Chinese) / sum(as.numeric(aus_SA2_demographics$Chinese), na.rm = T)) * passengers

## 1.2.2: Returning residents - India ##
passengers <- travel %>%
  filter(Year == 2019) %>%
  gather(country, visitors, -c(Date, Month, Year, type)) %>%
  filter(type == "Returning residents",
         country == "India") %>%
  group_by(Year,type) %>%
  summarise(max_visitors = sum(visitors)) %>%
  pull(max_visitors)

p_1_2_2 <- (as.numeric(aus_SA2_demographics$Indian) / sum(as.numeric(aus_SA2_demographics$Indian), na.rm = T)) * passengers

## 1.2.3: Returning residents - Sri Lanka ##
passengers <- travel %>%
  filter(Year == 2019) %>%
  gather(country, visitors, -c(Date, Month, Year, type)) %>%
  filter(type == "Returning residents",
         country == "Sri_Lanka") %>%
  group_by(Year,type) %>%
  summarise(max_visitors = sum(visitors)) %>%
  pull(max_visitors)

p_1_2_3 <- (as.numeric(aus_SA2_demographics$Sri_Lankan) / sum(as.numeric(aus_SA2_demographics$Sri_Lankan), na.rm = T)) * passengers

## 1.2.4: Returning residents - Vietnam ##
passengers <- travel %>%
  filter(Year == 2019) %>%
  gather(country, visitors, -c(Date, Month, Year, type)) %>%
  filter(type == "Returning residents",
         country == "Vietnam") %>%
  group_by(Year,type) %>%
  summarise(max_visitors = sum(visitors)) %>%
  pull(max_visitors)

p_1_2_4 <- (as.numeric(aus_SA2_demographics$Vietnamese) / sum(as.numeric(aus_SA2_demographics$Vietnamese), na.rm = T)) * passengers

# sum
p_1_2 <- p_1_2_1 + p_1_2_2 + p_1_2_3 + p_1_2_4

p_1_2 <- range01(replace_na(p_1_2, 0))

## 1.3.1: Visiting friends/family - China ## 
passengers <- travel %>%
  filter(Year == 2019) %>%
  gather(country, visitors, -c(Date, Month, Year, type)) %>%
  filter(type == "Visiting friends/family",
         country == "China") %>%
  group_by(Year,type) %>%
  summarise(max_visitors = sum(visitors)) %>%
  pull(max_visitors)

p_1_3_1 <- (as.numeric(aus_SA2_demographics$Chinese) / sum(as.numeric(aus_SA2_demographics$Chinese), na.rm = T)) * passengers

## 1.3.2: Visiting friends/family - India ##
passengers <- travel %>%
  filter(Year == 2019) %>%
  gather(country, visitors, -c(Date, Month, Year, type)) %>%
  filter(type == "Visiting friends/family",
         country == "India") %>%
  group_by(Year,type) %>%
  summarise(max_visitors = sum(visitors)) %>%
  pull(max_visitors)

p_1_3_2 <- (as.numeric(aus_SA2_demographics$Indian) / sum(as.numeric(aus_SA2_demographics$Indian), na.rm = T)) * passengers

## 1.3.3: Visiting friends/family - Sri Lanka ##
passengers <- travel %>%
  filter(Year == 2019) %>%
  gather(country, visitors, -c(Date, Month, Year, type)) %>%
  filter(type == "Visiting friends/family",
         country == "Sri_Lanka") %>%
  group_by(Year,type) %>%
  summarise(max_visitors = sum(visitors)) %>%
  pull(max_visitors)

p_1_3_3 <- (as.numeric(aus_SA2_demographics$Sri_Lankan) / sum(as.numeric(aus_SA2_demographics$Sri_Lankan), na.rm = T)) * passengers

## 1.3.4: Visiting friends/family - Vietnam ##
passengers <- travel %>%
  filter(Year == 2019) %>%
  gather(country, visitors, -c(Date, Month, Year, type)) %>%
  filter(type == "Visiting friends/family",
         country == "Vietnam") %>%
  group_by(Year,type) %>%
  summarise(max_visitors = sum(visitors)) %>%
  pull(max_visitors)

p_1_3_4 <- (as.numeric(aus_SA2_demographics$Vietnamese) / sum(as.numeric(aus_SA2_demographics$Vietnamese), na.rm = T)) * passengers

# sum
p_1_3 <- p_1_3_1 + p_1_3_2 + p_1_3_3 + p_1_3_4

p_1_3 <- range01(replace_na(p_1_3, 0))

### Pathway 2: International imports ###
commodity_weights <-
  read_csv("data/commodities_per_country_per_state.csv") %>%
  pivot_longer(3:9, names_to = "State", values_to = "AUD") %>%
  drop_na() %>%
  group_by(Commodity) %>%
  summarise(tot_vol = sum(AUD)) %>%
  mutate(sum = sum(tot_vol)) %>%
  mutate(weight = tot_vol/sum) %>%
  select(Commodity, weight)

commodities <-
  read_csv("data/commodities_per_country_per_state.csv") %>%
  pivot_longer(3:9, names_to = "State", values_to = "AUD") %>%
  drop_na() %>%
  group_by(Commodity) %>%
  mutate(sum = sum(AUD)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(AUD_rat = AUD/sum) %>%
  group_by(Commodity, State) %>%
  summarise(rel_vol = sum(AUD_rat))

fpoe <-
  read_csv("data/20190815-australias-first-points-of-entry-for-vessels.csv") %>%
  filter(Include == "yes") %>%
  mutate(State = case_when(State == "TAS" ~ "Tas",
                           State == "VIC" ~ "Vic",
                           T ~ State))

trade_by_port <-
  commodities %>%
  left_join(fpoe) %>%
  ungroup() %>%
  group_by(State, Commodity) %>%
  mutate(rel_vol = rel_vol/n()) %>%
  left_join(commodity_weights) %>%
  ungroup() %>%
  mutate(weight = weight,
         rel_vol = rel_vol * weight) %>%
  ungroup() %>%
  group_by(Port) %>%
  summarise(rel_vol = sum(rel_vol)) %>%
  left_join(fpoe)

p_2 <- list()
for(i in tools::file_path_sans_ext(dir("ports"))) {
  import <- as.numeric(trade_by_port %>% filter(Port == i) %>% pull(rel_vol))
  
  p_2[[i]] <- replace_na(as.numeric(as_tibble(aus_SA2_seaports) %>% pull(i) * import), 0)
}

p_2 <- rowSums(do.call(cbind, p_2))

p_2 <- range01(replace_na(p_2, 0))

### Pathway 3: Wind-assisted dispersal (border proximity) ###

north_prox <- exp((log(0.5)/100)*rast("output_maps/north_prox.tif")/1000) # assuming 50% of wind-dispersed ACP will be within 100 km of border

p_3 <- extract(north_prox %>% project("WGS84"), vect(aus_SA2), fun = mean, na.rm = T)[,2]
p_3 <- range01(replace_na(p_3, 0))

### Pathway 4: Budwood - citrus production areas ###
actm <- read_sf("data/AustralianTreeCropMap.shp") %>%
  st_set_crs(st_crs(aus_SA2)) %>%
  filter(commodity == "citrus")

aus_SA2_citrus <- st_intersects(aus_SA2,
                                actm)

p_4 <- lengths(aus_SA2_citrus)/aus_SA2$AREASQKM21 # density of citrus production areas
p_4 <- range01(replace_na(p_4, 0))

### weights ###
w_1_1 <- 0.25 # Tourists
w_1_2 <- 0.75 # Returning residents
w_1_3 <- 1 # Visiting friends/family
w_2 <- 0.1 # Vessels
w_3 <- 0.05 # Wind
w_4 <- 0.75 # Budwood

total_pest_arrivals <- matrixStats::rowWeightedMeans(cbind(p_1_1, p_1_2, p_1_3, p_2,  p_3, p_4), w = c(w_1_1, w_1_2, w_1_3, w_2, w_3, w_4))

total_pest_arrivals <- range01(replace_na(total_pest_arrivals, 0))

top_pests <- total_pest_arrivals
top_pests[top_pests < quantile(total_pest_arrivals, c(0.9))] <- NA


pests_map <-
  mapview(aus_SA2 %>%
            mutate(Incursion.risk = round(total_pest_arrivals, 3),
                   Tourist.risk = round(p_1_1, 3),
                   Returning.resident.risk = round(p_1_2, 3),
                   Visiting.friends.family.risk = round(p_1_3, 3),
                   Vessel.risk = round(p_2, 3),
                   Natural.dispersal.risk = round(p_3, 3),
                   Budwood.risk = round(p_4, 3)),
          na.color = NA, height = 600, layer.name = "Incursion.risk", map.types = "Esri.WorldStreetMap",
          zcol = "Incursion.risk",
          label = aus_SA2$SA2_NAME21
  )

mapshot(pests_map, url = "pests_map.html")


top_pests_map <-
  mapview(aus_SA2 %>%
            mutate(Incursion.risk = round(top_pests, 3),
                   Tourist.risk = round(p_1_1, 3),
                   Returning.resident.risk = round(p_1_2, 3),
                   Visiting.friends.family.risk = round(p_1_3, 3),
                   Vessel.risk = round(p_2, 3),
                   Natural.dispersal.risk = round(p_3, 3),
                   Budwood.risk = round(p_4, 3)) %>%
            filter(!is.na(Incursion.risk)),
          na.color = NA, height = 600, layer.name = "Incursion.risk", map.types = "Esri.WorldStreetMap",
          zcol = "Incursion.risk", label = (aus_SA2 %>% mutate(Pests = top_pests) %>% filter(!is.na(Pests)))$SA2_NAME21
  )

mapshot(top_pests_map, url = "top_pests_map.html")


#### Abiotic suitability ####

abiotic_suit <- rast("output_maps/pred_au1k_ens.tif") %>%
  project("EPSG:3112")

abiotic_suit[] <- range01(abiotic_suit[])

abiotic_suit <- extract(abiotic_suit %>% project("WGS84"), vect(aus_SA2), fun = mean, na.rm = T)[,2]
abiotic_suit <- replace_na(abiotic_suit, 0)

### ESTABLISHMENT ###

estab_lik <- range01(total_pest_arrivals * abiotic_suit)

top_estab <- estab_lik
top_estab[top_estab < quantile(estab_lik, c(0.9))] <- NA

estab_map <-
  mapview(aus_SA2 %>%
            mutate(Incursion.risk = round(total_pest_arrivals, 3),
                   Abiotic.suitability = round(abiotic_suit, 3),
                   Establishment.likelihood = round(estab_lik, 3)),
          na.color = NA, height = 600, layer.name = "Establishment.likelihood", map.types = "Esri.WorldStreetMap",
          zcol = "Establishment.likelihood",
          label = aus_SA2$SA2_NAME21
  )

mapshot(estab_map, url = "establishment_map.html")

top_estab_map <-
  mapview(aus_SA2 %>%
            mutate(Incursion.risk = round(total_pest_arrivals, 3),
                   Abiotic.suitability = round(abiotic_suit, 3),
                   Establishment.likelihood = round(top_estab, 3)) %>%
            filter(!is.na(Establishment.likelihood)),
          na.color = NA, height = 600, layer.name = "Establishment.likelihood", map.types = "Esri.WorldStreetMap",
          zcol = "Establishment.likelihood",
          label = (aus_SA2 %>% mutate(Pests = top_estab) %>% filter(!is.na(Pests)))$SA2_NAME21
  )

mapshot(top_estab_map, url = "top_establishment_map.html")


# export for shiny
aus_SA2 %>%
  mutate(`Incursion risk` = round(total_pest_arrivals, 3),
         `Tourist pathway` = round(p_1_1, 3),
         `Returning resident pathway` = round(p_1_2, 3),
         `Visiting friends & family pathway` = round(p_1_3, 3),
         `Sea cargo pathway` = round(p_2, 3),
         `Natural dispersal pathway` = round(p_3, 3),
         `Budwood pathway` = round(p_4, 3),
         `Climatic suitability` = round(abiotic_suit, 3),
         `Establishment likelihood` = round(estab_lik, 3)) %>%
  filter(!STE_NAME21 %in% c("Other Territories", "Outside Australia")) %>%
  select(SA2_CODE21, SA2_NAME21, STE_NAME21, SA4_NAME21, `Incursion risk`, `Climatic suitability`, `Establishment likelihood`, `Tourist pathway`, `Returning resident pathway`, `Visiting friends & family pathway`, `Sea cargo pathway`, `Natural dispersal pathway`, `Budwood pathway`) %>%
  rename(State = STE_NAME21,
         Region = SA4_NAME21) %>%
  write_sf("shiny-resources/aus_SA2.gpkg")



clim_rast <- raster::raster("shiny-resources/pred_au1k_ens.tif")

read_sf("shiny-resources/aus_SA2.gpkg") %>%
  mapview(layer.name = "Incursion risk",
          zcol = "Incursion risk",
          map.types = "Esri.WorldStreetMap",
          label = read_sf("shiny-resources/aus_SA2.gpkg")$SA2_NAME21,
          col.regions = viridis::inferno(n = 402)) +
  mapview(read_sf("shiny-resources/roads.gpkg"), layer.name = "Roads", legend = FALSE, label = read_sf("shiny-resources/roads.gpkg")$name, hide = TRUE)
