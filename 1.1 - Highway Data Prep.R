# Script for setting up the highway data; specifically the StreetLight Data Setup


library(tidyverse)
library(sf)
library(tmap)
tmap_mode("view")
library(arcpullr)

crs <- 6501 # NAD83 (2011) / Minnesota Central (ftUS)

mn_state_boundary <- tigris::states(cb = TRUE) %>% filter(STUSPS == "MN") %>% st_transform(crs)

# Function Creation ------------------------------------------------------

rename_geometry <- function(g, name){
  current = attr(g, "sf_column")
  names(g)[names(g)==current] = name
  st_geometry(g)=name
  g
}

read_sf_zipLoc <- function(path) {  
  temp <- tempfile()  
  unzip(zipfile = path, exdir = temp)  
  read_sf(temp)  
}
read_sf_zip <- function(path) { 
  temp <- tempfile() 
  temp2 <- tempfile() 
  download.file(path, temp) 
  unzip(zipfile = temp, exdir = temp2) 
  read_sf(temp2) 
}
read_sf_zipM <- function(path) {  
  temp <- tempfile()  
  temp2 <- tempfile()  
  download.file(path, temp)  
  unzip(zipfile = temp, exdir = temp2)  
  
  filenames <- list.files(temp2, recursive = TRUE, pattern = glob2rx("*.shp"), full.names = TRUE) 
  filenamesAbb <- list.files(temp2, recursive = TRUE, pattern = glob2rx("*.shp")) 
  filenum <- length(filenames) 
  
  x <- list() 
  
  for (i in 1:filenum) { 
    x[[i]] <- read_sf(filenames[i]) 
  } 
  
  names(x) <- filenamesAbb 
  
  x 
}
write_sf_zip <- function(SFobject, path, name) {  
  temp <- tempdir()  
  SFobject %>% write_sf(temp, name, driver = "ESRI Shapefile")  
  files <- list.files(temp, pattern = paste0(name, ".*"), full.names = TRUE)  
  zip(zipfile = paste0(getwd(), paste0("/", path, "/"), name), files = files, flags = " a -tzip", zip = "C://Program Files//7-Zip//7Z")  
}

# Highway Data ------------------------------------------------------------

# Download the MnDOT Trunk Highway Centerline Layer
mn_TH <- 
  get_spatial_layer("https://webgis.dot.state.mn.us/65agsf1/rest/services/sdw_trans/ROUTES_TRUNK_HIGHWAY/FeatureServer/0") %>% 
  st_transform(crs)

tm_shape(mn_TH) + tm_lines((col = "ROUTE_LABEL"))

# Download MnDOT National Highway System layer
NHS <- read_sf_zip("https://www.fhwa.dot.gov/planning/national_highway_system/nhs_maps/nhs_20230906.zip")
mn_NHS <- NHS %>% filter(STFIPS == 27) %>% st_transform(crs)

  # Note: some segments that are NHS, but not TH
tm_shape(mn_NHS) + tm_lines(col = "red", lwd = 10) + 
  tm_shape(mn_TH) + tm_lines()

# Download the NHFS Segments
NHFN <- read_sf_zipM("https://ops.fhwa.dot.gov/freight/infrastructure/nfn/shpfiles/nhfn.zip")

CRFC_MN <- 
  NHFN$`NHFNShapefiles/Critical_Rural_Freight_Corridors_11_8_19/NHFN_Critical_Rural_Corridors111219.shp` %>% 
  st_transform(crs) %>% st_zm() %>% st_intersection(mn_state_boundary) %>% mutate(type = "NHFN: CRFC")

CUFC_MN <- 
  NHFN$`NHFNShapefiles/Critical_Urban_Freight_Corridors_11_8_19/NHFN_Critical_Urban_Corridors111219.shp` %>% 
  st_transform(crs) %>% st_zm() %>% st_intersection(mn_state_boundary) %>% mutate(type = "NHFN: CUFC") 

NHFN_NonPHFS <- 
  NHFN$`NHFNShapefiles/Non_Primary_Highway_Freight_System_11_8_19/NHFN_NON_PHFS_111219.shp` %>% 
  st_transform(crs) %>% st_zm() %>% st_intersection(mn_state_boundary) %>% mutate(type = "NHFN: Non-PHFS") 

NHFN_PHFS <- 
  NHFN$`NHFNShapefiles/Primary_Highway_Freight_System_11_8_19/NHFN_PHFS_111219.shp` %>% 
  st_transform(crs) %>% st_zm() %>% st_intersection(mn_state_boundary) %>% mutate(type = "NHFN: PHFS") 

NHFNcombo <- bind_rows(CRFC_MN, CUFC_MN, NHFN_PHFS, NHFN_NonPHFS)

tm_shape(NHFNcombo) + tm_lines(col = "type")

tm_shape(mn_NHS) + tm_lines(col = "red", lwd = 10) + 
  tm_shape(NHFNcombo) + tm_lines()




# Combine all segments into single "Freight-Related" segment layer and combine with StreetLight Segments

load("data/streetlight/mn_streetlight_osm.rdata")

# Combine all potential freight segments into single polygon for filtering
mn_all_freight_roads <- 
  NHFNcombo %>% select(Route_Name, Start_Poin, End_Point) %>% mutate(source = "NHFN") %>% 
  bind_rows(mn_TH %>% select(ROUTE_ID, ROUTE_NAME) %>% mutate(source = "MN TH") %>% rename_geometry("geometry")) %>% 
  bind_rows(mn_NHS %>% select(ID, ROUTEID) %>% mutate(source = "MN NHS")) %>% 
  summarize() %>% 
  st_buffer(100)

tm_shape(mn_all_freight_roads) + tm_polygons()

df_mn_points <- df_mn %>% st_transform(crs) %>% st_cast("POINT")
df_mn_points_int <- df_mn_points %>% st_filter(mn_all_freight_roads)

df_mn_points_summary <- df_mn_points %>% st_drop_geometry() %>% select(zone_name) %>% count(zone_name)
df_mn_points_int_summary <- df_mn_points_int %>% st_drop_geometry() %>% select(zone_name) %>% count(zone_name)

df_mn_zones_within <- 
  df_mn_points_summary %>% 
  left_join(df_mn_points_int_summary, by = "zone_name") %>% 
  filter(n.x == n.y)

stl_segments <- 
  df_mn %>% filter(zone_name %in% df_mn_zones_within$zone_name)

tm_shape(mn_all_freight_roads) + tm_polygons(col = "red", alpha = 0.2) +
  tm_shape(stl_segments) + tm_lines()

save(stl_segments, file = "data/streetlight/stl_segments.RData")


# Upload Zones to StreetLight ---------------------------------------------


library(jsonlite)
library(geojsonsf)
library(httr)
library(stplanr)

# Set StreetLight  key
StlKey <- "mNT5hHj4PNlZzPaTxu6tybFMY7saxOBp"
email <- "chris.ryan@hdrinc.com"

load("data/streetlight/stl_segments.RData")

#### Create Zones ####

# Add properties needed for StL Analysis
data_forStl <- stl_segments %>%
  mutate(
    bearing = line_bearing(.) %>% as.integer(),
    bearing = if_else(bearing <= 0, as.integer(bearing + 360), bearing),
    len_meter = as.numeric(st_length(.))
  ) %>%
  filter(len_meter > 100) %>% 
  transmute(
    rowNum = row_number(),
    id = rowNum, #Set variable as id value
    name = zone_name, #Set variable as name value
    is_pass = 1, #Set features as pass-through (1) or not (0)
    direction = bearing, #Set feature direction or leave as null/NA
    is_bidi = 1 #Set feature as bidirectional
  ) %>% 
  st_transform(4326)

data_forStl <- data_forStl %>% 
  mutate(
    direction = as.integer(direction),
    is_pass = as.integer(is_pass),
    is_bidi = as.integer(is_bidi)
    ) %>% 
  select(-direction, -is_bidi)

tm_shape(data_forStl[5,]) + tm_lines()

data_forStl %>% write_sf("data/streetlight/data_forStl.geojson")


rowTot <- 1000 #Set number of rows per shapefile (Max zones is 1,000)
batches <- nrow(data_forStl) %/% rowTot + 1
i <- 1
# Loop to create StreetLight zones in batches
start <- Sys.time()
for(i in 1:batches) {
  start_batch <- Sys.time()
  filename <- paste0("StateFreightZone_", i, "_of_", batches)
  # Filter out rows based on rowTot setting
  shp <- data_forStl %>%
    filter(
      rowNum <= i * rowTot,
      rowNum > (i-1) * rowTot
    ) 
  
  # Create list to be converted into JSON file
  body <- list(
    insight_login_email = email, #Must include account login email
    geom_type = "line", #polygon or line
    zone_set_name = filename, #Set name of zone set
    zones = jsonlite::fromJSON(geojsonsf::sf_geojson(shp))) #Convert shp to geoJSON, then to list format
  
  # Convert to final JSON file
  bodyJSON <- jsonlite::toJSON(body, auto_unbox = TRUE)
  
  # API call to create Zone Set
  RETRY(
    "POST",
    times = 3,
    url = paste0("https://insight.streetlightdata.com/api/v2/zone_sets", "?key=", StlKey),
    content_type_json(), 
    body = bodyJSON
  )
  
  Sys.sleep(1)  #Max zone creation is 1 per second 
  
  print(i)
  print("Batch Run Time:")
  print(Sys.time() - start_batch)
  print("Total Run Time:")
  print(Sys.time() - start)
}  





# Create StreetLight Analyses ---------------------------------------------

data_forStl <- read_sf("data/streetlight/data_forStl.geojson")

# Segment Analysis

i <- 1
for(i in 2:batches) {
  filename <- paste0("StateFreightZone_", i, "_of_", batches)
  
  bodyA <- list(
    insight_login_email = email,
    analysis_name = paste0(filename, "_SegmentAnalysis"),
    analysis_type = "Segment_Analysis",
    travel_mode_type = "Truck",
    enable_visualization = TRUE, #not including this led to errors, StreetLight team may have addressed the issue
    oz_sets = list(list(name = filename)), #double list required to get square brackets in JSON
    date_ranges = list(list( #double list required to get square brackets in JSON
      start_date = "01/01/2021",
      end_date = "12/31/2021")),
    day_types = "AllDays|17,Weekdays|14,Weekends|67",
    day_parts = "wdAM|0609,wdMid|1015,wdPM|1619,wknd|0620,overnight|2006",
    output_type = "index",
    #segment_types = c("Motorway", "Trunk", "Primary", "Secondary", "Tertiary") #only need for top route
    enable_speed_percentile = TRUE,
    speed_percentile_bins = "5,15,50,85,95"
  )
  
  bodyAJSON <- jsonlite::toJSON(bodyA, auto_unbox = TRUE)
  
  # Call to create Analysis
  RETRY(
    "POST",
    times = 3,
    url = paste0("https://insight.streetlightdata.com/api/v2/analyses", "?key=", StlKey),
    content_type_json(), 
    body = bodyAJSON
  )
  
  Sys.sleep(6) #Max analysis creation is 10 per minute
  
}


# Create Zone Activity Analyses for population characteristics data
rowTot <- 1000 #Set number of rows per shapefile (Max zones is 1,000)
batches <- nrow(data_forStl) %/% rowTot + 1
i <- 1
for(i in 1:batches) {
  filename <- paste0("StateFreightZone_", i, "_of_", batches)
  
  bodyA <- list(
    insight_login_email = email,
    analysis_name = paste0(filename, "_ZA"),
    analysis_type = "Zone_Activity_Analysis",
    travel_mode_type = "All_Vehicles",
    enable_visualization = TRUE, #not including this led to errors, StreetLight team may have addressed the issue
    oz_sets = list(list(name = filename)), #double list required to get square brackets in JSON
    date_ranges = list(list( #double list required to get square brackets in JSON
      start_date = "01/01/2021",
      end_date = "12/31/2021")),
    day_types = "AllDays|17,Weekdays|14,Weekends|67",
    day_parts = "wdAM|0609,wdMid|1015,wdPM|1619,wknd|0620,overnight|2006",
    output_type = "index",
    traveler_attributes = TRUE
    #segment_types = c("Motorway", "Trunk", "Primary", "Secondary", "Tertiary") #only need for top route
    #enable_speed_percentile = TRUE,
    #speed_percentile_bins = "5,15,50,85,95"
  )
  
  bodyAJSON <- jsonlite::toJSON(bodyA, auto_unbox = TRUE)
  
  # Call to create Analysis
  RETRY(
    "POST",
    times = 3,
    url = paste0("https://insight.streetlightdata.com/api/v2/analyses", "?key=", StlKey),
    content_type_json(), 
    body = bodyAJSON
  )
  
  Sys.sleep(6) #Max analysis creation is 10 per minute
  
}

# Download and Process Analyses -------------------------------------------



# Loop to download and combine analysis results

finalResults <- tibble() #Initialize final table
#finalResults$`Trip Proportion` <- 1


for(i in 1:9) {
  filename <- paste0("osmBatch", i, "of9v2")
  
  # Call to download results
  results <- as.data.frame(content(RETRY(
    "GET",
    times = 3,
    url = paste0(
      "https://insight.streetlightdata.com/api/v2/analyses/download/name/",
      filename, "/",
      "sa_comm", #tr_za/od_pers/od_comm/zone_od_pers/zone_od_comm/sa_pers/sa_comm 
      "?key=", StlKey)
  ))) %>%
    mutate(name = filename) %>%
    mutate_all(as.character) #columns were coming in randomly as character or numeric
  #this forces all to be character, to be converted later
  
  # Append and filter to keep data size manageable but keep error messages 
  finalResults <- bind_rows(finalResults, results)#%>%
  #filter(`Trip Proportion` >= 0.05 | is.na(`Trip Proportion`)) 
  
  #if_else(i %% 50 == 0 & i>0, write_csv(finalResults, "finalResults.csv"))
  
  print(i)
  
}

save(finalResults, file = "SegmentAnalysis.RData")
load("SegmentAnalysis.RData")


load("SegmentAnalysis.RData")




load("data/streetlight/SegmentAnalysis.RData")
