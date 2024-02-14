# Script to create StreetLight Segment Analysis for the MnDOT D3/D3 Traffic Safety Study
# Chris Ryan
# Last updated: 2023-04-29

library(tidyverse)
library(sf)
library(tmap)
library(jsonlite)
library(geojsonsf)
library(httr)
library(stplanr)
tmap_mode("view")

basemaps <- tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery"))

crs <- 6501 # NAD83 (2011) / Minnesota Central (ft US)

# Function for reading in online zipped shapefiles
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

# Read in MnDOT ATP boundary data
atp <- 
  read_sf_zipM("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_dot/bdry_organization/shp_bdry_organization.zip")
atp34 <- 
  atp$Area_Transportation_Partnership_Boundaries_in_Minnesota.shp %>% 
  filter(ATP_CODE %in% c("3", "4")) %>% st_transform(crs)

    # # Read in OSM data
    # osm_mn <- 
    #   read_sf_zipM("http://download.geofabrik.de/north-america/us/minnesota-latest-free.shp.zip")
    # osm_mn_roads <- 
    #   osm_mn$gis_osm_roads_free_1.shp %>% 
    #   filter(fclass %in% c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified")) %>% 
    #   st_transform(crs) %>% 
    #   st_intersection(atp34 %>% select(ATP_CODE))
    # tally <- osm_mn_roads %>% st_drop_geometry() %>% count(fclass) %>% arrange(desc(n))

# Read in MnDOT Segment data from Sean Tuohey
st_layers("data/D34_Segments.gdb")
segments <- read_sf("data/D34_Segments.gdb") %>% st_zm() %>% st_transform(crs)

tm_shape(segments) + tm_lines()

pivvot_layer <- segments %>% summarize() %>% st_buffer(300)

tm_shape(pivvot_layer) + tm_polygons(alpha = 0.5, col = "red", border.col = "red")

pivvot_layer %>% st_transform(4326) %>% write_sf("pivvot_layer_D3D4.kml")

# Load in StreetLight OSM segments
load("C:/Users/frryan/Desktop/_Working Files/_R/StreetLight/mn_streetlight_osm_all_ways.rdata")

# Saved data since this takes about an hour to run
      # start <- Sys.time()
      segments_buff <- segments %>% select() %>% summarize() %>% st_buffer(200)
      # 
      # df34 <- df %>% 
      #   filter(highway %in% c("motorway", "trunk", "primary", "secondary", "tertiary", "residential", "unclassified")) %>% 
      #   st_transform(crs) %>% 
      #   mutate(length_before = st_length(.)) %>% 
      #   st_filter(segments_buff) %>% 
      #   st_intersection(segments_buff) %>% 
      #   mutate(length_after = st_length(.)) 
      # 
      # save(df34, file = "df34.RData")
load("df34.RData")

start <- Sys.time()
df34_filt <- df34 %>% 
  filter(length_before == length_after, as.numeric(length_after) > 400) %>% 
  mutate(
    length_simp = units::drop_units(length_after), 
    n_segs = ceiling(length_simp/1320)
  ) #%>% slice_head(n = 10)

i <- 3
filter_df34 <- function(i) {
  temp <- df34_filt[i, ] 
  n_segs <- temp$n_segs
  
  temp2 <- temp %>% line_segment(n_segments = n_segs)  
  
  return(temp2)
}

i <- 284
for (i in 1:(nrow(df34_filt)/100 + 1)) {
  temp <- 
    map_dfr(
      ((i-1)*100 + 1):(i*100),
      ~filter_df34(.x)
    )
  
  temp %>% write_sf(paste0("data/Line Split Temp/linesplit_", i, ".geojson"))
}

i <- 285
temp <- 
  map_dfr(
    ((i-1)*100 + 1):nrow(df34_filt),
    ~filter_df34(.x)
  )
temp %>% write_sf(paste0("data/Line Split Temp/linesplit_", i, ".geojson"))

  

# Reconsolidation process

df34_final <- 
  map_df(
    dir("data/Line Split Temp", full.names = TRUE),
    ~read_sf(.x)
  )

df34_final %>% write_sf("data/df34_final.geojson")

# Map the data for comparison
basemaps +
  tm_shape(segments_buff) + tm_polygons(col = "red") +
  tm_shape(df34_final) + tm_lines(lwd = 3)



#### StreetLight Setup ####

# Set StreetLight  key
StlKey <- "mNT5hHj4PNlZzPaTxu6tybFMY7saxOBp"
email <- "chris.ryan@hdrinc.com"


#### Create Zones ####

# Add properties needed for StL Analysis
data_forStl <- df34_final %>%
  transmute(
    rowNum = row_number(),
    id = rowNum, #Set variable as id value
    name = paste0(as.character(segment_id), "_", rowNum), #Set variable as name value
    is_pass = 1, #Set features as pass-through (1) or not (0)
    direction = NA, #Set feature direction or leave as null/NA
  ) %>% 
  st_transform(4326)

rowTot <- 1000 #Set number of rows per shapefile (Max zones is 1,000)
batches <- nrow(data_forStl) %/% rowTot + 1
i <- 1
# Loop to create StreetLight zones in batches
start <- Sys.time()
for(i in 1:batches) {
  start_batch <- Sys.time()
  filename <- paste0("D3D4_Study_", i, "_of_", batches)
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

