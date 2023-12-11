library(tidyverse)
library(sf)
library(tmap)


FreightNAICS <- c("11", "21", "22", "23", "31", "32", "33", "31-33", "42", "44", "45", "44-45", "48", "49", "48-49", "72")

# MnDOT Color Schemes
# https://mn.gov/portal/brand/style-guide/colors/
mycols <- list(
  colBlue  = "#003865",
  colGreen = "#78BE21",
  colWhite = "#FFFFFF",
  colBlack = "#000000",
  colAcTeal = "#008EAA",
  colAcGreen = "#0D5257",
  colAcOrange = "#8D3F2B",
  colAcPurple = "#5D295F",
  colAcBlueGray = "#A4BCC2",
  colAcCream = "#F5E1A4",
  colAcSkyBlue = "#9BCBEB",
  colAcGold = "#FFC845",
  colDarkGray = "#53565A",
  colMedGray = "#97999B",
  colLightGray = "#D9D9D6",
  colSafeRed = "#A6192E",
  colSafeOrange = "#E57200"
)

tigris::states(cb = TRUE) %>% filter(STUSPS == "MN") %>% crsuggest::suggest_crs()

crs <- 6501 #NAD83(2011) / Minnesota Central (ftUS)     projected    us-ft  

basemaps <- tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery"))

countiesSF <- 
  tigris::counties(state = "MN", cb = TRUE, progress_bar = FALSE) %>% st_transform(crs)

study_boundary <- countiesSF %>% summarize()

basetile <- basemaps::basemap(
  ext = countiesSF %>% st_buffer(5280*30), 
  map_service = "carto", map_type = "light"
)
tm_shape(countiesSF) + tm_polygons() +
  tm_shape(basetile) + tm_rgb()

tmap_mode("plot")
tm_shape(basetile) + tm_rgb()
tmap_mode("view")

MN_ATP <- 
  arcpullr::get_spatial_layer("https://webgis.dot.state.mn.us/65agsf1/rest/services/sdw_govnt/ATP_DISTRICT/FeatureServer/0") %>% 
  st_transform(crs)

# Define Functions --------------------------------------------------------

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

read_sf_zip <- function(path) { 
  temp <- tempfile() 
  temp2 <- tempfile() 
  download.file(path, temp) 
  unzip(zipfile = temp, exdir = temp2) 
  read_sf(temp2) 
}


save(
  basemaps,
  basetile,
  countiesSF,
  MN_ATP,
  mycols,
  study_boundary,
  crs,
  FreightNAICS,
  read_sf_zip,
  read_sf_zipM,
  file = "DataPrep_Misc.rdata"
)

# Outputs for GIS
study_boundary %>% write_sf("ArcMap/forGIS/StudyBoundary.shp")
countiesSF %>% write_sf("ArcMap/forGIS/MN_Counties.shp")
MN_ATP %>% write_sf("ArcMap/forGIS/MN_ATP.shp")




