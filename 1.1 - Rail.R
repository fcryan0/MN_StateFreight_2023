# Script for processing segment level rail volumes

library(tidyverse)
library(sf)
library(tmap)
library(tigris)

tmap_mode("view")
proj <- 6501

studyArea <- states(cb = TRUE) %>% filter(STUSPS %in% c("MN") ) %>% st_transform(proj)
#crsuggest::suggest_crs(studyArea)

# Download data (SKIP IF ALREADY DOWNLOADED) -----------------------------------------------------------

# Downlaad from: https://hub.arcgis.com/datasets/fedmaps::north-american-rail-lines-1/about
railLines <- read_sf("https://services2.arcgis.com/FiaPA4ga0iQKduv3/arcgis/rest/services/North_American_Rail_Lines_v1/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
railLinesSA <- railLines %>% filter(STATEAB %in% c("MN")) %>% st_transform(proj)

tm_shape(railLinesSA) + tm_lines()

#railLinesSA <- railLines %>% st_transform(proj) %>% st_intersection(studyArea)


tm_shape(railLinesSA %>% select(NET)) + tm_lines(col = "NET", lwd = 5, popup.vars = TRUE)

# Read in all Current US crossing data
gcisCurrent <- read_csv("C:/Users/frryan/Desktop/_Working Files/_R/FRA/CurrentInventory/PublishedCrossingData-09-30-2023.csv", col_types = cols(MilePost = col_double())) %>%
  filter(
    PosXing == 1, # Filter for at-grade crossings only
    ReasonID != 16, # Filter out "reason for change = closure" (more reliable than "CrossingClosed")
    #TypeXing == 3, # Filter for Private(2)/Public(3)
    #XPurpose == 1 # Filter for Highway(1)/PedPath(2)/PedStation(3)
  ) %>%
  mutate(
    Type = case_when(
      TypeXing == 3 & XPurpose == 1 ~ "PubHwy",
      TypeXing == 2  ~ "Priv",
      TypeXing == 3 & XPurpose  > 1 ~ "PubPed",
    ),
    WarnDev = case_when(
      GateConf == 3 ~ "FourQuad",
      Gates > 0 & Channel %in% 1:4 ~ "GatesWithMedian",
      Gates > 0 ~ "Gates",
      FlashPai > 0 & Gates == 0 ~ "FlashingOnly",
      Gates == 0 & FlashPai == 0 & StopStd > 0 ~ "StopSign",
      Gates == 0 & FlashPai == 0 & StopStd == 0 & XBuck > 0 ~ "Crossbuck",
      TRUE ~ "Unknown"
    ),
    QZ = case_when(
      Whistban == 1 ~ "QZ",
      Whistban == 0 ~ "Horns Sound",
      is.na(Whistban) ~ "Horns Sound"
    )
  )

# Convert to SF, filter for SA
shpXings <- gcisCurrent[!is.na(gcisCurrent$Latitude),] %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(proj) %>% st_intersection(studyArea)

# Read in all Accident/Incident data
file_namesAcc <- dir("C:/Users/frryan/Desktop/_Working Files/_R/FRA/CrossingData", pattern = "^Acc", full.names = TRUE) # List of all files starting with "Acc"
gcisAccHist <- map_dfr(file_namesAcc, ~read_csv(., col_types = cols(.default = "c")))

# Filter for SA Crashes
railCrashSA <- gcisAccHist %>% inner_join(shpXings %>% st_drop_geometry() %>% select(CrossingID), by = c("GXID" = "CrossingID"))

save(shpXings, railLinesSA, railCrashSA, file = "BaseRailData.RData")


# Rail Data Processing ----------------------------------------------------

load("C:/Users/frryan/Desktop/_Working Files/_R/FRA/_Git_FRA_Templates/BaseRailData.RData")

# Crosswalk table between operating RR and RR Owner
railNameLookup <- tibble(
  c("CEDR", "DME",   "PGR", "SOO",  "UP", "CP"),
  c("CN",   "CPRS",  "PGR", "CPRS", "UP", "CPRS"))
colnames(railNameLookup) <- c("CrossingIdSuffix", "RRname")

# Add RR names and calculate daily train volumes (doesn't include switching)
shpXings <- shpXings %>% left_join(railNameLookup) %>% mutate(trainTot = DayThru + NghtThru)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Join crossing data to rail line data to calculate train speed and volumes
railJoin <- st_join(shpXings, railLinesSA, join = st_nearest_feature) %>% st_drop_geometry() %>%
  select(OBJECTID, RROWNER1, trainTot, MaxTtSpd, RrDiv) %>% group_by(OBJECTID) %>% 
  summarize(
    trains = getmode(trainTot),
    speed = getmode(MaxTtSpd)
  )

# Add the speed and volume information to the rail line data for mainlines only
# Then split it and join the unknown data to the known data and recombine
railLinesSA_Join1 <- railLinesSA %>% left_join(railJoin) %>% filter(NET %in% c("M")) %>% select(OBJECTID, trains, speed)
railJoinKnown <- railLinesSA_Join1 %>% filter(!is.na(trains))
railJoinEmpty <- railLinesSA_Join1 %>% filter(is.na(trains))

missingDataJoin <- st_join(railJoinEmpty, railJoinKnown, join = st_nearest_feature) %>% 
  select(OBJECTID = OBJECTID.x, trains = trains.y, speed = speed.y) %>% st_drop_geometry()

railLinesSA_SpdVol <- railLinesSA_Join1 %>% left_join(missingDataJoin, by = "OBJECTID") %>%
  rowwise() %>%
  transmute(
    OBJECTID = OBJECTID,
    trains = max(trains.x, trains.y, na.rm = TRUE),
    speed = max(speed.x, speed.y, na.rm = TRUE)
  )


# Summary Tables ------------------------------------------------------------------

# Grade Crossing Count by Type
XingTable <- shpXings %>% st_drop_geometry() %>% group_by(RRname, Type) %>% summarize(count = n()) %>%
  pivot_wider(names_from = Type, values_from = count, values_fill = 0) %>% select(RRname, PubHwy, PubPed, Priv) %>%
  rename(
    Railroad = RRname,
    `Public Highway Crossing` = PubHwy,
    `Public Pedestiran Crossing` = PubPed,
    `Private` = Priv
  ) %>%
  mutate(`Total Grade Crossings` = sum(c_across(`Public Highway Crossing`:`Private`)))

# Miles of track by RR and track network type; Join with Crossing Count
RailTable <- railLinesSA %>% mutate(lengthMeters = st_length(.)) %>% st_drop_geometry() %>% 
  group_by(RROWNER1, NET) %>% summarize(miles = round(sum(as.numeric(lengthMeters)) / 1609.34, 0)) %>% filter(!is.na(RROWNER1)) %>%
  pivot_wider(names_from = NET, values_from = miles, values_fill = 0) %>%
  mutate(totMiles = sum(c_across(M:Y), na.rm = TRUE)) %>% arrange(desc(totMiles)) %>%
  rename(
    Railroad = RROWNER1,
    Mainline = M,
    'Non-Mainline' = O,
    Industry = I,
    Siding = S,
    Yard = Y,
    `Total Miles` = totMiles
  ) %>%
  left_join(XingTable) %>%
  ungroup()

RailTable <- bind_rows(RailTable, RailTable %>% summarize(across(where(is.numeric), sum)))

RailTable %>% write_csv("RailByOwnerAndType.csv")


# Public Crossing Type Summary
WarnDev <- shpXings %>% st_drop_geometry() %>% filter(Type %in% c("PubHwy", "PubPed")) %>%
  group_by(RRname, WarnDev) %>% summarize(count = n()) %>%
  pivot_wider(names_from = WarnDev, values_from = count, values_fill = 0) %>%
  mutate(Total = sum(c_across(Crossbuck:GatesWithMedian))) %>% arrange(desc(Total)) %>%
  select(
    Railroad = RRname,
    `Gates with Medians` = GatesWithMedian,
    Gates,
    `Flashing Lights Only` = FlashingOnly,
    `Stop Sign` = StopSign,
    Crossbuck,
    Unknown
  )

WarnDev %>% write_csv("CrossingsByType.csv")



# Rail Crash Analysis -----------------------------------------------------

# Summarize crashes for past X years by severity and GXID
railCrashSA_5yr <- railCrashSA %>% mutate(CrashYear = 2000 + as.numeric(YEAR)) %>% filter(CrashYear >= 2016) %>% 
  mutate(severity = case_when(
    as.numeric(TOTKLD) > 0 ~ "Fatal",
    as.numeric(TOTINJ) > 0 ~ "Injury",
    TRUE ~ "PDO"
  )) %>%
  group_by(GXID, severity) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = "severity", values_from = "count", values_fill = 0) %>%
  mutate(Total5yr = sum(c_across(Injury:Fatal)))

xingsCrash <- shpXings %>% inner_join(railCrashSA_5yr, by = c("CrossingID" = "GXID"))

railCrashSA %>% ggplot(aes(x = YEAR)) + geom_histogram(stat = "count")


plotly::ggplotly()



# Mapping -----------------------------------------------------------------

# Map of Ownership and grade crossings
tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery")) +
  tm_shape(railLinesSA) + tm_lines(lwd = 12, col = "RROWNER1", popup.vars = TRUE, palette = "Set1", textNA = "Abandoned", title.col = "Railroad") +
  tm_shape(shpXings %>% filter(Type == "PubHwy")) + tm_dots(col = "orange", popup.vars = TRUE) +
  #tm_shape(SASF) + tm_borders() +
  tm_add_legend(type = "fill", labels = "Public Grade Crossing", col = "orange")


# Map of Speed/Volume and Crashes
tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery")) +
  tm_shape(railLinesSA_SpdVol %>% filter(trains == 0)) + tm_lines(col = "speed", lwd = 2, popup.vars = TRUE, palette = "BuPu", legend.col.show = FALSE) +
  tm_shape(railLinesSA_SpdVol) + tm_lines(col = "speed", lwd = "trains", scale = 20, popup.vars = TRUE, palette = "BuPu", title.col = "Max Train Speed") +
  tm_shape(shpXings) + tm_dots()
#tm_shape(xingsCrash) + tm_dots(col = "orange", popup.vars = TRUE, size = 0.05) +
#tm_shape(xingsCrash %>% filter(Fatal > 0)) + tm_dots(col = "red", size = 0.2)
#  tm_add_legend(type = "fill", labels = c("Crash", "Fatal Crash"), col = c(mycols$colSafeOrange, mycols$colSafeRed))

railLinesSA_SpdVol %>% write_sf("Output/StudyArea_Rail_SpdVol.shp")
shpXings %>% write_sf("Output/StudyArea_Rail_Crossings.shp")
