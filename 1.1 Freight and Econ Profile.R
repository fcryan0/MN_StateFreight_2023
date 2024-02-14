# Freight and Econ Profile Analysis for the MN State Freight Plan 
# Last Updated 2023-12-02 by Chris Ryan


library(tidyverse)
library(sf)
library(tmap)
tmap_mode("view")
library(tigris)
library(tidycensus)
library(censusapi)
library(ggplot2)
library(plotly)
library(scales)
library(ggspatial)
library(arcpullr)
library(base)
library(basemaps)


# Initial Setup -----------------------------------------------------------



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

crs <- 6505
basemaps <- tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery"))

countiesSF <- 
  counties(state = "MN", cb = TRUE, progress_bar = FALSE) %>% 
  #filter(NAME %in% counties) %>% mutate(metro = TRUE) %>% 
  st_transform(crs)

study_boundary <- countiesSF %>% summarize()
study_boundary %>% write_sf("ArcMap/forGIS/StudyBoundary.shp")


basetile <- basemap(
  ext = countiesSF %>% filter(metro = TRUE) %>% st_buffer(5280*1), 
  map_service = "carto", map_type = "light"
)

tmap_mode("plot")
tm_shape(basetile) + tm_rgb()
tmap_mode("view")



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
  counties,
  study_boundary,
  mycols,
  crs,
  basemaps,
  read_sf_zip,
  read_sf_zipM,
  file = "base_data.rdata"
)





# Demographic Analysis ----------------------------------------------------

# Total Population
totPop <- 
  get_acs(
    "block group", variables = c("B01001_001"), output = "tidy", 
    state = "MN", geometry = TRUE, year = 2021
  ) %>%
  st_transform(crs) %>%
  mutate(
    area = st_area(.), 
    areaSM = area %>% units::set_units(mi^2),
    popDensity = estimate/areaSM) %>% 
  filter(as.numeric(areaSM) > 0)

totPop %>% write_sf("ArcMap/forGIS/PopulationDensity.shp")

basemaps +
  tm_shape(totPop %>% filter(estimate>0)) + tm_fill(alpha = 0.5, col = "popDensity", style = "log10_pretty")


popDetail2021 <- 
  get_acs(
    "county subdivision", variables = c("B01001_001"), output = "tidy", 
    state = "MN", geometry = TRUE, year = 2021
  ) %>%
  mutate(estimate2021 = estimate) %>%
  st_transform(crs) %>% 
  mutate(area = st_area(.)) %>% 
  filter(as.numeric(area) > 0)

tm_shape(popDetail2021)+ tm_sf()

popDetail2016 <-
  get_acs(
    "county subdivision", variables = c("B01001_001"), output = "tidy", 
    state = "MN", geometry = TRUE, year = 2016
  ) %>%
  st_transform(crs) %>% 
  mutate(area = st_area(.)) %>% 
  filter(as.numeric(area) > 0)

tm_shape(popDetail2016)+ tm_sf()

# Need to do a spatial join since the GEOIDs changed a bit between 2016 and 2021
popDetail2021Centroid <- popDetail2021 %>% st_point_on_surface()
popDetail2016Centroid <- popDetail2016 %>% st_point_on_surface()

popDetailXwalk <- 
  popDetail2021 %>% st_join(popDetail2016Centroid) %>% 
  select(
    GEOID2016 = GEOID.y,
    GEOID2021 = GEOID.x,
    pop2016 = estimate.y,
    pop2021 = estimate.x
  ) %>% 
  group_by(GEOID2021) %>% 
  summarize(
    pop2016 = sum(pop2016),
    pop2021 = first(pop2021)
  ) 


# Manual Adjustment for some areas that didn't line up well
popDetailXwalk[popDetailXwalk$GEOID2021 == "2705315148", 2] <- 4296
popDetailXwalk[popDetailXwalk$GEOID2021 == "2705315148", 3] <- 4367

popDetailXwalk <- 
  popDetailXwalk %>%
  mutate(
    percChange = (pop2021 - pop2016) / pop2016,
    change = pop2021 - pop2016
  )


popDetailChange <- 
  popDetail2016 %>% 
  left_join(popDetailXwalk %>% st_drop_geometry(), by = c("GEOID" = "GEOID2021"))

popDetailChange %>% write_sf("ArcMap/forGIS/PopulationChange.shp")

# Total Change
basemaps +
  tm_shape(popDetailChange) + tm_fill(col = "change", alpha = 0.8, palette = "Spectral", popup.vars = TRUE, style = "fixed", breaks = c(-5000, -1000, -100, 0, 1000, 5000, 10000, 30000))

# Population Gain (> 2% growth)
basemaps +
  tm_shape(popDetailChange) + # %>% filter(percChange > 0.02)) + 
  tm_fill(col = "change", alpha = 0.8, palette = "Spectral", popup.vars = TRUE, 
          style = "fixed", breaks = c(-5000, -1000, -500, 0, 500, 1000, 25000), 
          title = "Population Gain (2016 - 2021)")

# Population Percent Gain (> 100 pop change growth)
basemaps +
  tm_shape(popDetailChange %>% filter(change > 500)) + 
  tm_fill(col = "percChange", alpha = 0.8, palette = "YlGnBu", popup.vars = TRUE, style = "pretty", title = "Percent Gain (2016 - 2021)")

popDetailChange %>% filter(change > 500) %>% 
  arrange(desc(percChange)) %>% st_drop_geometry()

# Population Loss
basemaps +
  tm_shape(popDetailChange %>% filter(percChange < -0.01)) + 
  tm_fill(col = "change", alpha = 0.8, palette = "Reds", popup.vars = TRUE, style = "jenks")


# Percent Change
basemaps +
  tm_shape(popDetailChange) + tm_fill(col = "percChange", alpha = 0.8, palette = "Spectral", popup.vars = TRUE, style = "fixed", breaks = c(-1, -0.5, -0.10, 0, 0.10, 0.5, 1))



# Total Population Change

pop2021 <- 
  get_acs(
    "county", variables = c("B01001_001"), output = "tidy", 
    state = "MN", geometry = FALSE, year = 2021
  ) %>% summarize(pop2021 = sum(estimate)) %>% pull(pop2021)

pop2016 <- 
  get_acs(
    "county", variables = c("B01001_001"), output = "tidy", 
    state = "MN", geometry = FALSE, year = 2016
  ) %>% summarize(pop2016 = sum(estimate)) %>% pull(pop2016)

pop2021 - pop2016

pop2021 / pop2016





# Highways ----------------------------------------------------------------

# MnDOT Road File
roads <-
  read_sf_zipM("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_dot/trans_roads_centerlines/shp_trans_roads_centerlines.zip")

TH_state <- 
  roads$Trunk_Highways_in_Minnesota.shp %>% st_zm() %>% st_transform(crs)
tm_shape(TH_state) + tm_lines()

TH_state %>% write_sf("ArcMap/forGIS/TH_state.shp")

# MnDOT ADT and HCAADT
adt <- 
   read_sf_zipM("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_dot/trans_aadt_traffic_segments/shp_trans_aadt_traffic_segments.zip") %>%
   .$Annual_Average_Daily_Traffic_Segments_in_Minnesota.shp %>% st_zm() %>% st_transform(crs) %>% st_drop_geometry() %>% select(SEQUENCE_N, adt = CURRENT_VO)

hcaadt <- 
   read_sf_zipM("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_dot/trans_hcaadt_traffic_segments/shp_trans_hcaadt_traffic_segments.zip") %>%
   .$Heavy_Commercial_Annual_Average_Traffic_Segments_in_Minnesota.shp %>% st_zm() %>% st_transform(crs)  %>% left_join(adt) %>% 
  mutate(trucks = CURRENT_VO)
 
hcaadtSum <- 
  hcaadt %>% st_drop_geometry() %>% group_by(ROUTE_LABE) %>% 
  summarize(
    ROUTE_LABE = first(ROUTE_LABE),
    avgTrucks = mean(trucks, na.rm = TRUE),
    avgTruckP = mean(truckP, na.rm = TRUE)
    )  
 
# Calculate Truck Percentage 
hcaadt <- hcaadt %>% mutate(truckP = trucks / adt)
tm_shape(hcaadt) + tm_lines(col = "truckP", lwd = "truckP", scale = 10)
hcaadt %>% write_sf("ArcMap/forGIS/HCAADT.shp")
 
save(
  adt,
  hcaadt,
  hcaadtSum,
  file = "TH and HCAADT.rdata"
  )

hcaadt %>% write_sf("ArcMap/forGIS/HCAADT.shp")




# Download StreetLight Analyses -------------------------------------------

load("C:/Users/frryan/Desktop/_Working Files/MnDOT _Metro Freight Plan/Data Analysis/data/streetlight/SegmentAnalysis.RData")
data_forStl <- 
  read_sf("C:/Users/frryan/Desktop/_Working Files/MnDOT _State Freight Plan/_git_MN_StateFreight_2023/data/streetlight/data_forStl.geojson")



# Calculate FFF; write out shapefile
segAnalysisFFF <- finalResults %>% janitor::clean_names() %>%
  filter(
    day_type == "1: Weekdays (M-Th)",
    day_part == "0: All Day (12am-12am)",
    #line_zone_length_miles >= 0.125
  ) %>%
  transmute(
    zone_id, vehicle_weight,
    stlIndex = as.numeric(average_daily_segment_traffic_st_l_index),
    percRank = percent_rank(stlIndex),
    avgSpd = as.numeric(avg_segment_speed_mph),
    FFF = as.numeric(free_flow_factor)
  ) %>%
  arrange(zone_id) %>% 
  group_by(zone_id) %>% 
  mutate(
    proportion = stlIndex / sum(stlIndex), #this section proportionally combines the heavy and medium vehicles
    avgSpd2 = proportion * avgSpd,
    FFF2 = proportion * FFF 
  ) %>% 
  summarize(
    zone_id = first(zone_id),
    stlIndex = sum(stlIndex),
    avgSpd = sum(avgSpd2, na.rm = TRUE),
    FFF = sum(FFF2)
  ) %>%
  mutate(percRank = percent_rank(stlIndex)) %>% 
  mutate(zone_id = as.numeric(zone_id)) %>% 
  filter(FFF > 0, stlIndex >= 0) %>%
  left_join(data_forStl, by = c("zone_id" = "id")) %>% st_as_sf() %>%
  st_transform(crs) %>% 
  mutate(lengthFeet = as.numeric(st_length(.))) %>%
  mutate(
    delay_index = (1 - FFF) * stlIndex
  )
write_sf(segAnalysisFFF, "ArcMap/forGIS/StlFFF.shp")


#Exploratory Charts
segAnalysisFFF %>% ggplot(aes(x = stlIndex)) + geom_histogram(binwidth = 2000)
#plotly::ggplotly()

segAnalysisFFF %>%
  ggplot(aes(x = percRank, y = FFF)) + geom_bin2d() + coord_cartesian() + 
  scale_fill_gradientn(colors = c(mycols$colAcCream, mycols$colBlue)) + xlab("Truck Volume Percentile") + ylab("Free Flow Factor")
ggsave("Plot/FFF Chart.jpg", height = 4, width = 5, dpi = 500)
plotly::ggplotly()

# Filter out worst cases of FFF (In the top 50th Percentile; bottom 10 percent, road > 0.25 miles)
topFFFlocs <- 
  segAnalysisFFF %>% 
  #filter(lengthMeters > 402) %>% #at least quarter-mile length
  filter(stlIndex > quantile(.$stlIndex, 0.5)) %>% #top 50 percent by volume
  filter(FFF < 0.8 ) %>% #quantile(.$FFF, 0.15)) %>% # lowest 15 percent by FFF
  arrange(FFF) %>%
  # mutate(
  #   name = case_when(
  #     !is.na(name) & !is.na(ref) ~ paste0(name, " (", ref, ")"),
  #     is.na(name) ~ ref,
  #     is.na(ref) ~ name,
  #     TRUE ~ "unknown"
  #   ),
  #   ID = row_number()
  # ) %>%
  select(zone_id, stlIndex, avgSpd, FFF, name, lengthFeet)
topFFFlocs %>% write_sf("ArcMap/forGIS/StlFFF_TopLocs.shp")


topFFFlocs <- 
  segAnalysisFFF %>% filter(delay_index > quantile(.$delay_index, 0.9))

# FFF Map
tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery")) +
  tm_shape(segAnalysisFFF) + tm_lines(col = "FFF", title.col = "Free Flow Factor (FFF)", palette = "-YlOrRd", breaks = c(0, seq(0.5, 1, by = 0.1)), lwd = "stlIndex", scale = 30, popup.vars = TRUE)

tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery")) +
  tm_shape(segAnalysisFFF %>% filter(delay_index > 0)) + tm_lines(col = "delay_index", title.col = "Delay Index", palette = "YlOrRd", lwd = "delay_index", scale = 30, popup.vars = TRUE, style = "log10_pretty")




# Freight Employment and GDP ----------------------------------------------

# BEA Data downloaded from: https://apps.bea.gov/regional/downloadzip.cfm
# Much of the BEA data represents combinations of different industry categories
# This filters to the lowest denominator as well as "Gov" and "All" categories
beaAll <- read_csv("data/CAGDP2__ALL_AREAS_2001_2021.csv") %>%
  mutate(IndustryClassification = case_when( 
    Description == "All industry total" ~ "All",
    Description == "Government and government enterprises" ~ "Gov",
    TRUE ~ IndustryClassification
  )) %>%
  filter(IndustryClassification %in% c("All", "Gov", "11", "21", "22", "23", "31-33", "42", "44-45", "48-49", "51", "52", "53", "54", "55", "56", "61", "62", "71", "72", "81")) %>%
  filter(GeoName %in% c("United States *", "Minnesota")) %>% 
  mutate(
    GEOID = as.character(GeoFIPS),
    across(`2001`:`2021`, ~as.numeric(.)),
    FreightRel = case_when(
      IndustryClassification %in% FreightNAICS ~ TRUE,
      TRUE ~ FALSE
    )) %>%
  pivot_longer(cols = `2001`:`2021`, names_to = "Year", values_to = "Thousands") %>%
  left_join(
    countiesSF %>% st_drop_geometry() %>% select(GEOID, metro)
  ) %>%
  mutate(MissingData = case_when(
    is.na(Thousands) ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  group_by(Description, GeoFIPS) %>% 
  mutate(avg = mean(Thousands, na.rm = TRUE)) %>% 
  ungroup() %>%
  mutate(
    Thousands = case_when(
      MissingData == TRUE ~ avg,
      MissingData == FALSE ~ Thousands)
  ) 

naicsName = beaMN %>% group_by(IndustryClassification, Description) %>% summarize() %>%
  bind_cols(shortname = c(
    "11: Agriculture",
    "21: Mining/Quarrying",
    "22: Utilities",
    "23: Construction",
    "31-33: Manufacturing",
    "42: Wholesale Trade",
    "44-45: Retail Trade",
    "48-49: Transportation",
    "51: Information",
    "52: Finance/Insurance",
    "53: Real Estate",
    "54: Prof. Services",
    "55: Company Management",
    "56: Admin./Support",
    "61: Educational Services",
    "62: Health Care/Soc. Services",
    "71: Arts/Recreation",
    "72: Accomm./Food Services",
    "81: Other Services",
    "All Industry Total",
    "Government"
  )) %>% mutate(NAICS2 = str_sub(shortname, 1, 2))

beaMN <- beaMN %>% left_join(naicsName)

beaMNall <- beaMN %>% filter(IndustryClassification != "All", GeoName == "Minnesota")
beaMNmetro <- beaMN %>% filter(IndustryClassification != "All", metro == TRUE)

#LEHD Analysis

#Download data for Workplace characteristics, All Jobs
#Sum to block group level and sum for All Jobs and Freight NAICS Jobs
mn_wac_AllJobs_2021 <- read_csv("https://lehd.ces.census.gov/data/lodes/LODES8/mn/wac/mn_wac_S000_JT00_2020.csv.gz",
                                col_types = cols(w_geocode = col_character())) %>%
  mutate(BG = str_sub(w_geocode, 1, 12)) %>%
  group_by(BG) %>%
  summarize(
    numBlocks = n(),
    NAICS_All = sum(C000), #perc_All = NAICS_All / NAICS_All,
    NAICS_11 = sum(CNS01), #perc_11 = NAICS_11/NAICS_All,
    NAICS_21 = sum(CNS02), #perc_21 = NAICS_21/NAICS_All,
    NAICS_22 = sum(CNS03), #perc_22 = NAICS_22/NAICS_All,
    NAICS_23 = sum(CNS04), #perc_23 = NAICS_23/NAICS_All,
    NAICS_31 = sum(CNS05), #perc_31 = NAICS_31/NAICS_All,
    NAICS_42 = sum(CNS06), #perc_42 = NAICS_42/NAICS_All,
    NAICS_44 = sum(CNS07), #perc_44 = NAICS_44/NAICS_All,
    NAICS_48 = sum(CNS08), #perc_48 = NAICS_48/NAICS_All,
    NAICS_51 = sum(CNS09),
    NAICS_52 = sum(CNS10),
    NAICS_53 = sum(CNS11),
    NAICS_54 = sum(CNS12),
    NAICS_55 = sum(CNS13),
    NAICS_56 = sum(CNS14),
    NAICS_61 = sum(CNS15),
    NAICS_62 = sum(CNS16),
    NAICS_71 = sum(CNS17),
    NAICS_72 = sum(CNS18), #perc_72 = NAICS_72/NAICS_All,
    NAICS_81 = sum(CNS19),
    NAICS_92 = sum(CNS20),
    NAICS_All = sum(C000),
    FreightEmp = sum(CNS01, CNS02, CNS03, CNS04, CNS05, CNS06, CNS07, CNS08),
    GoodsProdEmp = sum(CNS01, CNS02, CNS04, CNS05),
    FreightProp = FreightEmp / NAICS_All,
    GoodsProp = GoodsProdEmp / NAICS_All
  ) %>%
  mutate(GEOID = str_sub(BG, 1, 5)) %>%
  inner_join(countiesSF %>% select(GEOID, NAME)) %>%
  select(-geometry)

FreightJobsSF <- 
  block_groups("MN", cb = TRUE) %>% 
  left_join(mn_wac_AllJobs_2021, by = c("GEOID" = "BG")) %>% 
  st_transform(crs) %>% 
  mutate(sqmi = st_area(.) %>% units::set_units(mi^2)) %>% 
  mutate(
    FreightEmpDens = FreightEmp / sqmi,
    FreightEmpProp = FreightEmp / NAICS_All
  )

tmap_mode("view")
basemaps +
  tm_shape(FreightJobsSF %>% filter(FreightEmp > 0)) + 
  tm_fill(col = "NAICS_21", style = "fisher", popup.vars = TRUE, alpha = 0.5)

FreightJobsSF %>% write_sf("ArcMap/forGIS/FreightEmp.shp")



# Highway Crash Data ------------------------------------------------------

load("TH and HCAADT.rdata")
crashes <- read_sf("data/crash/2013-2022-CMV-crashes/10yearCMVcrashesforFreightPlan_2023.shp")

tm_shape(crashes) + tm_dots()












