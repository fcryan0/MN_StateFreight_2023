library(tidyverse)
library(tidycensus)
library(sf)
library(tmap)
tmap_mode("view")

load("DataPrep_Misc.rdata")

CENSUS_KEY = "f3a45e01dd57821ead6ebefeb8e27670f4b474ea" 

# Population Data Summary (Totl and Equity Measures)

census_variables = c(
  "B01001_001", #Total Population 
  
  #Age over 65 (Uni: Total Pop) 
  "B01001_020", #Male, 65-66 
  "B01001_021", #Male, 67-69 
  "B01001_022", #Male, 70-74 
  "B01001_023", #Male, 75-79 
  "B01001_024", #Male, 80-84 
  "B01001_025", #Male, 85+ 
  "B01001_044", #Female, 65-55 
  "B01001_045", #Female, 67-69 
  "B01001_046", #Female, 70-74 
  "B01001_047", #Female, 75-79 
  "B01001_048", #Female, 80-84 
  "B01001_049", #Female, 85+ 
  
  #Race/Ethnicity (Uni: Total Pop) 
  "B03002_003", #White, not HL 
  "B03002_004", #Black, AA 
  "B03002_014", 
  "B03002_005", #AmInd 
  "B03002_015", 
  "B03002_006", #Asian 
  "B03002_016", 
  "B03002_007", #HawaiianPacIslander 
  "B03002_017", 
  "B03002_008", #Other Alone 
  "B03002_018", 
  "B03002_009", #Two or more 
  "B03002_019", 
  "B03002_012", #HispanicLatino 
  
  #Individual Low-Income (Uni: Total Pop; Technically pop for whom poverty status is determined) 
  "B17021_002", #Low-Income 
  
  #Foreign Born Population (uni: Total Pop) 
  "B05005_001", #Total Foreign Born 
  "B05005_004", #Entered US 2010 or later, Foreign Born 
  
  #Family Poverty Level (Uni: Families) 
  "B17026_001", #Total Families 
  "B17026_002", #Ratio income/Poverty level: <0.5 
  "B17026_003", #Ratio income/Poverty level: 0.5-0.74 
  "B17026_004", #Ratio income/Poverty level: 0.75-0.99 
  "B17026_005", #Ratio income/Poverty level: 1.0-1.24 
  "B17026_006", #Ratio income/Poverty level: 1.25-1.49 
  "B17026_007", #Ratio income/Poverty level: 1.5-1.74 
  "B17026_008", #Ratio income/Poverty level: 1.75-1.84 
  
  #Limited English Proficiency (LEP) (Uni: Pop 5 and over) 
  "B16004_001", #Pop 5 and above 
  "B16004_003", #Speak Only English, 5-17 
  "B16004_025", #Speak Only English, 18-64 
  "B16004_047", #Speak Only English, 65+ 
  "B16004_005",	#Estimate!!Total:!!5 to 17 years:!!Speak Spanish:!!Speak English "very well" 
  "B16004_010",	#Estimate!!Total:!!5 to 17 years:!!Speak other Indo-European languages:!!Speak English "very well" 
  "B16004_015",	#Estimate!!Total:!!5 to 17 years:!!Speak Asian and Pacific Island languages:!!Speak English "very well" 
  "B16004_020",	#Estimate!!Total:!!5 to 17 years:!!Speak other languages:!!Speak English "very well" 
  "B16004_027",	#Estimate!!Total:!!18 to 64 years:!!Speak Spanish:!!Speak English "very well" 
  "B16004_032",	#Estimate!!Total:!!18 to 64 years:!!Speak other Indo-European languages:!!Speak English "very well" 
  "B16004_037",	#Estimate!!Total:!!18 to 64 years:!!Speak Asian and Pacific Island languages:!!Speak English "very well" 
  "B16004_042",	#Estimate!!Total:!!18 to 64 years:!!Speak other languages:!!Speak English "very well" 
  "B16004_049",	#Estimate!!Total:!!65 years and over:!!Speak Spanish:!!Speak English "very well" 
  "B16004_054",	#Estimate!!Total:!!65 years and over:!!Speak other Indo-European languages:!!Speak English "very well" 
  "B16004_059",	#Estimate!!Total:!!65 years and over:!!Speak Asian and Pacific Island languages:!!Speak English "very well" 
  "B16004_064",  #Estimate!!Total:!!65 years and over:!!Speak other languages:!!Speak English "very well" 
  
  #With a Disability (Uni: Civilian Non-Institutionalized Population) 
  "B18101_001", #Civ Non Inst Pop 
  "B18101_004", #Estimate!!Total:!!Male:!!Under 5 years:!!With a disability 
  "B18101_007", #Estimate!!Total:!!Male:!!5 to 17 years:!!With a disability 
  "B18101_010", #Estimate!!Total:!!Male:!!18 to 34 years:!!With a disability 
  "B18101_013", #Estimate!!Total:!!Male:!!35 to 64 years:!!With a disability 
  "B18101_016", #Estimate!!Total:!!Male:!!65 to 74 years:!!With a disability 
  "B18101_019", #Estimate!!Total:!!Male:!!75 years and over:!!With a disability 
  "B18101_023", #Estimate!!Total:!!Female:!!Under 5 years:!!With a disability 
  "B18101_026", #Estimate!!Total:!!Female:!!5 to 17 years:!!With a disability 
  "B18101_029", #Estimate!!Total:!!Female:!!18 to 34 years:!!With a disability 
  "B18101_032", #Estimate!!Total:!!Female:!!35 to 64 years:!!With a disability 
  "B18101_035", #Estimate!!Total:!!Female:!!65 to 74 years:!!With a disability 
  "B18101_038", #Estimate!!Total:!!Female:!!75 years and over:!!With a disability 
  
  #Zero Vehicle HHs (Uni: Occupied Hus) 
  "B25044_001", #Occupied HUs 
  "B25044_003", #Owner occupied, no vehicles 
  "B25044_010", #Renter occupied, no vehicles 
  
  #Means of Transportation to Work (Uni: Workers 16+) 
  "B08006_001", #Total Workers 16+ 
  "B08006_002", #trans to work: Auto 
  "B08006_003", #trans to work: SOV 
  "B08006_008", #trans to work: Transit 
  "B08006_014", #trans to work: Bicycle 
  "B08006_015", #trans to work: Walk 
  "B08006_016", #trans to work: Other 
  "B08006_017"  #trans to work: WFH 
)


get_census <- function(cen_level, census_variables){
  get_acs(
    cen_level, output = "tidy", 
    state = "MN", geometry = TRUE, year = 2021,
    variables = census_variables
  ) %>%
    st_transform(crs) %>%
    select(-moe, -NAME) %>% 
    pivot_wider(names_from = "variable", values_from = "estimate") %>% 
    rowwise() %>% #allows for sums across rows below 
    transmute( #calculate populations for various groups 
      GEOID = GEOID, 
      
      #Total pop based 
      totPop = B01001_001,
      age65over = sum(c_across(B01001_020:B01001_049)),
      minority = totPop - B03002_003, # total - white/non-hispanic
      whiteNH = B03002_003,
      black = B03002_004 + B03002_014, # sum hispanic and non-hispanic together
      amInd = B03002_005 + B03002_015,
      asian = B03002_006 + B03002_016,
      haPacIs = B03002_007 + B03002_017,
      Other = B03002_008 + B03002_018,
      twoOrMore = B03002_009 + B03002_019,
      hispLat = B03002_012,
      lowIncome = B17021_002,
      totForBorn = B05005_001,
      forBornE2010 = B05005_004,
      
      #Family based 
      totFam = B17026_001,  
      pov100 = sum(c_across(B17026_002:B17026_004)), 
      pov185 = sum(c_across(B17026_002:B17026_008)), 
      
      #Pop 5 and above based 
      totPop5above = B16004_001, 
      LEP = totPop5above - sum(c_across(B16004_003:B16004_064)), 
      
      #Civ Non Inst based 
      totCivNonInstPop = B18101_001, 
      disab = sum(c_across(B18101_004:B18101_038)), 
      
      #Occupied HU based 
      occHU = B25044_001,  
      zeroVeh = B25044_003 + B25044_010, 
      
      #Workers 16+ based 
      totWork16 = B08006_001, 
      mttwAuto = B08006_002, 
      mttwSOV = B08006_003, 
      mttwTransit = B08006_008, 
      mttwBike = B08006_014, 
      mttwWalk = B08006_015, 
      mttwOther = B08006_016, 
      mttwWFH = B08006_017  
      
    ) %>%  
  mutate(#recalc all variables as a proportion of total population/families/etc. 
    across(age65over:forBornE2010, ~ round(. / totPop, 4)), 
    across(pov100:pov185, ~ round(. / totFam, 4)), 
    across(LEP, ~ round(. / totPop5above, 4)), 
    across(disab, ~ round(. / totCivNonInstPop, 4)), 
    across(zeroVeh, ~ round(. / occHU, 4)), 
    across(mttwAuto:mttwWFH, ~ round(. / totWork16, 4)) 
  ) %>% 
    ungroup() %>% 
    mutate(
      area = st_area(.), 
      areaSM = round(area %>% units::set_units(mi^2), 2),
      popDensity = round(totPop/areaSM, 0)
    )
}

mn_pop_demo_bg <- get_census("block group", census_variables)
mn_pop_demo_tract <- get_census("tract", census_variables)
mn_pop_demo_countysub <- get_census("county subdivision", census_variables)
mn_pop_demo_county <- get_census("county", census_variables)


save(
  mn_pop_demo_bg,
  mn_pop_demo_tract,
  mn_pop_demo_countysub,
  mn_pop_demo_county,
  file = "DataPrep_Population.rdata"
)



# Export to GIS
mn_pop_demo_bg %>% write_sf("ArcMap/forGIS/PopDemo_bg.shp")
mn_pop_demo_tract %>% write_sf("ArcMap/forGIS/PopDemo_tract.shp")
mn_pop_demo_countysub %>% write_sf("ArcMap/forGIS/PopDemo_countysub.shp")
mn_pop_demo_county %>% write_sf("ArcMap/forGIS/PopDemo_county.shp")


















