library(tidyverse)
library(tidycensus)
library(sf)
library(tmap)
tmap_mode("view")
library(censusapi)
library(scales)

load("DataPrep_Misc.rdata")


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
      MissingData == FALSE ~ Thousands),
    area = case_when(
      GeoName == "United State *" ~ "US",
      TRUE ~ "MN"
    )
  ) 

naicsName = beaAll %>% group_by(IndustryClassification, Description) %>% summarize() %>%
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

beaAll <- beaAll %>% left_join(naicsName)



#### Chart of GDP by Industry for MN ####
beaAll %>%
  filter(shortname != "All Industry Total", GeoName != "United States *") %>% 
  ggplot(aes(x = Year, y = Thousands/1000000, 
             fill = factor(FreightRel, levels = c(TRUE, FALSE)))) + 
  geom_col(col = NA) +  
  scale_fill_manual(name = element_blank(),
                    values = c( #assign legend colors
                      mycols$colBlue,
                      mycols$colGreen),
                    labels = c( #assign legend labels
                      "TRUE" = "Freight Related",
                      "FALSE" = "Not Freight Related"
                    )) +
  theme(
    strip.background = element_rect(fill = mycols$colAcBlueGray),
    strip.text = element_text(hjust = 0, size = 9),
    panel.background = element_rect(fill = "gray95"),
    panel.grid.major = element_line(color = "gray80", size = 0.15),
    axis.text.x = element_text(angle = 90, vjust = 0.4, size = 7),
    axis.text.y = element_text(size = 6),
    legend.position = "top",
    legend.text = element_text(size = 8),
  ) +
  
  labs(
    y = "Gross Domestic Product ($Billions)"
  ) +
  facet_wrap(~shortname)

ggsave("Plot/GDP_MN.png", width = 10, height = 6.5, dpi = 300)

#### Chart of GDP by Industry for All US ####
beaAll %>%
  filter(shortname != "All Industry Total", GeoName == "United States *") %>% 
  ggplot(aes(x = Year, y = Thousands/1000000, 
             fill = factor(FreightRel, levels = c(TRUE, FALSE)))) + 
  geom_col(col = NA) +  
  scale_fill_manual(name = element_blank(),
                    values = c( #assign legend colors
                      mycols$colBlue,
                      mycols$colGreen),
                    labels = c( #assign legend labels
                      "TRUE" = "Freight Related",
                      "FALSE" = "Not Freight Related"
                    )) +
  theme(
    strip.background = element_rect(fill = mycols$colAcBlueGray),
    strip.text = element_text(hjust = 0, size = 9),
    panel.background = element_rect(fill = "gray95"),
    panel.grid.major = element_line(color = "gray80", size = 0.15),
    axis.text.x = element_text(angle = 90, vjust = 0.4, size = 7),
    axis.text.y = element_text(size = 6),
    legend.position = "top",
    legend.text = element_text(size = 8),
  ) +
  
  labs(
    y = "Gross Domestic Product ($Billions)"
  ) +
  facet_wrap(~shortname)

ggsave("Plot/GDP_US.png", width = 10, height = 6.5, dpi = 300)


#### Chart of GDP by Industry for MN ####
beaMN_2021 <- beaAll %>% filter(Year == "2021", GeoName == "Minnesota") %>%
  mutate(totGDP = .$Thousands[.$IndustryClassification == "All"],
         percGDPlabel = scales::percent(Thousands / totGDP, accuracy = 0.1),
         percGDP = -Thousands / totGDP,
         area = "Minnesota")

beaUS_2021 <- beaAll %>% filter(Year == "2021", GeoName == "United States *") %>%
  group_by(IndustryClassification, shortname, FreightRel) %>% summarize(Thousands = sum(Thousands, na.rm = TRUE)) %>%
  mutate(totGDP = .$Thousands[.$IndustryClassification == "All"],
         percGDP = case_when(
           is.na(Thousands) ~ 0,
           TRUE ~ Thousands / totGDP
         ),
         percGDPlabel = scales::percent(Thousands / totGDP, accuracy = 0.1),
         area = "United States"
  )

beaCombo <- bind_rows(beaMN_2021, beaUS_2021) %>% filter(shortname != "All Industry Total")

# Percent of GDP by Industry Comparison
ggplot(beaCombo, aes(x = fct_rev(shortname), y = percGDP, 
                     fill = factor(interaction(FreightRel, area),
                                   levels = c(
                                     "TRUE.United States",
                                     "FALSE.United States",
                                     "TRUE.Minnesota",
                                     "FALSE.Minnesota"
                                   )))) + 
  geom_col() +
  coord_flip() +
  scale_fill_manual(name = element_blank(), 
                    values = c( #assign legend colors 
                      "TRUE.Minnesota" = alpha(mycols$colBlue, 1), 
                      "FALSE.Minnesota" = alpha(mycols$colGreen, 1),
                      "TRUE.United States" = alpha(mycols$colBlue, 0.5), 
                      "FALSE.United States" = alpha(mycols$colGreen, 0.5) 
                    ), 
                    labels = c( #assign legend labels 
                      "Freight Related", 
                      "Not Freight Related"
                    ), 
                    breaks = c( #include only the solid color legend items
                      "TRUE.Minnesota",
                      "FALSE.Minnesota"
                    )) +
  geom_text(data = beaCombo %>% filter(percGDP>0), 
            aes(label = percGDPlabel), hjust = 0, vjust = 0.4, nudge_y = 0.005, size = 2) +
  geom_text(data = beaCombo %>% filter(percGDP<=0), 
            aes(label = percGDPlabel), hjust = 1, vjust = 0.4, nudge_y = -0.005, size = 2) +
  geom_text(aes(x = 20, y = 0.08, hjust = 0, label = "United States Industries"), 
            size = 3, fontface = "bold", color = mycols$colDarkGray) +
  geom_text(aes(x = 20, y = -0.08, hjust = 1, label = "Minnesota Industries"), 
            size = 3, fontface = "bold", color = mycols$colDarkGray) +
  labs(
    y = "Percent GDP",
    x = "Industry"
  ) +
  theme(
    legend.position = "top",
    
  ) +
  scale_y_continuous(labels = function(x) scales::percent(abs(x)), limits = c(-0.2, 0.25))

ggsave("Plot/GDP_MNvsUS_2021Breakdown.png", width = 6.5, height = 4, dpi = 300)

#### MN GDP as proportion of US Total
beaMN_2021 %>%
  select(IndustryClassification, shortname, FreightRel, totGDPMN = Thousands) %>% 
  left_join(beaUS_2021 %>% select(shortname, Thousands), by = "shortname") %>% 
  mutate(
    percUS = totGDPMN / Thousands, 
    area = "MN",
    label = round(percUS * 100, 1)) %>% 
  arrange(desc(percUS)) %>% 
  ggplot(aes(x = fct_reorder(shortname, percUS), y = percUS,
             fill = factor(FreightRel, levels = c(TRUE, FALSE)))) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(name = element_blank(), 
                    values = c( #assign legend colors 
                      alpha(mycols$colBlue, 1), 
                      alpha(mycols$colGreen, 1)
                    ), 
                    labels = c( #assign legend labels 
                      "Freight Related", 
                      "Not Freight Related"
                    )
  ) +
  geom_text(aes(label = label), hjust = 0, vjust = 0.4, nudge_y = 0.00005, size = 2) +
  labs(
    y = "Percent of Statewide GDP",
    x = "Industry"
  ) +
  theme(
    legend.position = "top",
    
  ) +
  scale_y_continuous(labels = function(x) scales::percent(abs(x)), limits = c(0, 0.04))


ggsave("Plot/MN_GDP_perc_of_US.png", width = 6.5, height = 4, dpi = 300)





# LEHD --------------------------------------------------------------------

#Download data for Workplace characteristics, All Jobs
#Sum to block group level and sum for All Jobs and Freight NAICS Jobs
mn_wac_AllJobs_2021 <- read_csv("https://lehd.ces.census.gov/data/lodes/LODES8/mn/wac/mn_wac_S000_JT00_2021.csv.gz",
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
  mutate(GEOID = str_sub(BG, 1, 5))


FreightJobsSF <- 
  tigris::block_groups(cb = TRUE, state = "MN") %>% 
  left_join(mn_wac_AllJobs_2021, by = c("GEOID" = "BG")) %>% 
  st_transform(crs) %>% 
  mutate(sqmi = st_area(.) %>% units::set_units(mi^2)) %>%    
  mutate(
    across(numBlocks:GoodsProp, ~replace_na(., 0)),
    FreightEmpDens = FreightEmp / sqmi,
    FreightEmpProp = FreightEmp / NAICS_All
  )

# Calculate Freight EMployment Location Quotients

LQ_stateProps <- 
  FreightJobsSF %>% st_drop_geometry() %>% 
  summarize(across(NAICS_All:GoodsProdEmp, ~sum(.))) %>% 
  pivot_longer(NAICS_11:GoodsProdEmp) %>% 
  mutate(stateEmpProp = value / NAICS_All) %>% 
  select(NAICS = name, stateEmpProp) %>% 
  mutate(NAICS = paste0(NAICS, "_stateLQ")) %>% 
  pivot_wider(names_from = NAICS, values_from = stateEmpProp)
  
FreightEmpLQ_SF <- 
  FreightJobsSF %>% cbind(LQ_stateProps) %>% 
  transmute(
    GEOID, 
    FreightEmp,
    LQ_NAICS_11 = (NAICS_11 / NAICS_All) / NAICS_11_stateLQ,
    LQ_NAICS_21 = (NAICS_21 / NAICS_All) / NAICS_21_stateLQ,
    LQ_NAICS_22 = (NAICS_22 / NAICS_All) / NAICS_22_stateLQ, 
    LQ_NAICS_23 = (NAICS_23 / NAICS_All) / NAICS_23_stateLQ, 
    LQ_NAICS_31 = (NAICS_31 / NAICS_All) / NAICS_31_stateLQ, 
    LQ_NAICS_42 = (NAICS_42 / NAICS_All) / NAICS_42_stateLQ, 
    LQ_NAICS_44 = (NAICS_44 / NAICS_All) / NAICS_44_stateLQ, 
    LQ_NAICS_48 = (NAICS_48 / NAICS_All) / NAICS_48_stateLQ,  
    LQ_NAICS_51 = (NAICS_51 / NAICS_All) / NAICS_51_stateLQ, 
    LQ_NAICS_52 = (NAICS_52 / NAICS_All) / NAICS_52_stateLQ, 
    LQ_NAICS_53 = (NAICS_53 / NAICS_All) / NAICS_53_stateLQ, 
    LQ_NAICS_54 = (NAICS_54 / NAICS_All) / NAICS_54_stateLQ, 
    LQ_NAICS_55 = (NAICS_55 / NAICS_All) / NAICS_55_stateLQ,
    LQ_NAICS_56 = (NAICS_56 / NAICS_All) / NAICS_56_stateLQ,
    LQ_NAICS_61 = (NAICS_61 / NAICS_All) / NAICS_61_stateLQ,
    LQ_NAICS_62 = (NAICS_62 / NAICS_All) / NAICS_62_stateLQ,
    LQ_NAICS_71 = (NAICS_71 / NAICS_All) / NAICS_71_stateLQ,
    LQ_NAICS_72 = (NAICS_72 / NAICS_All) / NAICS_72_stateLQ,
    LQ_NAICS_81 = (NAICS_81 / NAICS_All) / NAICS_81_stateLQ,
    LQ_NAICS_92 = (NAICS_92 / NAICS_All) / NAICS_92_stateLQ, 
    LQ_FreightEmp = (FreightEmp / NAICS_All) / FreightEmp_stateLQ, 
    LQ_GoodsProdEmp  = (GoodsProdEmp / NAICS_All) / GoodsProdEmp_stateLQ,
    )

# basemaps +
#   tm_shape(FreightEmpLQ_SF %>% filter(LQ_FreightEmp > 1, FreightEmp > 50)) + 
#     tm_fill(col = "LQ_FreightEmp", style = "fisher", alpha = 0.5, popup.vars = TRUE)
# 
# basemaps +
#   tm_shape(FreightEmpLQ_SF %>% filter(LQ_NAICS_21 > 1, FreightEmp > 50)) + 
#   tm_fill(col = "LQ_NAICS_21", style = "log10_pretty", alpha = 0.5, popup.vars = TRUE, palette = c("green", "blue", "red"))
# 
# 
# tmap_mode("view")
# basemaps +
#   tm_shape(FreightJobsSF %>% filter(FreightEmp > 0)) + 
#   tm_fill(col = "NAICS_21", style = "fisher", popup.vars = TRUE, alpha = 0.5)



# County Business Pattern Data --------------------------------------------

Sys.setenv(CENSUS_KEY = "f3a45e01dd57821ead6ebefeb8e27670f4b474ea")

# CBP Data Analysis
empData2021 <- getCensus(
  name = "2021/cbp",
  vars = c("COUNTY", "EMP", "EMPSZES", "EMPSZES_LABEL", "ESTAB", "NAICS2017", "YEAR", "PAYANN"),
  region = "county:*", 
  #regionin = "state:27"
  )

# Summarize Employment and Establishment data
empDataMN <- empData2021 %>%
  mutate(
    NAICS = case_when( #Rename the ranged NAICS Codes 
      NAICS2017 == "31-33" ~ "31",
      NAICS2017 == "44-45" ~ "44",
      NAICS2017 == "48-49" ~ "48",
      TRUE ~ NAICS2017),
    len = nchar(NAICS), #Define the length of the NAICS codes
    EMP = as.numeric(EMP),
    ESTAB = as.numeric(ESTAB),
    PAYANN = as.numeric(PAYANN),
    FreightRelated = case_when( #Define whether the NAICS codes are freight or non-freight related
      NAICS %in% FreightNAICS ~ "Freight",
      TRUE ~ "Non-Freight"
    )
  ) %>%
  filter(
    COUNTY %in% countiesSF$COUNTYFP, #Filter to only include target counties
    len <= 2, # Include 2, 3, and 4 digit NAICS codes
    EMPSZES == "001", #Exclude establishment employee size bins, include only "all establishments"
    !NAICS %in% c("00", "99") #Exclude "All Industry" and unknown outliers
  ) %>%
  select(-EMPSZES_LABEL, -EMPSZES) %>%
  group_by(NAICS, YEAR, FreightRelated) %>%
  summarize(emp = sum(EMP), estab = sum(ESTAB), year = first(YEAR), pay = sum(PAYANN)) %>%
  left_join(naicsName, by = c("NAICS" = "NAICS2"))

empDataMN %>% group_by(FreightRelated) %>% summarize(emp = sum(emp), estab = sum(estab), pay = sum(pay))

# Freight-related employment as a proportion of all employment
empDataMN %>% filter(FreightRelated == "Freight") %>% group_by(YEAR) %>% summarize(sum(emp)) %>% pull() / empDataMN %>% group_by(YEAR) %>% summarize(sum(emp)) %>% pull()


# Setup LQ Function

NAICSdesc <- readxl::read_excel("data/2017_NAICS_Descriptions.xlsx") %>% 
  mutate(
    Title = case_when(
      str_sub(Title, -1,-1) == "T" ~ str_sub(Title, 1,-2),
      TRUE ~ Title
    )
  )

naicsDig <- 3

LQfunc <- function(naicsDig) {
  
  naicsLev <- naicsDig
  
  # Download US Employment by X-dig NAICS
  EcSpXdigUS <- getCensus(
    name = "2021/cbp",
    vars = c("EMP", "EMPSZES", "EMPSZES_LABEL", "ESTAB", "NAICS2017", "YEAR"),
    region = "us:*", 
  ) %>% 
    mutate(
      NAICS = case_when( #Rename the ranged NAICS Codes 
        NAICS2017 == "31-33" ~ "31",
        NAICS2017 == "44-45" ~ "44",
        NAICS2017 == "48-49" ~ "48",
        TRUE ~ NAICS2017),
      len = nchar(NAICS), #Define the length of the NAICS codes
      EMP = as.numeric(EMP),
      ESTAB = as.numeric(ESTAB),
      geo = "US"
    ) %>%
    filter(
      EMPSZES == "001",
      NAICS == "00" | len == naicsLev
    ) %>%
    mutate(denom = .$EMP[NAICS == "00"]) %>% select(EMP, NAICS, geo, denom)
  
  
  # Download MN Employment by X-dig NAICS
  EcSpXdigMN <- getCensus(
    name = "2021/cbp",
    vars = c("EMP", "EMPSZES", "EMPSZES_LABEL", "ESTAB", "NAICS2017", "YEAR"),
    region = "county:*",
    regionin = "state:27"
  ) %>% 
    mutate(
      NAICS = case_when( #Rename the ranged NAICS Codes 
        NAICS2017 == "31-33" ~ "31",
        NAICS2017 == "44-45" ~ "44",
        NAICS2017 == "48-49" ~ "48",
        TRUE ~ NAICS2017),
      len = nchar(NAICS), #Define the length of the NAICS codes
      EMP = as.numeric(EMP),
      ESTAB = as.numeric(ESTAB),
      geo = "MN"
    ) %>%
    filter(
      EMPSZES == "001",
      NAICS == "00" | len == naicsLev
    ) %>%
    group_by(NAICS) %>%
    summarize(
      EMP = sum(EMP),
      NAICS = first(NAICS),
      geo = first(geo)
    ) %>%
    mutate(denom = .$EMP[NAICS == "00"]) %>% select(EMP, NAICS, geo, denom)
  
  EcSpXdigMN %>% filter(!NAICS %in% c("00", "99")) %>% 
    left_join(NAICSdesc, by = c("NAICS" = "Code")) %>%
    ggplot(aes(x = reorder(Title, EMP), y = EMP)) + geom_col() +
    coord_flip()
  
  
  LQ <- EcSpXdigMN %>% filter(!NAICS %in% c("00", "99")) %>%   
    left_join(EcSpXdigUS, by = "NAICS") %>%
    left_join(NAICSdesc, by = c("NAICS" = "Code")) %>%
    mutate(
      LQ = (EMP.x / denom.x) / (EMP.y / denom.y),
      FreightRel = case_when(
        str_sub(NAICS, 1, 2) %in% FreightNAICS ~ "Freight",
        TRUE ~ "Non-Freight")
    ) 
  
  return(LQ)
  
}

# 2-Digit NAICS
LQfunc(2) %>% mutate(Title = case_when(
  NAICS == "31" ~ "Manufacturing",
  NAICS == "44" ~ "Retail Trade",
  NAICS == "48" ~ "Transportation",
  TRUE ~ Title
  )
) %>%                                                              
  ggplot(aes(x = reorder(paste0(NAICS, ": ", Title),LQ), y = LQ, fill = FreightRel)) + 
  geom_col() + coord_flip() + scale_x_discrete(labels = label_wrap(60)) + xlab("NAICS Industry") +
  scale_fill_manual(values = c(mycols$colBlue, mycols$colAcTeal)) +
  theme(legend.position = "top", legend.title = element_blank()) +
  geom_hline(yintercept = 1, color = mycols$colGreen, lwd = 1.25) +
  scale_y_log10()

ggsave("Plot/Employment_MN_LQ_2dig.jpg", height = 4, width = 6, dpi = 300)


# 3-Digit NAICS
LQfunc(3) %>% mutate(Title = case_when(
  NAICS == "31" ~ "Manufacturing",
  NAICS == "44" ~ "Retail Trade",
  NAICS == "48" ~ "Transportation",
  TRUE ~ Title
  )
) %>%
  filter(FreightRel == "Freight") %>% 
  arrange(desc(LQ)) %>% head(10) %>% 
  ggplot(aes(x = reorder(paste0(NAICS, ": ", Title),LQ), y = LQ, fill = FreightRel)) + 
  geom_col() + coord_flip() + scale_x_discrete(labels = label_wrap(60)) + xlab("NAICS Industry") +
  scale_fill_manual(values = c(mycols$colBlue, mycols$colAcTeal)) +
  theme(legend.position = "top", legend.title = element_blank()) +
  geom_hline(yintercept = 1, color = mycols$colGreen, lwd = 1.25) +
  scale_y_log10()

ggsave("Plot/Employment_MN_LQ_3digFreightOnly.jpg", height = 4, width = 6, dpi = 300)


# 4-Digit NAICS
LQfunc(4) %>% mutate(Title = case_when(
  NAICS == "31" ~ "Manufacturing",
  NAICS == "44" ~ "Retail Trade",
  NAICS == "48" ~ "Transportation",
  TRUE ~ Title
  )
) %>%
  filter(FreightRel == "Freight") %>% 
  arrange(desc(LQ)) %>% head(10) %>% 
  ggplot(aes(x = reorder(paste0(NAICS, ": ", Title),LQ), y = LQ, fill = FreightRel)) + 
  geom_col() + coord_flip() + scale_x_discrete(labels = label_wrap(60)) + xlab("NAICS Industry") +
  scale_fill_manual(values = c(mycols$colBlue, mycols$colAcTeal)) +
  theme(legend.position = "top", legend.title = element_blank()) +
  geom_hline(yintercept = 1, color = mycols$colGreen, lwd = 1.25) +
  scale_y_log10()

ggsave("Plot/Employment_MN_LQ_4digFreightOnly.jpg", height = 4, width = 6, dpi = 300)

# 6-Digit NAICS
LQfunc(6) %>% mutate(Title = case_when(
  NAICS == "31" ~ "Manufacturing",
  NAICS == "44" ~ "Retail Trade",
  NAICS == "48" ~ "Transportation",
  TRUE ~ Title
)
) %>%
  filter(FreightRel == "Freight") %>% 
  arrange(desc(LQ)) %>% head(10) %>% 
  ggplot(aes(x = reorder(paste0(NAICS, ": ", Title),LQ), y = LQ, fill = FreightRel)) + 
  geom_col() + coord_flip() + scale_x_discrete(labels = label_wrap(60)) + xlab("NAICS Industry") +
  scale_fill_manual(values = c(mycols$colBlue, mycols$colAcTeal)) +
  theme(legend.position = "top", legend.title = element_blank()) +
  geom_hline(yintercept = 1, color = mycols$colGreen, lwd = 1.25) +
  scale_y_log10()

ggsave("Plot/Employment_MN_LQ_6digFreightOnly.jpg", height = 4, width = 6, dpi = 300)


# Download County Level Data for the different NAICS Aggregations

CensusCBP_MN <- getCensus(
  name = "2021/cbp",
  vars = c("EMP", "EMPSZES", "EMPSZES_LABEL", "ESTAB", "NAICS2017", "YEAR"),
  region = "county:*",
  regionin = "state:27"
)

CensusCBP_MN2 <- 
  CensusCBP_MN %>% 
  mutate(
    NAICS = case_when( #Rename the ranged NAICS Codes 
      NAICS2017 == "31-33" ~ "31",
      NAICS2017 == "44-45" ~ "44",
      NAICS2017 == "48-49" ~ "48",
      TRUE ~ NAICS2017),
    len = nchar(NAICS), #Define the length of the NAICS codes
    NAICS2 = str_sub(NAICS, 1, 2),
    EMP = as.numeric(EMP),
    ESTAB = as.numeric(ESTAB),
    geo = "MN"
  ) %>%
  left_join(NAICSdesc, by = c("NAICS" = "Code")) %>%
  filter(
    EMPSZES == "001",
    NAICS == "00" | len == 4 #Ran this once for 3-digit, and once for 4-digit
  ) 
  
tmap_mode("plot")
digit <- "22"
employment_map <- function(digit){
  temp <-  
    CensusCBP_MN2 %>% filter(NAICS2 %in% digit) %>% 
    group_by(NAICS, Title) %>% summarize(empTot = sum(EMP)) %>% 
    arrange(desc(empTot)) 
  
  specialNAICS <- temp %>% head(1) %>% pull(NAICS)
  cat <- temp %>% head(1) %>% pull(Title)
  
  temp %>% 
    ggplot(aes(x = reorder(paste0(NAICS, ": ", Title), empTot), y = empTot/1000)) + 
    geom_col(fill = mycols$colBlue, width = 0.9) + ylab("Employees (Thousands)") + xlab(element_blank()) +
    coord_flip() + scale_x_discrete(labels = label_wrap(60)) +
    theme(legend.position = "top", legend.title = element_blank()) +
    theme_bw()
  ggsave(paste0("Plot/Employment_NAICS2_", specialNAICS, "_barchart.png"), width = 5, height = 0.75 + 0.3*nrow(temp), dpi = 300)

  specialNAICS_SF <- 
    CensusCBP_MN2 %>% filter(NAICS == specialNAICS) %>% 
    left_join(countiesSF, by = c("county" = "COUNTYFP")) %>% st_as_sf()
  
  tm <- 
    tm_shape(MN_ATP) + tm_fill() +
    tm_shape(basetile) + tm_rgb() +
      tm_shape(study_boundary) + tm_borders() + 
      tm_shape(specialNAICS_SF) + tm_polygons(col = "EMP", alpha = 0.5, title = "Industry Employment") +
        tm_text("NAME", scale = 0.6) +
      tm_layout(
        main.title = paste0(specialNAICS, ": ", cat),
        main.title.position = "left",
        main.title.size = 1,
        legend.position = c("right", "center")
      )
  
  tmap_save(tm, filename = paste0("Plot/Employment_NAICS2_", specialNAICS, ".jpg"), height = 7, dpi = 300, outer.margins = 0)
}

map(
  c("11", "21", "22", "23", "31", "32", "33", "42", "44", "44", "45", "48", "49", "72"),
  ~employment_map(.)
)



# Save and Export

save(
  FreightJobsSF,
  FreightEmpLQ_SF,
  CensusCBP_MN,
  file = "DataPrep_EmploymentGDP.rdata"
)

# Export to GIS

FreightJobsSF %>% write_sf("ArcMap/forGIS/FreightEmp.shp")












