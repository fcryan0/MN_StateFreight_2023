library(tidyverse)
library(sf)
library(tmap)
tmap_mode("view")
library(tidycensus)
library(tigris)


fmcsa_census <- read_csv("data/FMCSA_Census/FMCSA_CENSUS1_2023Nov.txt")
glimpse(fmcsa_census)

# See how many entries different physical and mailing addresses when at least one is in MN
check <- 
  fmcsa_census %>% 
  filter(
    PHY_STATE != MAILING_STATE,
    PHY_STATE == "MN" | MAILING_STATE == "MN"
  )

# Filter for MN and other factors
fmcsa_census_mn <- 
  fmcsa_census %>% 
  mutate(update_year = str_sub(MCS150_DATE, -2, -1)) %>%
  filter(
    PHY_STATE %in% c("MN", "ND", "SD", "IA", "WI"),
    PC_FLAG == "N",
    PRIVATE_PASSENGER_BUSINESS == "N",
    PRIVATE_PASSENGER_NONBUSINESS == "N",
    update_year <= 23,
    update_year >= 19,
    NBR_POWER_UNIT > 0,
    DRIVER_TOTAL > 0,
    DRIVER_TOTAL < 10 * NBR_POWER_UNIT,
    NBR_POWER_UNIT < 10 * DRIVER_TOTAL
    ) 
fmcsa_census_mn %>% count(PHY_STATE)

fmcsa_census_mn %>% summarize(trucks = sum(NBR_POWER_UNIT))

fmcsa_census_mn %>% filter(PHY_STATE == "MN") %>% ggplot(aes(x = NBR_POWER_UNIT)) + geom_histogram() + scale_y_log10()

fmcsa_census_mn %>% filter(PHY_STATE == "MN")

# Plot to check driver to truck ratio
fmcsa_census_mn %>% ggplot(aes(x = NBR_POWER_UNIT, y = DRIVER_TOTAL)) + geom_point() + scale_x_log10() + scale_y_log10()
#plotly::ggplotly()


    # fmcsa_census_mn %>% count(AUTHORIZED_FOR_HIRE)
    # fmcsa_census_mn %>% count(EXEMPT_FOR_HIRE)
    # fmcsa_census_mn %>% count(EXEMPT_FOR_HIRE, AUTHORIZED_FOR_HIRE)
    # fmcsa_census_mn %>% count(PRIVATE_PROPERTY)
    # fmcsa_census_mn %>% count(MIGRANT)
    # fmcsa_census_mn %>% count(US_MAIL)
    # fmcsa_census_mn %>% count(FEDERAL_GOVERNMENT)
    # fmcsa_census_mn %>% count(STATE_GOVERNMENT)
    # fmcsa_census_mn %>% count(LOCAL_GOVERNMENT)

zips <- 
  bind_rows(
    zctas(state = "MN", year = 2010),
    zctas(state = "ND", year = 2010),
    zctas(state = "SD", year = 2010),
    zctas(state = "IA", year = 2010),
    zctas(state = "WI", year = 2010)
  )

tm_shape(zips) + tm_dots()

states <- states(cb = TRUE)


#ZIP Code Map of locations
zip_map <- 
  fmcsa_census_mn %>% 
  mutate(ZCTA5CE10 = str_sub(PHY_ZIP, 1, 5)) %>% 
  group_by(ZCTA5CE10) %>% summarize(
    trucks = sum(NBR_POWER_UNIT),
    mileage = sum(MCS150_MILEAGE, na.rm = TRUE)
    ) %>% 
  right_join(zips %>% select(ZCTA5CE10)) %>% st_as_sf()
  
tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery")) +
  tm_shape(zip_map) + tm_fill(col = "trucks", alpha = 0.5, style = "log10_pretty", title = "Registered Trucks", palette = "Reds")

tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery")) +
  tm_shape(zip_map) + tm_dots(col = "trucks", size = "trucks", alpha = 0.5, style = "pretty", title = "Registered Trucks", palette = "-viridis") +
  tm_shape(states) + tm_borders()

tm_basemap(c("CartoDB.Positron", "OpenStreetMap.Mapnik", "Esri.WorldImagery")) +
  tm_shape(zip_map) + tm_fill(col = "mileage", alpha = 0.5, style = "fisher", title = "Estimated VMT for Registered Trucks")







# Review of Carbon Intensity Data -----------------------------------------

carbon_intensity <- 
  readxl::read_excel("data/Figure 7 data.xlsx", sheet = "All Pathways", skip = 3) %>% 
  janitor::clean_names() %>% 
  filter(!fuel_category %in% c("CARBOB", "Diesel", "FT Diesel"))

ci_order <- carbon_intensity %>% count(fuel_category) %>% arrange(desc(n)) %>% 
  mutate(chart_name = paste0(fuel_category, " (", n, ")"))

carbon_intensity <- 
  carbon_intensity %>% 
  left_join(ci_order, by = "fuel_category") %>% 
  mutate(
    ci = as.numeric(current_certified_ci),
    chart_name = factor(chart_name, levels = ci_order$chart_name)
    )


glimpse(carbon_intensity)

carbon_intensity %>% ggplot(aes(x = ci, group = fuel_category)) + 
  geom_density(show.legend = FALSE, fill = mycols$colBlue) + 
  theme_bw() + 
  geom_vline(xintercept = 0) +
  geom_vline(xintercept = 100, col = "red") +
  #xlim(-400,200) +
  scale_y_sqrt() + 
  scale_x_continuous(breaks = c(-400, -300, -200, -100, 0, 100, 200), limits = c(-400, 200)) +
  #scale_fill_gradient(low = "blue", high = "red") +
  facet_grid(rows = vars(chart_name), switch = "y") + 
  theme(
    strip.text.y.left = element_text(angle = 0, hjust = 1),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  xlab("EER-Adjusted Carbon Intensity Value") + ylab(element_blank())

ggsave(filename = "Plot/SustainableTruck/CarbonIntensityChart.jpg", height = 5, width = 6.5)
  
plotly::ggplotly()

                                                                                                  