#Data Explore
#May 2026

#Determine what data we have in the database and the underlying format

#Setup Scripts
source("Scripts/00_setup.R")

#See what we have in NatureCounts

collections<-meta_collections()

#BCMMP - British Columbia 
#MMMP  -Maritime
#MMPBIRDS  Great Lakes and Quebec

#Lets start with MMPBIRDS since this has the most data (0.5M records) and existing code
MMP<-nc_data_dl(collections = "MMPBIRDS", username="dethier", info = "Data check for MMP analysis")

#Save a copy so you don't need to download it again
write.csv(MMP, "Output/MMPBIRDS.csv")
dat<-read.csv("Output/MMPBIRDS.csv")

#Summary Stats

survey<-dat %>% select(RouteIdentifier, SiteCode, statprov_code, survey_year, record_id) %>% 
  group_by(RouteIdentifier, statprov_code, survey_year) %>% 
  summarise(nsites = n_distinct(SiteCode))
#Number of Sites per Route varies from 1-12, but should stay relatively constant within a Route over the years

survey2<-dat %>% select(RouteIdentifier, statprov_code, survey_year) %>% distinct() %>% 
  group_by(survey_year, statprov_code) %>% 
  summarise(n_survey = n(), .groups = "drop")
#surveys are complete from 1-28 years

ggplot(survey2,
       aes(x = survey_year,
           y = n_survey,
           colour = statprov_code,
           group = statprov_code)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks = pretty) +
  labs(
    x = "Year",
    y = "Number of surveys",
    colour = "State / Province",
    title = "Number of surveys per year by state/province"
  ) +
  theme_minimal()

#Create Spatial Temporal Map of Survey locations by Year
sf_use_s2(FALSE) 
map<-dat %>% select(SiteCode, statprov_code, latitude, longitude, survey_year) %>% distinct()
map<-map %>% filter(!is.na(latitude))

# 1. Convert your data to sf
pts <- map %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

# 2. Optional: keep only Ontario + Quebec + nearby Great Lakes extent
bbox_pts <- st_bbox(pts)
pts_crop <- st_crop(pts, bbox_pts)

# 3. Get Canada + USA land
countries <- ne_countries(
  scale = "large",
  country = c("Canada", "United States of America"),
  returnclass = "sf"
)

countries_crop <- st_crop(countries, bbox)

# 4. Provinces/states for context
can_prov <- ne_states(country = "Canada", returnclass = "sf") %>%
  filter(name %in% c("Ontario", "Quebec"))

us_states <- ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(name %in% c("Minnesota", "Wisconsin", "Michigan", "Ohio", "Pennsylvania", "New York"))

admin1 <- bind_rows(can_prov, us_states) %>%
  st_crop(bbox)

# 5. Great Lakes polygons
lakes <- ne_download(
  scale = 10,
  type = "lakes",
  category = "physical",
  returnclass = "sf"
)


lakes_crop <- st_crop(lakes, bbox)

# 6. Plot
ggplot() +
  geom_sf(data = countries_crop, fill = "grey92", colour = "grey60", linewidth = 0.2) +
  geom_sf(data = lakes_crop, fill = "lightblue", colour = "lightblue4", linewidth = 0.2) +
  geom_sf(data = admin1, fill = NA, colour = "grey45", linewidth = 0.25) +
  geom_sf(data = pts_crop, aes(colour = factor(survey_year)), size = 0.8, alpha = 0.7) +
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"]),
    expand = FALSE
  ) +
  scale_colour_viridis_d(name = "Survey year", option = "turbo") +
  labs(
    title = "Survey locations in the Ontario–Quebec Great Lakes region",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "aliceblue", colour = NA),
    panel.grid.major = element_line(colour = "white", linewidth = 0.2),
    legend.position = "right"
  )

