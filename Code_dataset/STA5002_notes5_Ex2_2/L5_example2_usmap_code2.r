library(sf)
library(ggplot2)
library(dplyr)
library(quantreg)

county_demo<- read.csv("county_demo_with_fips.csv")

## Education.Bachelor.s.Degree.or.Higher, Income.Per.Capita.Income             
names(county_demo) <- c("FIPS","County","State","Edu","Inc","Pop")  

county_demo1<- county_demo[ county_demo$Pop>= median(county_demo$Pop),]
nrow(county_demo1)

county_demo[!county_demo$FIPS %in% county_demo1$FIPS,]$Inc<-NA

# Remove rows where both Inc and Edu are available for model fitting
fit_data <- county_demo[!is.na(county_demo$Inc) & !is.na(county_demo$Edu), ]

# Fit quantile regression for the median (tau = 0.5)
model <- rq(Inc ~ Edu, data = fit_data, tau = 0.5)

# View the model summary
summary(model)

# Subset rows where Inc is missing but Edu is available
missing_data <- county_demo[is.na(county_demo$Inc) & !is.na(county_demo$Edu), ]

# Predict the median Inc for these rows
predicted_median_Inc <- predict(model, newdata = missing_data)

# Assign the predicted values back to the original dataframe
county_demo$Inc[is.na(county_demo$Inc) & !is.na(county_demo$Edu)] <- 
  predicted_median_Inc

View(county_demo)

county_demo1<-county_demo

median(county_demo$Inc)
quantile(county_demo$Inc, probs = c(0.2, 0.4, 0.6, 0.8), na.rm = TRUE)

# ----------------------------------------
# 1. Data Prep (Only what we need)
# ----------------------------------------

# Ensure FIPS is 5-digit character
county_demo1 <- county_demo1 %>%
  mutate(FIPS = stringr::str_pad(as.character(FIPS), 5, pad = "0"))

# ----------------------------------------
# 2. Load Maps
# ----------------------------------------
# Download county map (simplified)
counties <- tigris::counties(cb = TRUE, resolution = "5m")

# Download state map
states <- tigris::states(cb = TRUE, resolution = "5m")

# Add state FIPS code
counties <- counties %>%
  mutate(state_fips = substr(GEOID, 1, 2))

# Conterminous US (exclude AK=02, HI=15, territories)
conus <- counties %>%
  filter(!state_fips %in% c("02", "15", "60", "66", "69", "72", "78"))

# Alaska
alaska <- counties %>% filter(state_fips == "02")

# Hawaii
hawaii <- counties %>% filter(state_fips == "15")

# Bin income (example)
county_demo1_binned <- county_demo1 %>%
  mutate(
    Inc_Bin = case_when(
      Inc >= 31040 ~ "31,040 or more, 80% or more",
      Inc >= 27638 ~ "27,638 to 31,039, 60% to 80%",
      Inc >= 25356 ~ "25,356 to 27,637, 40% to 60%",
      Inc >= 23356 ~ "23,356 to 25,355, 20% to 40%",
      TRUE ~ "Less than 23,355, less than 20%"
    ),
    Inc_Bin = factor(Inc_Bin, levels = c("31,040 or more, 80% or more", "27,638 to 31,039, 60% to 80%", "25,356 to 27,637, 40% to 60%", "23,356 to 25,355, 20% to 40%", "Less than 23,355, less than 20%"))
  )

# Merge
conus <- conus %>%
  left_join(county_demo1_binned, by = c("GEOID" = "FIPS"))

alaska <- alaska %>%
  left_join(county_demo1_binned, by = c("GEOID" = "FIPS"))

hawaii <- hawaii %>%
  left_join(county_demo1_binned, by = c("GEOID" = "FIPS"))

# Define CRS: EPSG 5070 (Conus Albers)
crs_projected <- 5070

conus_proj <- st_transform(conus, crs = crs_projected)
states_proj <- st_transform(states, crs = crs_projected)

# Alaska: use CRS 3338 (Alaska-specific)
alaska_plot <- st_transform(alaska, crs = 3338)
state_ak <- st_transform(filter(states, GEOID == "02"), crs = 3338)

p_ak <- ggplot() +
  geom_sf(data = alaska_plot, aes(fill = Inc_Bin), color = NA, size = 0) +
  geom_sf(data = state_ak, fill = NA, color = "white", size = 0.3) +
  theme_void() +
  coord_sf(datum = NA, clip = "off") +
  theme(legend.position = "none")

# Hawaii: use same CRS
hawaii_plot <- st_transform(hawaii, crs = 3338)
state_hi <- st_transform(filter(states, GEOID == "15"), crs = 3338)

p_hi <- ggplot() +
  geom_sf(data = hawaii_plot, aes(fill = Inc_Bin), color = NA, size = 0) +
  geom_sf(data = state_hi, fill = NA, color = "white", size = 0.3) +
  theme_void() +
  coord_sf(datum = NA, clip = "off") +
  theme(legend.position = "none")

# Your bbox (from earlier)
bbox <- st_bbox(conus_proj)
# xmin = -2356113.7
# ymin =   268976.5
# xmax =  2258200.2
# ymax =  3172567.9

p_main <- ggplot() +
  geom_sf(data = conus_proj, aes(fill = Inc_Bin), color = NA, size = 0) +
  geom_sf(data = states_proj, fill = NA, color = "white", size = 0.2) +
  scale_fill_manual(
    name = "Per Capita Income",
    # values = c(
    #   "50,000 or more" = "#880000", "#24D2AB"
    #   "40,000 to 49,999" = "#CC0000","#45CAAA"
    #   "35,000 to 39,999" = "#FF4444","#66C2A5"
    #   "30,000 to 34,999" = "#FFAAAA","#87BAA2"
    #   "Less than 30,000" = "#FFDDDD","#DDECE6"
    # ),
    values = c(
      "31,040 or more, 80% or more" = "#027967",
      "27,638 to 31,039, 60% to 80%" = "#009987",
      "25,356 to 27,637, 40% to 60%" = "#00B3A5",
      "23,356 to 25,355, 20% to 40%" = "#81D4CA",
      "Less than 23,355, less than 20%" = "#E8F4F8"
    ),
    na.value = "grey90"
  ) +
  labs(title = "Per Capita Income by County",
       subtitle = "U.S. median income: $26,467") +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray30"),
    plot.margin = margin(1, 1, 1, 1, "cm")  # Add margin for insets
  ) +
  # 🔥 Force full extent
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"]),
    datum = NA
  )
# Convert to grobs
ak_grob <- ggplotGrob(p_ak)
hi_grob <- ggplotGrob(p_hi)

# Final plot
p_final <- p_main +
  annotation_custom(ak_grob, xmin = -3.2e6, xmax = -1.2e6, ymin = 2.7e6, ymax = 4.5e6) +
  annotation_custom(hi_grob, xmin = -2.5e6, xmax = -1.5e6, ymin = 0.2e6, ymax = 1e6) 

# Show
print(p_final)

#ggsave("L5_example2_jpeg", p_final, 960*650, dpi = 150, bg = "white")

