# Cost of Climate Change Analysis
# Full paper link: https://www.sciencedirect.com/science/article/pii/S0198971524000747

# Required Packages
install.packages(c("r5r", "sf", "ggplot2", "dplyr", "bit64", "rgdal", "raster", "sp", "moments"))
library(r5r)
library(sf)
library(ggplot2)
library(dplyr)
library(data.table)
library(raster)
library(sp)
library(moments)

# Set Java Memory Allocation
options(java.parameters = "-Xmx30G")

# Paths
data_path <- "C:/path/to/your/data"   # Data folder path
output_path <- "C:/path/to/your/output" # Output folder path
setwd(output_path)

# Section 1: Data Preparation ---------------------------------------------

# Load Origin and Destination Points
origins <- fread(file.path(data_path, "origin_points.csv"))
destinations <- fread(file.path(data_path, "destination_points.csv"))

# Set up Multimodal Transport Network
r5r_core <- setup_r5(data_path, verbose = FALSE)

# Routing Parameters
mode <- c("WALK", "TRANSIT")
departure_datetime <- as.POSIXct("2022-01-24 11:00:00", format = "%Y-%m-%d %H:%M:%S")
max_walk_time <- 17  # minutes
max_trip_duration <- 120  # minutes
time_window <- 30  # minutes

# Generate Detailed Itineraries
itineraries <- detailed_itineraries(
  r5r_core,
  origins = origins,
  destinations = destinations,
  mode = mode,
  departure_datetime = departure_datetime,
  max_walk_time = max_walk_time,
  max_trip_duration = max_trip_duration,
  time_window = time_window,
  shortest_path = TRUE,
  progress = TRUE
)

# Save Results
st_write(itineraries, file.path(output_path, "itineraries.geojson"))

# Section 2: Environmental Health Costs -----------------------------------

# Load Land Surface Temperature (LST) Raster
lst_raster <- raster(file.path(data_path, "LST.tif"))
lst_points <- rasterToPoints(lst_raster, spatial = TRUE) %>% st_as_sf()

# Buffer Walking Segments
buffered_walk <- st_buffer(st_as_sf(itineraries), dist = 30)

# Spatial Join and Summarize LST
temp_joined <- st_join(buffered_walk, lst_points)
temp_summary <- temp_joined %>%
  group_by(id) %>%
  summarise(mean_temperature = mean(value, na.rm = TRUE), .groups = "drop")

### Equation (4): Environmental Health Costs for Walking
walk_tdm <- temp_joined %>%
  mutate(TDM_walk = segment_duration * mean_temperature)

### Equation (5): Environmental Health Costs for Waiting
wait_segments <- itineraries %>% filter(mode != "WALK")
temp_joined_wait <- st_join(st_buffer(st_as_sf(wait_segments), dist = 30), lst_points)
wait_tdm <- temp_joined_wait %>%
  mutate(TDM_wait = wait * mean_temperature)

### Equation (6): Total Environmental Health Costs
combined_tdm <- walk_tdm %>%
  select(from_id, to_id, TDM_walk) %>%
  left_join(wait_tdm %>% select(from_id, to_id, TDM_wait), by = c("from_id", "to_id")) %>%
  mutate(TDM_total = TDM_walk + TDM_wait)

# Section 3: Travel Time Costs -------------------------------------------

### Equation (3): Total Travel Time
travel_time <- itineraries %>%
  group_by(from_id, to_id) %>%
  summarise(
    t_walking = sum(segment_duration[mode == "WALK"]),
    t_waiting = sum(wait),
    t_in_vehicle = sum(segment_duration[mode == "BUS"]),
    total_time = t_walking + t_waiting + t_in_vehicle,
    .groups = "drop"
  )

# Combine Travel Time and Environmental Health Costs
cost_data <- combined_tdm %>%
  left_join(travel_time, by = c("from_id", "to_id"))

# Section 4: Generalized Cost Function ------------------------------------

### Equation (1): Generalized Total Cost

cost_data <- cost_data %>%
  mutate(
    normalized_t = total_time / max(total_time),
    normalized_e = TDM_total / max(TDM_total),
    generalized_cost = (0.5 * normalized_t) + (0.5 * normalized_e)
  )

### Equation (2): Min-Max Normalization

cost_data <- cost_data %>%
  mutate(
    min_max_normalized_cost = (generalized_cost - min(generalized_cost)) / 
      (max(generalized_cost) - min(generalized_cost))
  )

# Section 5: Save Results and Visualize -----------------------------------

# Save Final Cost Data
st_write(cost_data, file.path(output_path, "cost_analysis_results.shp"))

# Visualization
ggplot(cost_data) +
  geom_sf(aes(fill = min_max_normalized_cost)) +
  scale_fill_viridis_c() +
  ggtitle("Normalized Generalized Cost") +
  theme_minimal()
