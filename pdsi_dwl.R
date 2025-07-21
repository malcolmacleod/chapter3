# clear everything
rm(list = ls(all = TRUE))


# load libraries
library(data.table)
library(janitor)
library(tidyverse)
library(jsonlite)


# 1. Read in PDSI data
# Set the folder path
folder_path <- "data\\pdsi"

# Get a list of all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Read all CSVs and combine them into one data.table
all_pdsi0 <- rbindlist(lapply(csv_files, fread), fill = TRUE)
all_pdsi<-all_pdsi0 %>% clean_names()


# 2. Looking at PDSI to determine 'wet' and 'dry' years

#filtering to just 2015 -2025 to do it again just for then
pdsi_1525<-all_pdsi[year >= 2015 & year <= 2025]

# Compute mean PDSI for each reservoir-year combo
pdsi_summary <- all_pdsi[, .(mean_pdsi = mean(pdsi, na.rm = TRUE)), by = .(res_name, year)]

# Filter to just May and September
may_pdsi<- pdsi_1525[month %in% c(5)]
sep_pdsi<- pdsi_1525[month %in% c(9)]

# Average across May and September for each reservoir and year
may_subset <- may_pdsi[, .(mean_pdsi = mean(pdsi, na.rm = TRUE)), by = .(res_name, year)]
sep_subset <- sep_pdsi[, .(mean_pdsi = mean(pdsi, na.rm = TRUE)), by = .(res_name, year)]

# Get driest 2 years per reservoir
driest_may <- may_subset[order(mean_pdsi), .SD[1], by = res_name]
driest_may[, dry_or_wet := "Dry"]

driest_may %>%
  group_by(year) %>%
  summarise(n_reservoirs = n())

driest_sep <- sep_subset[order(mean_pdsi), .SD[1], by = res_name]
driest_sep[, dry_or_wet := "Dry"]

driest_sep %>%
  group_by(year) %>%
  summarise(n_reservoirs = n())

# Get wettest 2 years per reservoir
wettest_may <- may_subset[order(-mean_pdsi), .SD[1], by = res_name]
wettest_may[, dry_or_wet := "Wet"]

wettest_may %>%
  group_by(year) %>%
  summarise(n_reservoirs = n())

wettest_sep <- sep_subset[order(-mean_pdsi), .SD[1], by = res_name]
wettest_sep[, dry_or_wet := "Wet"]

wettest_sep %>%
  group_by(year) %>%
  summarise(n_reservoirs = n())

# Combine for may and sep
top_pdsi_may <- rbind(driest_may, wettest_may)
top_pdsi_sep <- rbind(driest_sep, wettest_sep)

################################################################################
# After the wettest and driest years were identified, DWL was calculated for May and Sep in GEE
# We calculated the mean DWL for the centroid of each reservoir for 8 different months

# 1. Reading in DWL data
dwl_df<-fread("data\\DWL_by_reservoir_and_month.csv")

dwl_df<-dwl_df0 %>% mutate(year_type = case_when(
  year %in% c(2016, 2019) ~ "wet",
  year %in% c(2022, 2023) ~ "dry",
  TRUE ~ "other")) %>%
  filter(year_type != "other")

# now doing some basic analysis

dwl_summary <- dwl_df %>%
  group_by(lake_id, month, year_type) %>%
  summarize(mean_dwl = mean(meanDWL, na.rm = TRUE), sd_dwl = sd(meanDWL, na.rm = TRUE),
                            n = n())


# t-test did not show any significant difference across all
t.test(mean_dwl ~ year_type, data = dwl_summary %>% filter(month == 5))
t.test(mean_dwl ~ year_type, data = dwl_summary %>% filter(month == 9))

# categorizing by longitude to see differences
# first have to extract the longitude from the GEOJSON
# If .geo is a string of GeoJSON per row, extract coordinates
dwl_coords  <- dwl_df %>%
  mutate(
    lon = map_dbl(.geo, ~ {
      clean_json <- gsub('""', '"', .x)
      clean_json <- gsub('^"|"$', '', clean_json)
      fromJSON(clean_json)$coordinates[1]
    }),
    lat = map_dbl(.geo, ~ {
      clean_json <- gsub('""', '"', .x)
      clean_json <- gsub('^"|"$', '', clean_json)
      fromJSON(clean_json)$coordinates[2]
    })
  )

dwl_lon <-  dwl_coords %>%
  mutate(region = case_when(
    lon < -100 ~ "west",
    lon >= -100 & lon <= -98 ~ "central",
    lon > -98 ~ "east"
  ))

dwl_lon<- dwl_lon %>% 
  mutate(region = factor(region, levels = c("east", "central", "west")))

# plotting to see
ggplot(dwl_lon, aes(x = year_type, y = meanDWL, fill = year_type)) +
  geom_boxplot() +
  facet_wrap(~region) +
  theme_minimal() +
  labs(title = "DWL by Year Type and Longitude Group",
       y = "Mean DWL (mm)", x = "Year Type")


# t-tests within each group
dwl_t<-dwl_lon %>% 
  group_by(region) %>% 
  summarise(
    t_test = list(t.test(meanDWL ~ year_type)),
    .groups = "drop"
  ) %>% 
  mutate(
    p_value = map_dbl(t_test, ~ .x$p.value),
    mean_diff = map_dbl(t_test, ~ diff(.x$estimate))
  )

dwl_t_bymonth<-dwl_lon %>%
  filter(month %in% c("5", "9")) %>%  
  group_by(region, month) %>%
  summarise(
    t_test = list(t.test(meanDWL ~ year_type)),
    .groups = "drop"
  ) %>%
  mutate(
    p_value = map_dbl(t_test, ~ .x$p.value),
    mean_diff = map_dbl(t_test, ~ diff(.x$estimate))
  )