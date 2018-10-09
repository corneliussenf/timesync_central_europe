
library(patchwork)
library(tidyverse)
library(raster)

#### Load Hansen data ####

hansen_fc <- list.files("rawdata/hansen/", full.names = TRUE) %>%
  map(raster) %>%
  c(., fun = mean) %>%
  do.call("mosaic", .)

save(hansen_fc, file = "data/hansen_fc.RData")
load(file = "data/hansen_fc.RData")

writeRaster(hansen_fc, "data/hansen_fc.tif")

#### Get disturbance plots ####

dat <- read_csv("data/timesync.csv")

dat_processed <- mutate(dat,
                        agent = dplyr::lead(change_process),
                        post_disturbance_lc = dplyr::lead(dominant_landcover),
                        length = dplyr::lead(image_year) - image_year)

dat_processed_forest <- filter(dat_processed, dominant_landuse == "Forest")

plotid_country_forest <- dat_processed_forest %>%
  group_by(project_code, plotid) %>%
  summarize()

disturbances <- dat_processed %>%
  split(.$project_code) %>%
  map2(.y = plotid_country_forest %>% split(.$project_code),
       ~ filter(.x, plotid %in% .y$plotid)) %>%
  bind_rows() %>%
  filter(agent %in% c("Harvest", "Wind", "Decline", "Fire", "Hydrology", "Debris", "Other")) %>%
  mutate(standreplacing = ifelse(post_disturbance_lc == "Non-tree", "Yes", "No")) %>%
  dplyr::select(country = project_code, plotid, year = image_year, agent, standreplacing, length) %>%
  mutate(year = year + 1)

write_csv(disturbances, "data/disturbances.csv")

#### Extract and compare ####

plots <- read_csv("data/samples.csv")
plots_locations <- as.data.frame(plots[, c("country", "plotid", "xcoord", "ycoord")])
coordinates(plots_locations) <- ~xcoord+ycoord
proj4string(plots_locations) <- CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
plots_locations <- spTransform(plots_locations, CRS = projection(hansen_fc))
plots <- plots %>% mutate(join_id = paste0(country, "_", plotid))

hansen_fc_extract <- raster::extract(hansen_fc, plots_locations)
hansen_fc_extract <- data.frame(fc = hansen_fc_extract, 
                                join_id = paste0(plots_locations$country, "_", plots_locations$plotid))

val_dat <- disturbances %>%
  filter(year %in% 1998:2000 & length <= 3) %>%
  mutate(join_id = paste0(tolower(ifelse(country == "Czech", "Czechia", country)), "_", plotid)) %>%
  left_join(hansen_fc_extract, by = "join_id")

val_dat %>%
  group_by(standreplacing, country) %>%
  summarize(fc_mean = mean(fc, na.rm = TRUE),
           fc_se = sd(fc, na.rm = TRUE) / sqrt(length(fc))) %>%
  na.omit() %>%
  ggplot(., aes(x = country, y = fc_mean, col = standreplacing)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = fc_mean - fc_se, ymax = fc_mean + fc_se), 
                width = 0, position = position_dodge(width = 0.2)) +
  coord_flip()

val_dat %>%
  na.omit() %>%
  ggplot(., aes(x = standreplacing, y = fc)) +
  geom_boxplot() +
  geom_point(position = position_jitter(width = 0.2))

mean_differences <- lapply(1:1000, function(x) {val_dat %>%
    sample_n(., size = nrow(val_dat), replace = TRUE) %>%
    group_by(standreplacing, country) %>%
    summarize(m = mean(fc, na.rm = TRUE)) %>%
    na.omit() %>%
    group_by(country) %>%
    summarize(dif = ifelse(length(m) == 2, abs(diff(m)), NA))}) %>%
  bind_rows(.id = "replica")

p <- mean_differences %>%
  na.omit() %>%
  ggplot(.) +
  geom_violin(aes(x = country, y = dif, fill = country)) +
  coord_flip() +
  labs(x = NULL, y = "Difference in residual forest cover between\nNSR and SR mortality events") +
  ggthemes::theme_few() +
  theme(strip.text = element_text(size = 7),
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 9),
        legend.position = "none") +
  scale_fill_manual(values = c("#4477AA", "#66CCEE", "#228833", "#CCBB44", "#EE6677", "#AA3377")) +
  geom_hline(yintercept = 0, linetype = "dashed")

ggsave("nsr_validation.pdf", p, path = "paper/reporting", width = 3.5, height = 3.5)
