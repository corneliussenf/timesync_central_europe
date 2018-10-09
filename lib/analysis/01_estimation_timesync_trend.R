
# devtools::install_github("corneliussenf/disturbanceBayes")
# The disturbanceBayes model is available under https://zenodo.org/record/1221340

library(disturbanceBayes)
library(tidyverse)
library(raster)
library(rgeos)

#### Get data and wrangel into form ####

dat <- read_csv("data/timesync.csv")

dat_summary <- dat %>%
  split(.$project_code) %>%
  map(~ disturbance_summary(., by.agent = FALSE)) %>%
  bind_rows(.id = "country") %>%
  mutate(country = factor(country, labels = c("Austria", 
                                              "Czechia",
                                              "Germany", 
                                              "Poland", 
                                              "Slovakia", 
                                              "Switzerland"))) %>%
  filter(year != 2017)

#### Check if all year-country combinations exist ####

t <- table(dat_summary$country, dat_summary$year)
k <- which(t == 0, arr.ind = TRUE)
country_missing <- rownames(t)[k[,1]]
year_missing <- colnames(t)[k[,2]]

for (i in 1:length(country_missing)) {
  row_add <- filter(dat_summary, country == country_missing[i])[1,]
  row_add[, "country"] <- country_missing[i]
  row_add[, "year"] <- year_missing[i]
  row_add[, "disturbance"] <- 0
  dat_summary <- rbind(dat_summary, row_add)
}

dat_summary <- dat_summary %>%
  split(.$country) %>%
  map(., ~ arrange(., year)) %>%
  bind_rows() %>%
  mutate(year = as.integer(year))

#### Fit country-wise model using trend component ####

fit_country <- dat_summary %>%
  split(.$country) %>%
  map(., ~ bayes_estimator(x = ., 
                           disturbance_col = "disturbance",
                           total_col = "forest",
                           index_cols = c("year"),
                           p = c(0.025, 0.2, 0.25, 0.5, 0.75, 0.8, 0.975), 
                           model = "binomial",
                           trend = TRUE))


save(fit_country, file = "results/timesync/timesync_model_country_trend.RData")
#load(file = "results/timesync/timesync_model_country_trend.RData")

#### Calculate sampling-weights ####

countries <- shapefile("rawdata/nuts/data/NUTS_RG_01M_2013.shp")
countries <- subset(countries, countries$STAT_LEVL_ == 0)
countries <- spTransform(countries, CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"))
countries <- subset(countries, countries$NUTS_ID %in% c("AT", "CZ", "DE", "CH", "SK", "PL"))
countries$area <- gArea(countries, byid = TRUE)

# Country weights

weights_country <- countries@data %>%
  mutate(weight = area / sum(area)) %>%
  mutate(country = recode(NUTS_ID, 
                          AT = "Austria", 
                          CH = "Switzerland", 
                          CZ = "Czechia",
                          SK = "Slovakia", 
                          PL = "Poland", 
                          DE = "Germany")) %>%
  dplyr::select(country, weight)

# Weights for combining CZ and SK

weights_czsk <- countries@data %>%
  filter(NUTS_ID %in% c("CZ", "SK")) %>%
  mutate(weight = area / sum(area)) %>%
  mutate(country = recode(NUTS_ID, 
                          CZ = "Czechia",
                          SK = "Slovakia")) %>%
  dplyr::select(country, weight)

# Country weights for TimeSync estimation with CZ/SK combined

weights_country_czsk <- countries@data
weights_country_czsk[weights_country_czsk$NUTS_ID == "CZ", "area"] <- weights_country_czsk[weights_country_czsk$NUTS_ID == "CZ", "area"] + 
  weights_country_czsk[weights_country_czsk$NUTS_ID == "SK", "area"]
weights_country_czsk <- weights_country_czsk %>%
  filter(NUTS_ID != "SK") %>%
  mutate(weight = area / sum(area)) %>%
  mutate(country = recode(NUTS_ID, 
                          AT = "Austria", 
                          CH = "Switzerland", 
                          CZ = "Czechia/Slovakia",
                          PL = "Poland", 
                          DE = "Germany")) %>%
  dplyr::select(country, weight)

#### Annual disturbance rate and trend by country ####

country_estimates <- fit_country %>%
  map(., ~ .$posterior %>%
        group_by(year) %>%
        summarise(estimate = median(value),
                  lower = quantile(value, 0.25),
                  upper = quantile(value, 0.75),
                  llower = quantile(value, 0.025),
                  uupper = quantile(value, 0.975))) %>%
  bind_rows(.id = "country") %>%
  mutate(year = as.integer(year))

country_trends <- fit_country %>%
  map(., ~ .$trend_posterior %>%
        group_by(year) %>%
        summarise(estimate = median(value),
                  lower = quantile(value, 0.25),
                  upper = quantile(value, 0.75),
                  llower = quantile(value, 0.025),
                  uupper = quantile(value, 0.975))) %>%
  bind_rows(.id = "country") %>%
  mutate(year = as.integer(year))

#### Mean disturbance rate by country ####

country_estimates_mean <- fit_country %>%
  map(., ~ .$posterior %>%
        group_by(iterations) %>%
        summarize(value = mean(value)) %>%
        ungroup() %>%
        summarise(estimate = median(value),
                  lower = quantile(value, 0.25),
                  upper = quantile(value, 0.75),
                  llower = quantile(value, 0.025),
                  uupper = quantile(value, 0.975))) %>%
  bind_rows(.id = "country")

#### Aggregate Czechia + Slovakia ####

country_estimate_czsk <- fit_country %>%
  map(., ~ .$posterior) %>%
  bind_rows(.id = "country") %>%
  filter(country %in% c("Czechia", "Slovakia")) %>%
  left_join(weights_czsk, by = "country") %>%
  group_by(iterations, year) %>%
  summarize(value = sum(value * weight)) %>%
  mutate(country = "Czechia/Slovakia")

country_estimate_czsk <- fit_country %>%
  map(., ~ .$posterior) %>%
  bind_rows(.id = "country") %>%
  filter(!country %in% c("Czechia", "Slovakia")) %>%
  bind_rows(country_estimate_czsk) %>%
        group_by(country, year) %>%
        summarise(estimate = median(value),
                  lower = quantile(value, 0.25),
                  upper = quantile(value, 0.75),
                  llower = quantile(value, 0.025),
                  uupper = quantile(value, 0.975)) %>%
  mutate(year = as.integer(year)) %>%
  ungroup()

country_trend_czsk <- fit_country %>%
  map(., ~ .$trend_posterior) %>%
  bind_rows(.id = "country") %>%
  filter(country %in% c("Czechia", "Slovakia")) %>%
  left_join(weights_czsk, by = "country") %>%
  group_by(iterations, year) %>%
  summarize(value = sum(value * weight)) %>%
  mutate(country = "Czechia/Slovakia")

country_trend_czsk <- fit_country %>%
  map(., ~ .$trend_posterior) %>%
  bind_rows(.id = "country") %>%
  filter(!country %in% c("Czechia", "Slovakia")) %>%
  bind_rows(country_trend_czsk) %>%
  group_by(country, year) %>%
  summarise(estimate = median(value),
            lower = quantile(value, 0.25),
            upper = quantile(value, 0.75),
            llower = quantile(value, 0.025),
            uupper = quantile(value, 0.975)) %>%
  mutate(year = as.integer(year)) %>%
  ungroup()

country_estimate_mean_czsk <- fit_country %>%
  map(., ~ .$posterior) %>%
  bind_rows(.id = "country") %>%
  filter(country %in% c("Czechia", "Slovakia")) %>%
  left_join(weights_czsk, by = "country") %>%
  group_by(iterations, year) %>%
  summarize(value = sum(value * weight)) %>%
  mutate(country = "Czechia/Slovakia")

country_estimate_mean_czsk <- fit_country %>%
  map(., ~ .$posterior) %>%
  bind_rows(.id = "country") %>%
  filter(!country %in% c("Czechia", "Slovakia")) %>%
  bind_rows(country_estimate_mean_czsk) %>%
  group_by(iterations, country) %>%
  summarize(value = mean(value)) %>%
  group_by(country) %>%
  summarise(estimate = median(value),
            lower = quantile(value, 0.25),
            upper = quantile(value, 0.75),
            llower = quantile(value, 0.025),
            uupper = quantile(value, 0.975))

#### Annual disturbance rate and trend for europe ####

europe_estimate <- fit_country %>%
  map(., ~ .$posterior) %>%
  bind_rows(.id = "country") %>%
  left_join(weights_country, by = "country") %>%
  group_by(iterations, year) %>%
  summarize(est = sum(value * weight)) %>%
  group_by(year) %>%
  summarize(estimate = median(est),
            lower = quantile(est, 0.25),
            llower = quantile(est, 0.025),
            upper = quantile(est, 0.75),
            uupper = quantile(est, 0.975)) %>%
  mutate(year = as.integer(year)) %>%
  mutate(country = "All")

europe_trend <- fit_country %>%
  map(., ~ .$trend_posterior) %>%
  bind_rows(.id = "country") %>%
  left_join(weights_country, by = "country") %>%
  group_by(iterations, year) %>%
  summarize(est = sum(value * weight)) %>%
  group_by(year) %>%
  summarize(estimate = median(est),
            lower = quantile(est, 0.25),
            llower = quantile(est, 0.025),
            upper = quantile(est, 0.75),
            uupper = quantile(est, 0.975)) %>%
  mutate(year = as.integer(year)) %>%
  mutate(country = "All")

#### Mean annual disturbance rate for Central Europe ####

europe_estimate_mean <- fit_country %>%
  map(., ~ .$posterior) %>%
  bind_rows(.id = "country") %>%
  left_join(weights_country, by = "country") %>%
  group_by(iterations, year) %>%
  summarize(value = sum(value * weight)) %>%
  group_by(iterations) %>%
  summarize(value = mean(value)) %>%
  mutate(country = "All") %>%
  group_by(country) %>%
  summarise(estimate = median(value),
            lower = quantile(value, 0.25),
            upper = quantile(value, 0.75),
            llower = quantile(value, 0.025),
            uupper = quantile(value, 0.975))

#### Evidence of increasing forest mortality ####

trend_evidence_country <- fit_country %>%
  map(., ~ data_frame(iterations = 1:length(.$trend),
                      trend = .$trend)) %>%
  bind_rows(.id = "country") %>%
  group_by(country) %>%
  summarize(evidence = mean(trend > 0),
            estimate = median(trend),
            lower = quantile(trend, 0.25),
            upper = quantile(trend, 0.75),
            llower = quantile(trend, 0.025),
            uupper = quantile(trend, 0.975))

trend_evidence_country_czsk <- fit_country %>%
  map(., ~ data_frame(iterations = 1:length(.$trend),
                      trend = .$trend)) %>%
  bind_rows(.id = "country") %>%
  filter(country %in% c("Czechia", "Slovakia")) %>%
  left_join(weights_czsk, by = "country") %>%
  group_by(iterations) %>%
  summarize(trend = sum(trend * weight)) %>%
  mutate(country = "Czechia/Slovakia")

trend_evidence_country_czsk <- fit_country %>%
  map(., ~ data_frame(iterations = 1:length(.$trend),
                      trend = .$trend)) %>%
  bind_rows(.id = "country") %>%
  filter(!country %in% c("Czechia", "Slovakia")) %>%
  bind_rows(trend_evidence_country_czsk) %>%
  group_by(country) %>%
  summarize(evidence = mean(trend > 0),
            estimate = median(trend),
            lower = quantile(trend, 0.25),
            upper = quantile(trend, 0.75),
            llower = quantile(trend, 0.025),
            uupper = quantile(trend, 0.975))

trend_evidence_europe <- fit_country %>%
  map(., ~ data_frame(iterations = 1:length(.$trend),
                      trend = .$trend)) %>%
  bind_rows(.id = "country") %>%
  left_join(weights_country, by = "country") %>%
  group_by(iterations) %>%
  summarize(trend = sum(trend * weight)) %>%
  ungroup() %>%
  summarize(evidence = mean(trend > 0),
            estimate = median(trend),
            lower = quantile(trend, 0.25),
            upper = quantile(trend, 0.75),
            llower = quantile(trend, 0.025),
            uupper = quantile(trend, 0.975)) %>%
  mutate(country = "All")

#### Deviance from trend (europea level) ####

posterior_europe <- fit_country %>%
  map(., ~ .$posterior) %>%
  bind_rows(.id = "country") %>%
  left_join(weights_country, by = "country") %>%
  group_by(iterations, year) %>%
  summarize(estimate = sum(value * weight)) %>%
  ungroup()

posterior_trend <- fit_country %>%
  map(., ~ .$trend_posterior) %>%
  bind_rows(.id = "country") %>%
  left_join(weights_country, by = "country") %>%
  group_by(iterations, year) %>%
  summarize(trend = sum(value * weight)) %>%
  ungroup()

deviance <- posterior_europe %>%
  left_join(posterior_trend, by = c("year", "iterations")) %>%
  mutate(deviance = estimate - trend) %>%
  group_by(year) %>%
  summarize(estimate = median(deviance),
            lower = quantile(deviance, 0.25),
            upper = quantile(deviance, 0.75),
            llower = quantile(deviance, 0.025),
            uupper = quantile(deviance, 0.975)) %>%
  mutate(country = "All")

#### Write to disc ####

rbind(country_estimates, 
      europe_estimate,
      filter(country_estimate_czsk, country == "Czechia/Slovakia")) %>%
  write_csv(., path = "results/timesync/timesync_estimates_annual.csv")

rbind(country_estimates_mean, 
      europe_estimate_mean,
      filter(country_estimate_mean_czsk, country == "Czechia/Slovakia")) %>%
  write_csv(., path = "results/timesync/timesync_estimates.csv")

rbind(trend_evidence_country,
      trend_evidence_europe,
      filter(trend_evidence_country_czsk, country == "Czechia/Slovakia")) %>%
  write_csv(., path = "results/timesync/timesync_trends.csv")

rbind(country_trends,
      europe_trend,
      filter(country_trend_czsk, country == "Czechia/Slovakia")) %>%
  write_csv(., path = "results/timesync/timesync_trend_predictions.csv")

write_csv(deviance, path = "results/timesync/timesync_deviance.csv")
