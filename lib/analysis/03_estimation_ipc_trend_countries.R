
# devtools::install_github("corneliussenf/disturbanceBayes")
# The disturbanceBayes model is available under https://zenodo.org/record/1221340

library(disturbanceBayes)
library(tidyverse)

#### Load data and wrangel into form ####

dat <- read.csv("data/icp.csv")

years_all <- dat %>%
  split(.$country) %>%
  map(., ~ .$year) %>%
  Reduce(intersect, .)

dat_summary <- dat %>%
  group_by(country, plotid, year) %>%
  summarize(dead = sum(dead, na.rm = TRUE),
            n_trees = length(unique(treeid))) %>%
  group_by(year, country) %>%
  summarize(dead = sum(dead),
            n_trees = sum(n_trees)) %>%
  ungroup() %>%
  filter(year %in% years_all)

#### Fit country model ####

fit_country <- dat_summary %>%
  split(.$country) %>%
  map(., ~ bayes_estimator(x = .,
                           disturbance_col = "dead",
                           total_col = "n_trees",
                           index_cols = "year",
                           p = c(0.025, 0.2, 0.25, 0.5, 0.75, 0.8, 0.975), 
                           model = "binomial",
                           trend = TRUE))

save(fit_country, file = "results/icp/icp_model_country_trend_country.RData")

#### Calculate weights ####

weights_country <- dat_summary %>%
  group_by(country) %>%
  summarize(trees = sum(n_trees)) %>%
  ungroup() %>%
  mutate(weight = trees / sum(trees))

weights_czsk <- weights_country %>%
  filter(country %in% c("Czechia", "Slovakia")) %>%
  mutate(weight = trees / sum(trees))

#### Annual disturbance rate ####

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

year_czsk <- intersect(fit_country$Slovakia$estimate$year, fit_country$Czechia$estimate$year)

country_estimate_czsk <- fit_country %>%
  map(., ~ .$posterior) %>%
  bind_rows(.id = "country") %>%
  filter(country %in% c("Czechia", "Slovakia") & year %in% year_czsk) %>%
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
  filter(country %in% c("Czechia", "Slovakia") & year %in% year_czsk) %>%
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
  filter(country %in% c("Czechia", "Slovakia") & year %in% year_czsk) %>%
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

#### Write to disc ####

rbind(country_estimates, 
      filter(country_estimate_czsk, country == "Czechia/Slovakia")) %>%
  write_csv(., path = "results/icp/icp_estimates_annual_country.csv")

rbind(country_estimates_mean, 
      filter(country_estimate_mean_czsk, country == "Czechia/Slovakia")) %>%
  write_csv(., path = "results/icp/icp_estimates_country.csv")

rbind(trend_evidence_country,
      filter(trend_evidence_country_czsk, country == "Czechia/Slovakia")) %>%
  write_csv(., path = "results/icp/icp_trends_country.csv")

rbind(country_trends,
      filter(country_trend_czsk, country == "Czechia/Slovakia")) %>%
  write_csv(., path = "results/icp/icp_trend_predictions_country.csv")
