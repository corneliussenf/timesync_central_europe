
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
  group_by(year) %>%
  summarize(dead = sum(dead),
            n_trees = sum(n_trees)) %>%
  ungroup() %>%
  filter(year %in% years_all)

#### Fit country model ####

fit <- bayes_estimator(x = dat_summary,
                       disturbance_col = "dead",
                       total_col = "n_trees",
                       index_cols = "year",
                       p = c(0.025, 0.2, 0.25, 0.5, 0.75, 0.8, 0.975), 
                       model = "binomial",
                       trend = TRUE)

save(fit, file = "results/icp/icp_model_country_trend_europe.RData")
#load(file = "results/icp/icp_model_country_trend_europe.RData")

#### Annual forest mortality ####

europe_estimates_annual <- fit$posterior %>%
  group_by(year) %>%
  summarise(estimate = median(value),
            lower = quantile(value, 0.25),
            upper = quantile(value, 0.75),
            llower = quantile(value, 0.025),
            uupper = quantile(value, 0.975)) %>%
  mutate(country = "All")
  
#### Mean annual forest mortality ####

europe_estimates <- fit$posterior %>%
  group_by(iterations) %>%
  summarize(value = mean(value)) %>%
  ungroup() %>%
  summarise(estimate = median(value),
            lower = quantile(value, 0.25),
            upper = quantile(value, 0.75),
            llower = quantile(value, 0.025),
            uupper = quantile(value, 0.975)) %>%
  mutate(country = "All")

#### Trend in forest mortality ####

country_trends <- fit$trend_posterior %>%
  group_by(year) %>%
  summarise(estimate = median(value),
            lower = quantile(value, 0.25),
            upper = quantile(value, 0.75),
            llower = quantile(value, 0.025),
            uupper = quantile(value, 0.975)) %>%
  mutate(year = as.integer(year)) %>%
  mutate(country = "All")

#### Evidence of increasing forest mortality ####

trend_evidence_europe <- fit$trend %>%
  data_frame(iterations = 1:length(.),
             trend = .) %>%
  summarize(evidence = mean(trend > 0),
            estimate = median(trend),
            lower = quantile(trend, 0.25),
            upper = quantile(trend, 0.75),
            llower = quantile(trend, 0.025),
            uupper = quantile(trend, 0.975)) %>%
  mutate(country = "All")

#### Deviance from trend (europea level) ####

deviance <- fit$posterior %>%
  dplyr::rename(estimate = value) %>%
  left_join(fit$trend_posterior %>% dplyr::rename(trend = value), by = c("year", "iterations")) %>%
  mutate(deviance = estimate - trend) %>%
  group_by(year) %>%
  summarize(estimate = median(deviance),
            lower = quantile(deviance, 0.25),
            upper = quantile(deviance, 0.75),
            llower = quantile(deviance, 0.025),
            uupper = quantile(deviance, 0.975)) %>%
  mutate(country = "All")

#### Write to disc ####

write_csv(europe_estimates_annual, path = "results/icp/icp_estimates_annual_europe.csv")
write_csv(europe_estimates, path = "results/icp/icp_estimates_europe.csv")
write_csv(trend_evidence_europe, path = "results/icp/icp_trends_europe.csv")
write_csv(country_trends, path = "results/icp_trend_predictions_europe.csv")

write_csv(deviance, path = "results/icp/icp_deviance.csv")
