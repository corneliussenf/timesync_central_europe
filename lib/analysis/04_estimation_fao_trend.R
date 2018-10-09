
library(tidyverse)
library(brms)

#### Get data and calculate rates ####

dat_all <- read_csv("data/fao.csv")

harvest_rates <- dat_all %>%
  group_by(year) %>%
  summarize(volume = sum(volume),
            stock = sum(stock)) %>%
  ungroup() %>%
  mutate(volume_rate = volume / stock) %>%
  mutate(country = "All") %>%
  bind_rows(dat_all)
  
#### Average over countries ####

harvest_rates_mean <- harvest_rates %>%
  group_by(country) %>%
        summarise(volume_rate = mean(volume_rate)) %>%
  ungroup()

#### Trend estimation ####

trend_fit <- harvest_rates %>%
  group_by(country) %>%
  mutate(time = 1:length(year)) %>%
  ungroup() %>%
  split(.$country) %>%
  map(., ~ brm(volume_rate ~ time,
               data = .,
               family = Beta()))

trend <- trend_fit %>%
  map(~ as.data.frame(.)$b_time)

trend_evidence <- trend %>%
  map(., ~ data_frame(iterations = 1:length(.),
                      trend = .)) %>%
  bind_rows(.id = "country") %>%
  group_by(country) %>%
  summarize(evidence = mean(trend > 0),
            estimate = median(trend),
            lower = quantile(trend, 0.25),
            upper = quantile(trend, 0.75),
            llower = quantile(trend, 0.025),
            uupper = quantile(trend, 0.975))

trend_prediction <- trend_fit %>%
  map(., ~ brms::posterior_linpred(object = ., 
                                   newdata = data.frame(time = 1:33),
                                   transform = TRUE)) %>%
  map(., ~ apply(., 2, quantile, c(0.5, 0.025, 0.25, 0.75, 0.975))) %>%
  map(., ~ as.data.frame(t(.))) %>%
  map(., ~ setNames(., c("estimate", "llower", "lower", "upper", "uupper"))) %>%
  map(., ~ mutate(., year = 1984:2016)) %>%
  bind_rows(.id = "country")

#### Write to disc ####

write_csv(harvest_rates, path = "results/fao/fao_harvest_rates_annual.csv")

write_csv(harvest_rates_mean, path = "results/fao/fao_harvest_rates.csv")

write_csv(trend_evidence, path = "results/fao/fao_trends.csv")
  
write_csv(trend_prediction, path = "results/fao/fao_trend_predictions.csv")
