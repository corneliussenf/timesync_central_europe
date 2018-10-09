
library(tidyverse)
library(brms)

#### Get data and calculate rates ####

dat_all <- read_csv("data/grey_lit.csv")

harvest_rates <- dat_all %>%
  group_by(year, agent) %>%
  summarize(volume = sum(volume, na.rm = TRUE),
            stock = sum(stock, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(volume_rate = volume / stock) %>%
  mutate(country = "All") %>%
  bind_rows(dat_all)
  
#### Average over countries ####

harvest_rates_mean <- harvest_rates %>%
  group_by(country, agent) %>%
        summarise(volume_rate = mean(volume_rate, na.rm = TRUE)) %>%
  ungroup()

#### Trend estimation ####

trend_fit <- harvest_rates %>%
  filter(agent %in% c("barkbeetle", "wind") & country == "All") %>%
  mutate(volume_rate = ifelse(volume_rate == 0, 0.00000001, volume_rate)) %>%
  group_by(agent) %>%
  mutate(time = 1:length(year)) %>%
  ungroup() %>%
  split(.$agent) %>%
  map(., ~ brm(volume_rate ~ time,
               data = .,
               family = Beta()))

trend <- trend_fit %>%
  map(~ as.data.frame(.)$b_time)

trend_evidence <- trend %>%
  map(., ~ data_frame(iterations = 1:length(.),
                      trend = .)) %>%
  bind_rows(.id = "agent") %>%
  group_by(agent) %>%
  summarize(evidence = mean(trend > 0),
            estimate = median(trend),
            lower = quantile(trend, 0.25),
            upper = quantile(trend, 0.75),
            llower = quantile(trend, 0.025),
            uupper = quantile(trend, 0.975)) %>%
  mutate(country = "All")

trend_prediction <- trend_fit %>%
  map(., ~ brms::posterior_linpred(object = ., 
                                   newdata = data.frame(time = 1:length(unique((harvest_rates$year)))),
                                   transform = TRUE)) %>%
  map(., ~ apply(., 2, quantile, c(0.5, 0.025, 0.25, 0.75, 0.975))) %>%
  map(., ~ as.data.frame(t(.))) %>%
  map(., ~ setNames(., c("estimate", "llower", "lower", "upper", "uupper"))) %>%
  map(., ~ mutate(., year = unique(harvest_rates$year))) %>%
  bind_rows(.id = "agent") %>%
  mutate(country = "All")

ggplot(harvest_rates %>% filter(country == "All" & agent != "fire"), aes(x = year, y = volume_rate, col = agent)) +
  geom_line() +
  #geom_line(aes(y = zoo::rollmean(volume_rate, 3, na.pad = TRUE))) +
  geom_line(data = trend_prediction, aes(y = estimate)) +
  geom_line(data = trend_prediction, aes(y = llower), linetype = "dashed") +
  geom_line(data = trend_prediction, aes(y = uupper), linetype = "dashed") +
  facet_wrap(~agent, scales = "free")

#### Write to disc ####

write_csv(harvest_rates, path = "results/greylit/greylit_harvest_rates_annual.csv")

write_csv(harvest_rates_mean, path = "results/greylit/greylit_harvest_rates.csv")

write_csv(trend_evidence, path = "results/greylit/greylit_trends.csv")
  
write_csv(trend_prediction, path = "results/greylit/greylit_trend_predictions.csv")
