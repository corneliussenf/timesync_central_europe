
library(tidyverse)

#### Checl TimeSync model ####

load("results/timesync/timesync_model_country_trend.RData")

fit_country %>%
  map(., ~ .$posterior_p_values) %>%
  map(., ~ as.data.frame(.)) %>%
  map(., ~ dplyr::select(., mean, se_mean) %>%
        mutate(., parameter = c("Minimum", "Maximum", "Mean", "Standard deviation"))) %>%
  bind_rows(.id = "country") %>%
  mutate(label = paste0(round(mean, 2), "±", round(se_mean, 2))) %>%
  data.table::dcast(country ~ parameter, value.var = "label") %>%
  dplyr::rename(Country = country) %>%
  knitr::kable()

posterior_draws <- fit_country %>%
  map(., ~ .$posterior_draws) %>%
  bind_rows(.id = "country")

ggplot() +
  geom_density(data = filter(posterior_draws, replication != "data"), 
               aes(x = rate, group = replication), size = 0.3) +
  geom_density(data = filter(posterior_draws, replication == "data"), 
               aes(x = rate), col = "#EE6677") +
  labs(x = bquote("Forest mortality [% "*yr^-1*"]"), y = "Density") +
  ggthemes::theme_few() +
  facet_wrap(~country, scales = "free") +
  theme(strip.text.x = element_text(angle = 0, hjust = 0, size = 9),
        plot.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position = "none") +
  scale_x_continuous(labels = function(x) formatC(sprintf("%.2f", x * 100), digits = 3)) +
  geom_hline(yintercept = 0, size = 0.8, col = "white")

#### Check ICP model ####
  
load("results/icp/icp_model_country_trend_europe.RData")

fit$posterior_p_values %>%
  as.data.frame(.) %>%
  mutate(., parameter = c("Minimum", "Maximum", "Mean", "Standard deviation")) %>%
  mutate(label = paste0(round(mean, 2), "±", round(se_mean, 2))) %>%
  mutate(Country = "All") %>%
  data.table::dcast(Country ~ parameter, value.var = "label") %>%
  knitr::kable()

posterior_draws <- fit$posterior_draws

ggplot() +
  geom_density(data = filter(posterior_draws, replication != "data"), 
               aes(x = rate, group = replication), size = 0.3) +
  geom_density(data = filter(posterior_draws, replication == "data"), 
               aes(x = rate), col = "#EE6677") +
  labs(x = bquote("Forest mortality [% "*yr^-1*"]"), y = "Density") +
  ggthemes::theme_few() +
  theme(strip.text.x = element_text(angle = 0, hjust = 0, size = 9),
        plot.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position = "none") +
  scale_x_continuous(labels = function(x) formatC(sprintf("%.2f", x * 100), digits = 3)) +
  geom_hline(yintercept = 0, size = 0.8, col = "white")
