
library(patchwork)
library(tidyverse)

#### Figure 1 ####

timesync_trend <- read_csv("results/timesync/timesync_trend_predictions.csv") %>%
  filter(country != "Czechia/Slovakia")

timesync_rates <- read_csv("results/timesync/timesync_estimates_annual.csv") %>%
  filter(country != "Czechia/Slovakia")

timesync_rate_mean <- "results/timesync/timesync_estimates.csv" %>%
  read_csv(.) %>%
  mutate(label = paste0("Average:~", round(.$estimate * 100, 2), "~(", round(.$llower * 100, 2), "-", 
                        round(.$uupper * 100, 2), ")~'%'~yr^-1")) %>%
  dplyr::select(country, label) %>%
  mutate(x = 1984.5, y = c(0.04)) %>%
  filter(country != "Czechia/Slovakia")

timesync_trend_mean <- "results/timesync/timesync_trends.csv" %>%
  read_csv(.) %>%
  mutate(label = paste0("Trend:~", round((exp(.$estimate) - 1) * 100, 2), "~(", 
                        round((exp(.$llower) - 1) * 100, 2), "-", round((exp(.$uupper) - 1) * 100, 2), ")~'%'~yr^-1")) %>%
  dplyr::select(country, label) %>%
  mutate(x = 1984.5, y = c(0.035)) %>%
  filter(country != "Czechia/Slovakia")

timesync_trend_evidence <- "results/timesync/timesync_trends.csv" %>%
  read_csv(.) %>%
  mutate(label = paste0("P(alpha > 0)==", round(.$evidence, 2))) %>%
  dplyr::select(country, label) %>%
  mutate(x = 1984.5, y = c(0.03)) %>%
  filter(country != "Czechia/Slovakia")

plotdat <- list(a = timesync_rates %>% filter(country != "All"),
                b = timesync_trend %>% filter(country != "All"),
                c = timesync_rate_mean %>% filter(country != "All"),
                d = timesync_trend_mean %>% filter(country != "All"),
                e = timesync_trend_evidence %>% filter(country != "All"))

figure1a <- ggplot() + 
  geom_ribbon(data = plotdat$a, aes(x = year, ymin = llower, ymax = uupper, fill = country), alpha = 0.5) +
  geom_line(data = plotdat$a, aes(x = year, y = estimate, col = country), size = 0.5) +
  geom_line(data = plotdat$b, aes(x = year, y = llower, group = country), size = 0.2, linetype = "dashed") +
  geom_line(data = plotdat$b, aes(x = year, y = uupper, group = country), size = 0.2, linetype = "dashed") +
  geom_line(data = plotdat$b, aes(x = year, y = estimate, group = country), size = 0.3, linetype = "solid") +
  scale_y_continuous(#limits = c(0, 0.05),
    labels = function(x) formatC(sprintf("%.2f", x * 100), digits = 3)) +
  scale_x_continuous(breaks = c(1990, 2000, 2010), limits = c(1984, 2016)) +
  labs(x = "Year", y = NULL, fill = NULL, col = NULL) +
  ggthemes::theme_few() +
  geom_text(data = plotdat$c, aes(x = x, y = y, label = label), 
            size = 1.9, hjust = 0, parse = TRUE) +
  geom_text(data = plotdat$d, aes(x = x, y = y, label = label), 
            size = 1.9, hjust = 0, parse = TRUE) +
  geom_text(data = plotdat$e, aes(x = x, y = y, label = label), 
            size = 1.9, hjust = 0, parse = TRUE) +
  facet_wrap(~country, ncol = 3) +
  theme(strip.text.x = element_text(angle = 0, hjust = 0, size = 9),
        plot.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 9),
        legend.position = "none") +
  scale_color_manual(values = c("#4477AA", "#66CCEE", "#228833", "#CCBB44", "#EE6677", "#AA3377")) +
  scale_fill_manual(values = c("#4477AA", "#66CCEE", "#228833", "#CCBB44", "#EE6677", "#AA3377"))

plotdat <- list(a = timesync_rates %>% filter(country == "All"),
                b = timesync_trend %>% filter(country == "All"),
                c = timesync_rate_mean %>% filter(country == "All"),
                d = timesync_trend_mean %>% filter(country == "All"),
                e = timesync_trend_evidence %>% filter(country == "All"))

figure1b <- ggplot() + 
  geom_ribbon(data = plotdat$a, aes(x = year, ymin = llower, ymax = uupper, fill = country), alpha = 0.5) +
  geom_line(data = plotdat$a, aes(x = year, y = estimate, col = country), size = 0.5) +
  geom_line(data = plotdat$b, aes(x = year, y = llower, group = country), size = 0.2, linetype = "dashed") +
  geom_line(data = plotdat$b, aes(x = year, y = uupper, group = country), size = 0.2, linetype = "dashed") +
  geom_line(data = plotdat$b, aes(x = year, y = estimate, group = country), size = 0.3, linetype = "solid") +
  scale_y_continuous(limits = c(0, 0.02),
    labels = function(x) formatC(sprintf("%.2f", x * 100), digits = 3)) +
  scale_x_continuous(breaks = c(1990, 2000, 2010), limits = c(1984, 2016)) +
  labs(x = "Year", y = bquote("Canopy mortality [%"*yr^-1*"]"), fill = NULL, col = NULL) +
  ggthemes::theme_few() +
  geom_text(data = plotdat$c, aes(x = x, y = y - 0.02, label = label), 
            size = 3, hjust = 0, parse = TRUE) +
  geom_text(data = plotdat$d, aes(x = x, y = y - 0.0165, label = label), 
            size = 3, hjust = 0, parse = TRUE) +
  geom_text(data = plotdat$e, aes(x = x, y = y - 0.013, label = label), 
            size = 3, hjust = 0, parse = TRUE) +
  facet_wrap(~country, ncol = 1) +
  theme(strip.text.x = element_text(angle = 0, hjust = 0, size = 9),
        plot.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 9),
        legend.position = "none") +
  scale_color_manual(values = c("#888888")) +
  scale_fill_manual(values = c("#888888"))

figure1 <- figure1b + figure1a + plot_layout(ncol = 2, width = c(4.5, 7))

ggsave("figure1.pdf", figure1, path = "paper/figures/", width = 7, height = 3.25)

#### Figure 2 ####

# Figure 2 is created in script 'driver_analysis.R'

#### Figure 3 ####

timesync_trend_agent <- "results/timesync/agent/timesync_trends_agent.csv" %>%
  read_csv(.) %>%
  mutate(agent = ifelse(agent == "non-standreplacing", "Non-stand-\nreplacing", "Stand-\nreplacing"))

timesync_trend <- "results/timesync/timesync_trends.csv" %>%
  read_csv(.) %>%
  mutate(agent = "Both") %>%
  bind_rows(timesync_trend_agent) %>%
  filter(country != "Czechia/Slovakia")

figure3 <- ggplot(timesync_trend) +
  geom_point(aes(x = fct_rev(fct_infreq(factor(country))),
                 y = (exp(estimate) - 1) * 100,
                 col = agent, shape = agent),
             position = position_dodge(0.35)) +
  geom_errorbar(aes(x = country,
                    ymin = (exp(llower) - 1) * 100,
                    ymax = (exp(uupper) - 1) * 100,
                    col = agent),
                width = 0, position = position_dodge(0.35)) +
  scale_y_continuous(labels = function(x) formatC(sprintf("%.2f", x)), limits = c(-8, 9)) +
  scale_x_discrete() +
  labs(x = NULL, 
       y = bquote("Fractional change in canopy mortality [% "*yr^-1*"]"),
       col = NULL, shape = NULL) +
  ggthemes::theme_few() +
  theme(strip.text.x = element_text(angle = 0, hjust = 0, size = 9),
        plot.title = element_text(size = 9),
        axis.text.y = element_text(size = 9, color = "black"),
        axis.text.x = element_text(size = 9),
        axis.title = element_text(size = 9),
        legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.text = element_text(size = 7)) +
  guides(col = guide_legend(ncol = 1,
                            keywidth = 0.2,
                            keyheight = 0.2,
                            default.unit = "inch"),
         shape = guide_legend(ncol = 1,
                              keywidth = 0.2,
                              keyheight = 0.2,
                              default.unit = "inch")) +
  scale_color_manual(values = c("#AA3377", "#66CCEE", "#CCBB44")) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed")

ggsave("figure3.pdf", figure3, path = "paper/figures/", width = 3.32, height = 3.32)

#### Figure 4 ####

### Part A

timesync_trend <- read_csv("results/timesync/timesync_trend_predictions.csv")
icp_trend <- read_csv("results/icp/icp_trend_predictions_europe.csv")
fao_trend <- read_csv("results/fao/fao_trend_predictions.csv")
greylit_trend <- read_csv("results/greylit/greylit_trend_predictions.csv")

timesync_rates <- read_csv("results/timesync/timesync_estimates_annual.csv")
icp_rates <- read_csv("results/icp/icp_estimates_annual_europe.csv")
fao_rates <- read_csv("results/fao/fao_harvest_rates_annual.csv")
greylit_rates <- read_csv("results/greylit/greylit_harvest_rates_annual.csv")

trend <- list(timesync = timesync_trend, 
              icp = icp_trend, 
              fao = fao_trend, 
              wind = greylit_trend %>% filter(., agent == "wind") %>% dplyr::select(., -agent), 
              barkbeetle = greylit_trend %>% filter(., agent == "barkbeetle") %>% dplyr::select(., -agent)) %>%
  bind_rows(.id = "measure") %>%
  mutate(measure = recode(measure,
                          fao = "Wood\nremoval",
                          icp = "Tree\nmortality",
                          timesync = "Canopy\nmortality",
                          wind = "Wind\ndisturbances",
                          barkbeetle = "Bark beetle\ndisturbances")) %>%
  filter(country %in% c("All")) %>%
  dplyr::select(measure, year, country, estimate, llower, uupper) %>%
  dplyr::rename(trend = estimate) %>%
  mutate(measure = factor(measure, 
                          levels = c("Canopy\nmortality", "Wood\nremoval",
                                     "Tree\nmortality", "Bark beetle\ndisturbances",
                                     "Wind\ndisturbances")))

rates <- list(timesync = timesync_rates, 
       icp = icp_rates, 
       fao = fao_rates %>% dplyr::select(., year, country, estimate = volume_rate), 
       wind = greylit_rates %>% filter(., agent == "wind") %>% dplyr::select(., year, country, estimate = volume_rate), 
       barkbeetle = greylit_rates %>% filter(., agent == "barkbeetle") %>% dplyr::select(., year, country, estimate = volume_rate)) %>% 
  bind_rows(.id = "measure") %>%
  mutate(measure = recode(measure, 
                          fao = "Wood\nremoval",
                          icp = "Tree\nmortality",
                          timesync = "Canopy\nmortality",
                          wind = "Wind\ndisturbances",
                          barkbeetle = "Bark beetle\ndisturbances")) %>%
  filter(country %in% c("All")) %>%
  mutate(measure = factor(measure, 
                          levels = c("Canopy\nmortality", "Wood\nremoval",
                                     "Tree\nmortality", "Bark beetle\ndisturbances",
                                     "Wind\ndisturbances")))

correlations <- read_csv("results/correlations.csv") %>%
  filter(which == "trend") %>%
  filter(measure != "salvage") %>%
  mutate(x = c(Inf), 
         y = c(-Inf, -Inf, Inf, Inf),
         hjustvar = c(1, 1, 1, 1),
         vjustvar = c(-0.2, -0.2, 1.2, 1.2)) %>%
  mutate(label = paste0("r = ", round(estimate, 2), " (", round(llower, 2), " - ", round(uupper, 2), ")")) %>%
  mutate(measure = recode(measure, 
                          fao = "Wood\nremoval",
                          icp = "Tree\nmortality",
                          timesync = "Canopy\nmortality",
                          wind = "Wind\ndisturbances",
                          barkbeetle = "Bark beetle\ndisturbances")) %>%
  mutate(measure = factor(measure, 
                          levels = c("Canopy\nmortality", "Wood\nremoval",
                                     "Tree\nmortality", "Bark beetle\ndisturbances",
                                     "Wind\ndisturbances")))

figure4a <- ggplot(rates) +
  geom_ribbon(aes(x = year, ymin = llower, ymax = uupper, fill = measure), alpha = 0.25) +
  geom_line(aes(x = year, y = estimate, col = measure)) +
  geom_ribbon(data = trend, aes(x = year, ymin = llower, ymax = uupper), alpha = 0.15) +
  geom_line(data = trend, aes(x = year, y = trend), size = 0.4) +
  scale_y_continuous(#limits = c(0, 0.05),
    labels = function(x) formatC(sprintf("%.2f", x * 100), digits = 3), expand = c(0.0001, 0.0001)) +
  scale_x_continuous(breaks = c(1990, 2000, 2010), limits = c(1984, 2016), expand = c(0.0001, 0.0001)) +
  labs(x = "Year", y = bquote("Rate [% "*yr^-1*"]"), fill = NULL, col = NULL) +
  ggthemes::theme_few() +
  facet_wrap(~measure, ncol = 1, scales = "free_y", strip.position = "right") +
  theme(strip.text = element_text(size = 7),
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 9),
        panel.spacing = unit(0, "lines"),
        legend.position = "none") +
  scale_color_manual(values = c("#4477AA", "#66CCEE", "#228833", "#CCBB44", "#EE6677", "#AA3377")) +
  scale_fill_manual(values = c("#4477AA", "#66CCEE", "#228833", "#CCBB44", "#EE6677", "#AA3377"))

### Part B

canopymort <- rates %>%
  filter(measure == "Canopy\nmortality") %>%
  dplyr::select(country, year, mortality = estimate, mortality_lower = llower, mortality_upper = uupper)

figure4b <- rates %>%
  filter(measure != "Canopy\nmortality") %>%
  left_join(canopymort, by = c("country", "year")) %>%
  mutate(measure = gsub("\n", " ", measure)) %>%
  mutate(measure = factor(measure, 
                          levels = c("Wood removal",
                                     "Tree mortality", "Bark beetle disturbances",
                                     "Wind disturbances"))) %>%
  ggplot(.) +
  geom_point(aes(x = mortality, y = estimate, col = measure)) +
  geom_errorbar(aes(x = mortality, ymin = llower, ymax = uupper, col = measure), width = 0) +
  geom_errorbarh(aes(y = estimate, xmin = mortality_lower, xmax = mortality_upper, col = measure), height = 0) +
  geom_smooth(aes(x = mortality, y = estimate), method = "lm", 
              col = "darkgrey", se = FALSE, size = 0.75) +
  scale_x_continuous(labels = function(x) formatC(sprintf("%.2f", x * 100))) +
  scale_y_continuous(labels = function(x) formatC(sprintf("%.2f", x * 100))) +
  labs(x = bquote("Canopy mortality [% "*yr^-1*"]"), y = bquote("Rate [% "*yr^-1*"]")) +
  ggthemes::theme_few() +
  theme(strip.text = element_text(size = 7),
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 9),
        legend.position = "none") +
  facet_wrap(~measure, scales = "free_y") +
  scale_color_manual(values = c("#66CCEE", "#228833", "#CCBB44", "#EE6677", "#AA3377"))

figure4 <- figure4a + figure4b + 
  plot_layout(ncol = 2, widths = c(0.4, 0.5))

ggsave("figure4.pdf", figure4, path = "paper/figures/", width = 6.5, height = 3.42)



