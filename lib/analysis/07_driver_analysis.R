
library(tidyverse)
library(rstanarm)
library(gridExtra)
library(grid)
library(raster)
library(rgeos)

#### Prepare climate data ####

climate <- read_csv("data/climate.csv")

climate_annual <- climate %>%
  group_by(country, index, year) %>%
  summarize(mean = mean(annual, na.rm = TRUE),
            se = sd(annual, na.rm = TRUE) / sqrt(sum(!is.na(annual))),
            CI95 = se * 2)

climate_fiveyear <- climate %>%
  mutate(period = cut(year, c(1984, seq(1990, 2010, 5), 2016),
                      labels = c("1984-1989", "1990-1994", "1995-1999", "2000-2004", "2005-2009", "2010-2015"),
                      right = FALSE, include.lowest = TRUE)) %>%
  group_by(period, index, country) %>%
  summarize(mean = mean(annual, na.rm = TRUE),
            se = sd(annual, na.rm = TRUE) / sqrt(sum(!is.na(annual))),
            CI95 = se * 2) %>%
  ungroup() %>%
  na.omit(.)

ggplot(climate_fiveyear, aes(x = period, y = mean, col = country)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean - CI95, ymax = mean + CI95), width = 0) +
  facet_wrap(~index, scales = "free") 

#### Prepare forest structural data ####

structure <- read_csv("data/structure.csv") 
structure[structure$year == 2015, "year"] <- 2010
structure <- structure %>% 
  dplyr::select(country, year, area:conifer) %>%
  group_by(country, year) %>%
  summarise_all(mean) %>%
  ungroup()

structure_fiveyear <- structure %>%
  mutate(country = ifelse(country == "Czechoslovakia", "Czechia/Slovakia", country)) %>%
  dplyr::select(area, country, year, growing.stock, median.age, conifer, older100) %>%
  mutate(growing.stock.area = growing.stock / area * 1000) %>%
  dplyr::select(country, year, growing.stock.area, median.age, conifer, older100) %>%
  mutate(period = recode(year, 
                         "1985" = "1984-1989", 
                         "1990" = "1990-1994", 
                         "1995" = "1995-1999", 
                         "2000" = "2000-2004", 
                         "2005" = "2005-2009", 
                         "2010" = "2010-2015")) %>%
  dplyr::select(-year)

ggplot(structure_fiveyear, aes(x = period, y = growing.stock.area, col = country, group = country)) +
  geom_point() +
  geom_line()

#### Prepare mortality data ####

## TimeSync

load("results/timesync/timesync_model_country_trend.RData")

timesync_annual <- read_csv("results/timesync/timesync_estimates_annual.csv")

# Weights for combining CZ and SK

countries <- shapefile("rawdata/nuts/data/NUTS_RG_01M_2013.shp")
countries <- subset(countries, countries$STAT_LEVL_ == 0)
countries <- spTransform(countries, CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"))
countries <- subset(countries, countries$NUTS_ID %in% c("AT", "CZ", "DE", "CH", "SK", "PL"))
countries$area <- gArea(countries, byid = TRUE)

weights <- countries@data %>%
  filter(NUTS_ID %in% c("CZ", "SK")) %>%
  mutate(weight = area / sum(area)) %>%
  mutate(country = recode(NUTS_ID, 
                          CZ = "Czechia",
                          SK = "Slovakia")) %>%
  dplyr::select(country, weight)

posterior_czsk <- fit_country %>%
  map(., ~ .$posterior) %>%
  bind_rows(.id = "country") %>%
  filter(country %in% c("Czechia", "Slovakia")) %>%
  left_join(weights, by = "country") %>%
  group_by(iterations, year) %>%
  summarize(value = sum(value * weight)) %>%
  mutate(country = "Czechia/Slovakia")

timesync_fiveyear <- fit_country %>%
  map(., ~ .$posterior) %>%
  bind_rows(.id = "country") %>%
  bind_rows(posterior_czsk)  %>%
  mutate(period = cut(year, c(1984, seq(1990, 2010, 5), 2016),
                      labels = c("1984-1989", "1990-1994", "1995-1999", "2000-2004", "2005-2009", "2010-2015"),
                      right = FALSE, include.lowest = TRUE)) %>%
  group_by(period, country, iterations) %>%
  summarize(mortality = mean(value)) %>%
  ungroup() %>%
  group_by(period, country) %>%
  summarize(mortality = median(mortality, na.rm = TRUE),
            mortality_lower = quantile(mortality, 0.025, na.rm = TRUE),
            mortality_upper = quantile(mortality, 0.975, na.rm = TRUE))

#### Combine data ####

### Climate

dat_climate <- climate_fiveyear %>%
  left_join(timesync_fiveyear, by = c("country", "period")) %>%
  ungroup()

# dat_climate_annual <- climate_fiveyear %>%
#   left_join(timesync_annual, by = c("country", "year")) %>%
#   ungroup()

### Forest structure

dat_structure <- structure_fiveyear %>%
  gather(key = index, value = value, -country, -period) %>%
  mutate(index = gsub("\\.", "", index)) %>%
  left_join(timesync_fiveyear, by = c("country", "period")) %>%
  ungroup()
  
#### Explorative plots ####

ggplot(dat_climate, aes(x = mean, y = mortality, col = country)) +
  geom_point() +
  #geom_errorbarh(aes(xmin = mean - se, xmax = mean + se), height = 0) +
  #geom_errorbar(aes(ymin = mortality_lower, ymax = mortality_upper), width = 0) +
  facet_wrap(~index, scales = "free") +
  geom_smooth(method = "lm", se = FALSE) +
  ggthemes::theme_few()

ggplot(dat_structure, aes(x = value, y = mortality, col = country)) +
  geom_point() +
  geom_errorbar(aes(ymin = mortality_lower, ymax = mortality_upper), width = 0) +
  facet_wrap(~index, scales = "free") +
  geom_smooth(method = "lm", se = FALSE) +
  ggthemes::theme_few()

#### Models ####

rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

library(loo)

model_fit <- function(m) {
  f0 <- stan_lmer(log(mortality) ~ x + (1 | country), 
                  data = m, adapt_delta = 0.999,
                  prior_intercept = student_t(3, 0, 10),
                  prior = student_t(3, 0, 10))
  f1 <- stan_lmer(log(mortality) ~ x + (1 + x | country), 
                  data = m, adapt_delta = 0.999,
                  prior_intercept = student_t(3, 0, 10),
                  prior = student_t(3, 0, 10))
  l <- compare(loo(f0, k_threshold = 0.7), loo(f1, k_threshold = 0.7))
  if (l[1] > 0) {return(f1)} else {return(f0)}
}

## Fit models

fit_climate <- dat_climate %>%
  filter(index %in% c("RR", "TG")) %>%
  mutate(x = mean) %>%
  split(.$index) %>%
  map(., ~ model_fit(.))

fit_structure <- dat_structure %>%
  filter(index %in% c("growingstockarea", "medianage")) %>%
  mutate(x = value) %>%
  split(.$index) %>%
  map(., ~ model_fit(.))

fits <- c(fit_climate, fit_structure)

save(fits, file = "temp/fits_drivers.RData")
#load(file = "temp/fits_drivers.RData")

## Calculate R2

r2 <- fits %>%
  map(., ~ bayes_R2(., re.form = NA)) %>% 
  map(., ~ as.data.frame(t(quantile(., c(0.025, 0.5, 0.975))))) %>%
  bind_rows(.id = "index") %>%
  dplyr::rename("lower" = "2.5%",
                "estimate" = "50%",
                "upper" = "97.5%") %>%
  mutate(index = factor(index, levels = c("growingstockarea", "medianage", "TG", "RR")))

evidence <- fits %>%
  map(~ as.data.frame(.) %>%
        summarize(evidence = mean(x > 0),
                  estimate = median(x),
                  lower = quantile(x, 0.25),
                  upper = quantile(x, 0.75),
                  llower = quantile(x, 0.025),
                  uupper = quantile(x, 0.975))) %>%
  bind_rows(.id = "index") %>%
  mutate(index = factor(index, levels = c("growingstockarea", "medianage", "TG", "RR")))

#### Plot models ####

plotdat_climate <- dat_climate %>%
  filter(index %in% c("RR", "TG")) %>%
  mutate(x = mean,
         x_lower = mean - CI95,
         x_upper = mean + CI95) %>%
  dplyr::select(period, index, country, x, x_lower, x_upper, mortality, mortality_lower, mortality_upper)

plotdat_structure <- dat_structure %>%
  filter(index %in% c("growingstockarea", "medianage")) %>%
  mutate(x = value)  %>%
  dplyr::select(period, index, country, x, mortality, mortality_lower, mortality_upper)

plotdat <- bind_rows(plotdat_climate,
                     plotdat_structure) %>%
  mutate(country = factor(country)) %>%
  mutate(period = factor(period)) %>%
  mutate(index = factor(index, levels = c("growingstockarea", "medianage", "TG", "RR")))

pred <- fits %>%
  map(~ data.frame(x = seq(min(.$x[,2]), max(.$x[,2]), length.out = 100),
                   pred = apply(posterior_linpred(., newdata = data.frame(x = seq(min(.$x[,2]), max(.$x[,2]), length.out = 100)), re.form = NA), 2, median),
                   pred_lower = apply(posterior_linpred(., newdata = data.frame(x = seq(min(.$x[,2]), max(.$x[,2]), length.out = 100)), re.form = NA), 2, quantile, 0.025),
                   pred_upper = apply(posterior_linpred(., newdata = data.frame(x = seq(min(.$x[,2]), max(.$x[,2]), length.out = 100)), re.form = NA), 2, quantile, 0.975))) %>%
  bind_rows(.id = "index") %>%
  mutate(index = factor(index, levels = c("growingstockarea", "medianage", "TG", "RR")))

plots <- pmap(list(a = plotdat %>% 
                     split(.$index), 
                   b = pred %>% 
                     split(.$index),
                   c = list(bquote("Growing stock (100 "*m^3*"/"*ha*")"), 
                            "Median age (years)", 
                            "Temperature (°C)", 
                            "Precipitation (100 mm)"),
                   e = r2 %>% 
                     split(.$index),
                   f = evidence %>% 
                     split(.$index),
                   g0 = as.list(rep(0, 4)),
                   g1 = as.list(rep(0.03, 4)),
                   g2 = as.list(rep(0.03, 4)),
                   h = as.list(rep(0.0035, 4)),
                   i = as.list(c(100, 1, 1, 100))),
     function (a, b, c, e, f, g0, g1, g2, h, i) {
       p <- ggplot(a, aes(x = x / i, y = mortality)) +
         #geom_errorbar(aes(ymin = mortality_lower, ymax = mortality_upper, col = country), width = 0, alpha = 0.5) +
         #geom_errorbarh(aes(xmin = x_lower / i, xmax = x_upper / i, col = country), height = 0, alpha = 0.5) +
         geom_point(aes(shape = period, col = country), size = 0.5) +
         #geom_line(data = j, aes(x = x / i, y = exp(pred), col = country), alpha = 0.5, size = 0.5) +
         geom_line(data = b, aes(x = x / i, y = exp(pred_lower)), linetype = "dashed", alpha = (f$evidence)) +
         geom_line(data = b, aes(x = x / i, y = exp(pred_upper)), linetype = "dashed", alpha = (f$evidence)) +
         geom_line(data = b, aes(x = x / i, y = exp(pred)), alpha = (f$evidence)) +
         labs(x = c, y = NULL, color = NULL, shape = NULL) +
         ggthemes::theme_few() +
         theme(plot.title = element_text(size = 8),
               axis.text = element_text(size = 8),
               axis.title = element_text(size = 8),
               legend.text = element_text(size = 8),
               legend.key.size = unit(0.8,"line"),
               legend.position = "right") +
         annotate("text", x = min(a$x / i, na.rm = TRUE), y = g2, 
                  hjust = 0, vjust = 1, 
                  label = paste("R^2 ==", round(e$estimate, 2), "~(", round(e$lower, 2), "-", round(e$upper, 2),")"), size = 2, parse = TRUE) +
         annotate("text", x = min(a$x / i, na.rm = TRUE), y = g2 - h, 
                  hjust = 0, vjust = 1, 
                  label = paste("beta[1] ==", round(f$estimate * i, 2), "~(", round(f$lower * i, 2), "-", round(f$upper * i, 2),")"), size = 2, parse = TRUE) +
         annotate("text", x = min(a$x / i, na.rm = TRUE), y = g2 - 2 * h, 
                  hjust = 0, vjust = 1, 
                  label = paste("P(beta[1] > 0) ==", round(f$evidence, 2)), size = 2, parse = TRUE) +
         scale_shape_manual(values = 1:7) +
         scale_y_continuous(labels = function(x) formatC(sprintf("%.1f", x * 100)), limits = c(g0, g1)) +
         scale_color_manual(values = c("#4477AA", "#66CCEE", "#BBBBBB", "#228833", "#CCBB44", "#EE6677", "#AA3377"), drop = FALSE)
       print(p)
     })

tmp <- ggplot_gtable(ggplot_build(plots[[2]]))
leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
legend <- tmp$grobs[[leg]]

p <- grid.arrange(
  arrangeGrob(grobs = lapply(plots, function(p) p + theme(legend.position = "none")), 
              ncol = 2,
              left = textGrob(bquote("Canopy mortality (% "*yr^-1*")"), gp = gpar(fontsize = 9), rot = 90)),
  legend, 
  widths = c(6, 2)
)

ggsave("figure2.pdf", p, path = "paper/figures/", width = 4.5, height = 3.5)

 ### Split predictions by country

pred_country <- fits %>%
  map(~ expand.grid(x = seq(min(.$x[,2]), max(.$x[,2]), length.out = 1000), 
              country = unique(.$data$country))) %>%
  map2(.y = fits, ~ cbind(.x,
                              pred = apply(posterior_linpred(.y, newdata = .x), 2, median),
                              pred_lower = apply(posterior_linpred(.y, newdata = .x), 2, quantile, 0.025),
                              pred_upper = apply(posterior_linpred(.y, newdata = .x), 2, quantile, 0.975))) %>%
  bind_rows(.id = "index") %>%
  mutate(index = factor(index, levels = c("growingstockarea", "medianage", "TG", "RR")))

plots_country <- pmap(list(a = plotdat %>% 
                             split(.$index), 
                           b = pred_country %>% 
                             split(.$index),
                           c = list(bquote("Growing stock [100 "*m^3*"/"*ha*"]"), 
                                    "Median age [years]", 
                                    "Temperature [°C]", 
                                    "Precipitation [100 mm]"),
                           g0 = as.list(rep(0, 4)),
                           g1 = as.list(rep(0.03, 4)),
                           i = as.list(c(100, 1, 1, 100))),
              function (a, b, c, g0, g1, i) {
                p <- ggplot(a, aes(x = x / i, y = mortality)) +
                  geom_point(aes(shape = period, col = country)) +
                  geom_line(data = b, aes(x = x / i, y = exp(pred), col = country)) +
                  labs(x = c, y = NULL, color = NULL, shape = NULL) +
                  ggthemes::theme_few() +
                  theme(plot.title = element_text(size = 9),
                        axis.text = element_text(size = 7),
                        axis.title = element_text(size = 8),
                        legend.text = element_text(size = 8),
                        legend.position="right") +
                  scale_y_continuous(labels = function(x) formatC(sprintf("%.1f", x * 100)), limits = c(g0, g1)) +
                  scale_color_manual(values = c("#4477AA", "#66CCEE", "#BBBBBB", "#228833", "#CCBB44", "#EE6677", "#AA3377"), drop = FALSE)
                print(p)
              })

tmp <- ggplot_gtable(ggplot_build(plots_country[[1]]))
leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
legend <- tmp$grobs[[leg]]

p_country <- grid.arrange(
  arrangeGrob(grobs = lapply(plots_country, function(p) p + theme(legend.position = "none")), 
              ncol = 2,
              left = textGrob(bquote("Mortality [% forest area "*yr^-1*"]"), gp = gpar(fontsize = 9), rot = 90)),
  legend, 
  widths = c(6, 2)
)

ggsave("figure2_country.pdf", p_country, path = "paper/figures/", width = 5.5, height = 4)




