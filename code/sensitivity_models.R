set.seed(100)

library("hrbrthemes")
library("tidyverse")
library("here")
library("gridExtra")
library("brms")
library("tidybayes")


data_path <- here::here("data","data.Rda")
load(file = data_path)

data_noimputed <- data %>%
    filter(
        impute_se == 0
    )

m_1_no_impute <- brm(data = data_noimputed, family = student,
                     dist| se(dist_se,  sigma = TRUE) ~ 1 +  
                         ls + ml + neg_bin + other + zero_inflated + 
                         no_region_time_effects + fixed_effects_no_time + random_effects + no_panel + duration +
                         origin_constrained + destination_constrained + no_dom_international +
                         dist_se + 
                         s(average_year) + s(pub_year) +
                         (1 | study_id),
                     iter = 5000, warmup = 2000, cores = 3, chains = 3,
                     prior = c(prior(normal(-1, 1), class = Intercept),
                               prior(normal(0, 1), class = b),
                               prior(exponential(1), class = sd),
                               prior(exponential(1), class = sigma)),
                     seed = 14,
                     control = list(adapt_delta = 0.8,
                                    max_treedepth = 14)
)

m_2_no_impute <- brm(data = data_noimputed, family = student,
                     dist| se(dist_se,  sigma = TRUE) ~ 1 +  
                         ls + ml + neg_bin + other + zero_inflated + 
                         no_region_time_effects + fixed_effects_no_time + random_effects + no_panel + duration +
                         origin_constrained + destination_constrained + no_dom_international + 
                         continental_o + country_o + continental_d + country_d +
                         no_europe_o + no_australia_o + no_africa_o + no_north_america_o + no_south_america_o + no_asia_o +
                         no_europe_d + no_australia_d + no_africa_d + no_north_america_d + no_south_america_d + no_asia_d +
                         s(average_year) + s(pub_year) +
                         dist_se +
                         (1 | study_id),
                     iter = 5000, warmup = 2000, cores = 3, chains = 3,
                     prior = c(prior(normal(-1, 1), class = Intercept),
                               prior(normal(0, 1), class = b),
                               prior(exponential(1), class = sd),
                               prior(exponential(1), class = sigma)),
                     seed = 14,
                     control = list(adapt_delta = 0.8,
                                    max_treedepth = 14)
)

m_3_no_impute <- brm(data = data_noimputed, family = student,
                     dist| se(dist_se,  sigma = TRUE) ~ 1 +  
                         ls + ml + neg_bin + other + zero_inflated + 
                         no_region_time_effects + fixed_effects_no_time + random_effects + no_panel + duration +
                         origin_constrained + destination_constrained + no_dom_international +
                         continental_o + country_o + continental_d + country_d +
                         no_europe_o + no_australia_o + no_africa_o + no_north_america_o + no_south_america_o + no_asia_o +
                         no_europe_d + no_australia_d + no_africa_d + no_north_america_d + no_south_america_d + no_asia_d +
                         no_gdp_o + no_gdppc_o +no_pop_o + no_gdp_d + no_gdppc_d + no_pop_d + 
                         dist_se +
                         s(average_year) + s(pub_year) +
                         (1 | study_id),
                     iter = 5000, warmup = 2000, cores = 3, chains = 3,
                     prior = c(prior(normal(-1, 1), class = Intercept),
                               prior(normal(0, 1), class = b),
                               prior(exponential(1), class = sd),
                               prior(exponential(1), class = sigma)),
                     seed = 14,
                     control = list(adapt_delta = 0.8,
                                    max_treedepth = 14)
)

m_4_no_impute <- brm(data = data_noimputed, family = student,
           dist| se(dist_se,  sigma = TRUE) ~ 1 +  
               ls + ml + neg_bin + other + zero_inflated + 
               no_region_time_effects + fixed_effects_no_time + random_effects + no_panel + duration +
               origin_constrained + destination_constrained + no_dom_international + 
               continental_o + country_o + continental_d + country_d +
               no_europe_o + no_australia_o + no_africa_o + no_north_america_o + no_south_america_o + no_asia_o +
               no_europe_d + no_australia_d + no_africa_d + no_north_america_d + no_south_america_d + no_asia_d +
               no_gdp_o + no_gdppc_o +no_pop_o + no_gdp_d + no_gdppc_d + no_pop_d + 
               no_colony + no_language + no_border + no_exchange + no_comcur + no_rta + no_price + no_whs + no_island + 
               no_climate + no_sea + no_politics + no_culture + no_religion + no_trade + no_migration + no_disease + 
               dist_se + 
               s(average_year) + s(pub_year) +
               (1 | study_id),
           iter = 5000, warmup = 2000, cores = 3, chains = 3,
           prior = c(prior(normal(-1, 1), class = Intercept),
                     prior(normal(0, 1), class = b),
                     prior(exponential(1), class = sd),
                     prior(exponential(1), class = sigma)),
           seed = 14,
           control = list(adapt_delta = 0.8,
                          max_treedepth = 14)
)

data_dom_int <- data %>% mutate(domestic = (continental_o == 0 & continental_d == 0 & intercontinental_o == 0 & intercontinental_d == 0))
m_dom_int <- brm(data = data_dom_int, family = student,
                        dist| se(dist_se,  sigma = TRUE) ~ 1 +  
                            ls + ml + neg_bin + other + zero_inflated + 
                            no_region_time_effects + fixed_effects_no_time + random_effects + no_panel + duration +
                            origin_constrained + destination_constrained + no_dom_international + impute_se + domestic + 
                            # continental_o + country_o + continental_d + country_d + 
                            # no_europe_o + no_australia_o + no_africa_o + no_north_america_o + no_south_america_o + no_asia_o +
                            # no_europe_d + no_australia_d + no_africa_d + no_north_america_d + no_south_america_d + no_asia_d +
                            # no_gdp_o + no_gdppc_o +no_pop_o + no_gdp_d + no_gdppc_d + no_pop_d + 
                            # no_colony + no_language + no_border + no_exchange + no_comcur + no_rta + no_price + no_whs + no_island + 
                            # no_climate + no_sea + no_politics + no_culture + no_religion + no_trade + no_migration + no_disease + 
                            dist_se + 
                            s(average_year) + s(pub_year) +
                            (1 | study_id),
                        iter = 5000, warmup = 2000, cores = 3, chains = 3,
                        prior = c(prior(normal(-1, 1), class = Intercept),
                                  prior(normal(0, 1), class = b),
                                  prior(exponential(1), class = sd),
                                  prior(exponential(1), class = sigma)),
                        seed = 14,
                        control = list(adapt_delta = 0.8,
                                       max_treedepth = 14)
)

m_2_time <- brm(data = data, family = student,
                   dist| se(dist_se,  sigma = TRUE) ~ 1 +  
                       ls + ml + neg_bin + other + zero_inflated + 
                       no_region_time_effects + fixed_effects_no_time + random_effects + no_panel + duration +
                       origin_constrained + destination_constrained + no_dom_international + impute_se + 
                       continental_o + country_o + continental_d + country_d +
                       no_europe_o + no_australia_o + no_africa_o + no_north_america_o + no_south_america_o + no_asia_o +
                       no_europe_d + no_australia_d + no_africa_d + no_north_america_d + no_south_america_d + no_asia_d +
                       dist_se + 
                       s(average_year) + s(pub_year) +
                       (1 | study_id),
                   iter = 5000, warmup = 2000, cores = 3, chains = 3,
                   prior = c(prior(normal(-1, 1), class = Intercept),
                             prior(normal(0, 1), class = b),
                             prior(exponential(1), class = sd),
                             prior(exponential(1), class = sigma)),
                   seed = 14,
                   control = list(adapt_delta = 0.8,
                                  max_treedepth = 14)
)

m_4_unilateral_a <- brm(data = data, family = student,
                        dist| se(dist_se,  sigma = TRUE) ~ 1 +  
                            ls + ml + neg_bin + other + zero_inflated + 
                            no_region_time_effects + fixed_effects_no_time + random_effects + no_panel + duration +
                            origin_constrained + destination_constrained + no_dom_international + impute_se + 
                            continental_o + country_o + continental_d + country_d +
                            no_europe_o + no_australia_o + no_africa_o + no_north_america_o + no_south_america_o + no_asia_o +
                            no_europe_d + no_australia_d + no_africa_d + no_north_america_d + no_south_america_d + no_asia_d +
                            no_gdp_o + no_gdppc_o +no_pop_o + no_gdp_d + no_gdppc_d + no_pop_d + 
                            dist_se + 
                            s(average_year) + s(pub_year) +
                            (1 | study_id),
                        iter = 5000, warmup = 2000, cores = 3, chains = 3,
                        prior = c(prior(normal(-1, 1), class = Intercept),
                                  prior(normal(0, 1), class = b),
                                  prior(exponential(1), class = sd),
                                  prior(exponential(1), class = sigma)),
                        seed = 14,
                        control = list(adapt_delta = 0.8,
                                       max_treedepth = 14)
)

m_3_unilateral_a <- brm(data = data, family = student,
                        dist| se(dist_se,  sigma = TRUE) ~ 1 +  
                            ls + ml + neg_bin + other + zero_inflated + 
                            no_region_time_effects + fixed_effects_no_time + random_effects + no_panel + duration +
                            origin_constrained + destination_constrained + no_dom_international + impute_se + 
                            continental_o + country_o + continental_d + country_d +
                            no_europe_o + no_australia_o + no_africa_o + no_north_america_o + no_south_america_o + no_asia_o +
                            no_europe_d + no_australia_d + no_africa_d + no_north_america_d + no_south_america_d + no_asia_d +
                            no_gdp_o + no_gdppc_o +no_pop_o + no_gdp_d + no_gdppc_d + no_pop_d + 
                            dist_se + 
                            (1 | study_id),
                        iter = 5000, warmup = 2000, cores = 3, chains = 3,
                        prior = c(prior(normal(-1, 1), class = Intercept),
                                  prior(normal(0, 1), class = b),
                                  prior(exponential(1), class = sd),
                                  prior(exponential(1), class = sigma)),
                        seed = 14,
                        control = list(adapt_delta = 0.8,
                                       max_treedepth = 14)
)

m_4_unilateral_b <- brm(data = data, family = student,
                        dist| se(dist_se,  sigma = TRUE) ~ 1 +  
                            ls + ml + neg_bin + other + zero_inflated + 
                            no_region_time_effects + fixed_effects_no_time + random_effects + no_panel + duration +
                            origin_constrained + destination_constrained + no_dom_international + impute_se + 
                            continental_o + country_o + continental_d + country_d +
                            no_europe_o + no_australia_o + no_africa_o + no_north_america_o + no_south_america_o + no_asia_o +
                            no_europe_d + no_australia_d + no_africa_d + no_north_america_d + no_south_america_d + no_asia_d +
                            no_gdp_o + no_gdppc_o +no_pop_o + no_gdp_d + no_gdppc_d + no_pop_d + 
                            no_whs + no_island + 
                            no_climate + no_sea + no_disease + 
                            dist_se + 
                            s(average_year) + s(pub_year) +
                            (1 | study_id),
                        iter = 5000, warmup = 2000, cores = 3, chains = 3,
                        prior = c(prior(normal(-1, 1), class = Intercept),
                                  prior(normal(0, 1), class = b),
                                  prior(exponential(1), class = sd),
                                  prior(exponential(1), class = sigma)),
                        seed = 14,
                        control = list(adapt_delta = 0.8,
                                       max_treedepth = 14)
)

m_4_bilateral <- brm(data = data, family = student,
                        dist| se(dist_se,  sigma = TRUE) ~ 1 +  
                            ls + ml + neg_bin + other + zero_inflated + 
                            no_region_time_effects + fixed_effects_no_time + random_effects + no_panel + duration +
                            origin_constrained + destination_constrained + no_dom_international + impute_se + 
                            continental_o + country_o + continental_d + country_d +
                            no_europe_o + no_australia_o + no_africa_o + no_north_america_o + no_south_america_o + no_asia_o +
                            no_europe_d + no_australia_d + no_africa_d + no_north_america_d + no_south_america_d + no_asia_d +
                            no_colony + no_language + no_border + no_exchange + no_comcur + no_rta + no_price +
                            no_politics + no_culture + no_religion + no_trade + no_migration + 
                            dist_se + 
                            s(average_year) + s(pub_year) +
                            (1 | study_id),
                        iter = 5000, warmup = 2000, cores = 3, chains = 3,
                        prior = c(prior(normal(-1, 1), class = Intercept),
                                  prior(normal(0, 1), class = b),
                                  prior(exponential(1), class = sd),
                                  prior(exponential(1), class = sigma)),
                        seed = 14,
                        control = list(adapt_delta = 0.8,
                                       max_treedepth = 14)
)

m_4_income <- brm(data = data, family = student,
           dist| se(dist_se,  sigma = TRUE) ~ 1 +  
               ls + ml + neg_bin + other + zero_inflated + 
               no_region_time_effects + fixed_effects_no_time + random_effects + no_panel + duration +
               origin_constrained + destination_constrained + no_dom_international + impute_se + 
               continental_o + country_o + continental_d + country_d +
               no_low_o + no_medium_o + no_high_o + 
               no_low_d + no_medium_d + no_high_d + 
               #no_gdp_o + no_gdppc_o + no_pop_o + no_gdp_d + no_gdppc_d + no_pop_d + 
               dist_se + 
               s(average_year) + s(pub_year) +
               (1 | study_id),
           iter = 5000, warmup = 2000, cores = 3, chains = 3,
           prior = c(prior(normal(-1, 1), class = Intercept),
                     prior(normal(0, 1), class = b),
                     prior(exponential(1), class = sd),
                     prior(exponential(1), class = sigma)),
           seed = 14,
           control = list(adapt_delta = 0.8,
                          max_treedepth = 14)
)

post <- as_draws_df(m_4_income)

post1 <- post %>% 
    mutate(`Low income` = b_Intercept + b_no_medium_o + b_no_high_o,
           `Medium income` = b_Intercept + b_no_low_o + b_no_high_o, 
           `High income` = b_Intercept + b_no_low_o + b_no_medium_o,
           `Difference high-medium` = `High income` - `Medium income`,
           `Difference low-medium` = `Low income` - `Medium income`) %>%
    select(`Low income`, `Medium income`, `High income`, `Difference high-medium`, `Difference low-medium`) %>%
    pivot_longer(1:5) %>%
    mutate(region = "Origin")

post2 <- post %>% 
    mutate(`Low income` = b_Intercept + b_no_medium_d + b_no_high_d,
           `Medium income` = b_Intercept + b_no_low_d + b_no_high_d, 
           `High income` = b_Intercept + b_no_low_d + b_no_medium_d, 
           `Difference high-medium` = `High income` - `Medium income`,
           `Difference low-medium` = `Low income` - `Medium income`) %>%
    select(`Low income`, `Medium income`, `High income`, `Difference high-medium`, `Difference low-medium`) %>%
    pivot_longer(1:5) %>%
    mutate(region = "Destination")

post_combined = rbind(post1, post2)

plot <- post_combined %>% 
    mutate(name = fct_relevel(name, "Low income", "Difference low-medium", "Medium income" , "Difference high-medium", "High income" )) %>%
    ggplot(aes(x = value, y = name, fill = name)) +
    stat_halfeye(point_interval = median_qi, .width = .95,
                 color = "sienna4" )  + 
    scale_fill_manual(values = c("salmon1", "steelblue1", "salmon2","steelblue4", "salmon3" )) +
    facet_wrap(. ~ region, ncol = 2) +
    theme_ipsum() +
    theme(legend.position = "none") +
    labs(subtitle = "",
         x = "Posterior distribution", 
         y = NULL) +
    theme(plot.title = element_text(size=12), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
    geom_vline(xintercept = 0, linetype="dotted", color = "black", linewidth=0.5) +
    geom_vline(xintercept = -1, linetype="dotted", color = "black", linewidth=0.5) + 
    geom_vline(xintercept = -2, linetype="dotted", color = "black", linewidth=0.5) + 
    geom_vline(xintercept = 0.5, linetype="dotted", color = "black", linewidth=0.2) +
    geom_vline(xintercept = -0.5, linetype="dotted", color = "black", linewidth=0.2) + 
    geom_vline(xintercept = -1.5, linetype="dotted", color = "black", linewidth=0.2)  

plot

figure_path <- here::here("figures","income.pdf")

cairo_pdf(file = figure_path, width = 7.5, height = 4)
plot
dev.off()

post1 <- post %>% 
    mutate(`Low income` = b_Intercept + b_no_medium_o + b_no_high_o,
           `Medium income` = b_Intercept + b_no_low_o + b_no_high_o, 
           `High income` = b_Intercept + b_no_low_o + b_no_medium_o, 
           `Difference High-Medium` = `High income` - `Medium income`,
           `Difference Low-Medium` = `Low income` - `Medium income`) %>%
    select(`Low income`, `Medium income`, `High income`, `Difference High-Medium`, `Difference Low-Medium`) %>%
    summarise(
        mean_h = mean(`High income`),
        st_error_h = sd(`High income`),
        mean_m = mean(`Medium income`),
        st_error_m = sd(`Medium income`),
        mean_l = mean(`Low income`),
        st_error_l = sd(`Low income`)
    )

post2 <- post %>% 
    mutate(`Low income` = b_Intercept + b_no_medium_d + b_no_high_d,
           `Medium income` = b_Intercept + b_no_low_d + b_no_high_d, 
           `High income` = b_Intercept + b_no_low_d + b_no_medium_d, 
           `Difference High-Medium` = `High income` - `Medium income`,
           `Difference Low-Medium` = `Low income` - `Medium income`) %>%
    select(`Low income`, `Medium income`, `High income`, `Difference High-Medium`, `Difference Low-Medium`) %>%
    summarise(
        mean_h = mean(`High income`),
        st_error_h = sd(`High income`),
        mean_m = mean(`Medium income`),
        st_error_m = sd(`Medium income`),
        mean_l = mean(`Low income`),
        st_error_l = sd(`Low income`)
    )

post <- as_draws_df(m_dom_int)

post1 <- post %>% 
    mutate(`International` = b_Intercept,
           `Domestic` = b_Intercept + b_domesticTRUE, 
           `Difference international-domestic` = `International` - `Domestic`) %>%
    select(`International`, `Domestic`, `Difference international-domestic`) %>%
    pivot_longer(1:3) 

plot <- post1 %>% 
    mutate(name = fct_relevel(name, "Domestic", "Difference international-domestic", "International" )) %>%
    ggplot(aes(x = value, y = name, fill = name)) +
    stat_halfeye(point_interval = median_qi, .width = .95,
                 color = "sienna4" )  + 
    scale_fill_manual(values = c("salmon1", "steelblue1", "salmon2")) +
    theme_ipsum() +
    theme(legend.position = "none") +
    labs(subtitle = "",
         x = "Posterior distribution", 
         y = NULL) +
    theme(plot.title = element_text(size=12), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
    geom_vline(xintercept = 0, linetype="dotted", color = "black", linewidth=0.5) +
    geom_vline(xintercept = -1, linetype="dotted", color = "black", linewidth=0.5) + 
    geom_vline(xintercept = -2, linetype="dotted", color = "black", linewidth=0.5) + 
    geom_vline(xintercept = 0.5, linetype="dotted", color = "black", linewidth=0.2) +
    geom_vline(xintercept = -0.5, linetype="dotted", color = "black", linewidth=0.2) + 
    geom_vline(xintercept = -1.5, linetype="dotted", color = "black", linewidth=0.2)  

plot

figure_path <- here::here("figures","domestic_international.pdf")

cairo_pdf(file = figure_path, width = 7.5, height = 3)
plot
dev.off()

output_path <- here::here("output", "full_models.Rdata")
load(output_path)
post <- as_draws_df(m_4)

post1 <- post %>% 
    mutate(`Island` = b_Intercept,
           `Continuous` = b_Intercept + b_no_island, 
           `Difference island-continuous` = `Island` - `Continuous`) %>%
    select(`Island`, `Continuous`, `Difference island-continuous`) %>%
    pivot_longer(1:3) 

plot <- post1 %>% 
    mutate(name = fct_relevel(name, "Island", "Difference island-continuous", "Continuous" )) %>%
    ggplot(aes(x = value, y = name, fill = name)) +
    stat_halfeye(point_interval = median_qi, .width = .95,
                 color = "sienna4" )  + 
    scale_fill_manual(values = c("salmon1", "steelblue1", "salmon2")) +
    theme_ipsum() +
    theme(legend.position = "none") +
    labs(subtitle = "",
         x = "Posterior distribution", 
         y = NULL) +
    theme(plot.title = element_text(size=12), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
    geom_vline(xintercept = 0, linetype="dotted", color = "black", linewidth=0.5) +
    geom_vline(xintercept = -1, linetype="dotted", color = "black", linewidth=0.5) + 
    geom_vline(xintercept = -2, linetype="dotted", color = "black", linewidth=0.5) + 
    geom_vline(xintercept = 0.5, linetype="dotted", color = "black", linewidth=0.2) +
    geom_vline(xintercept = -0.5, linetype="dotted", color = "black", linewidth=0.2) + 
    geom_vline(xintercept = -1.5, linetype="dotted", color = "black", linewidth=0.2)  

plot

figure_path <- here::here("figures","island.pdf")

cairo_pdf(file = figure_path, width = 10, height = 4)
plot
dev.off()

data_path <- here::here("output", "full_models.RData")

load(data_path)
model_weights(m_2, m_4_income, weights = "loo")
model_weights(m_1, m_2, m_2_time, m_3, m_3_unilateral_a, m_4, m_4_unilateral_a, m_4_unilateral_b, m_4_bilateral, weights = "loo")

model_path <- here::here("./output/", "sensitivity_models.RData")
save(m_2_time, m_3_unilateral_a, m_4_unilateral_a, m_4_unilateral_b, m_4_bilateral, m_4_income, file = model_path)
