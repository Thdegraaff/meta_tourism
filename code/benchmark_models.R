set.seed(100)

library("here")
library("brms")

data_path <- here::here("data","data.Rda")
load(file = data_path)

m_0 <- brm(data = data, family = student,
                   dist| se(dist_se,  sigma = TRUE)  ~ 1 + (1 | study_id) + dist_se, 
                   iter = 5000, warmup = 2000, cores = 3, chains = 3,
                   prior = c(prior(normal(-1, 2), class = Intercept),
                             prior(exponential(1), class = sd),
                             prior(exponential(1), class = sigma)),
                   seed = 14,
                   control = list(adapt_delta = 0.8,
                                  max_treedepth = 14)
)

m_1 <- brm(data = data, family = student,
           dist| se(dist_se,  sigma = TRUE) ~ 1 +  
               ls + ml + neg_bin + other + zero_inflated + 
               no_region_time_effects + fixed_effects_no_time + random_effects + no_panel + duration +
               origin_constrained + destination_constrained + no_dom_international + impute_se + 
               dist_se + 
               (1 | study_id),
           iter = 5000, warmup = 2000, cores = 3, chains = 3,
           prior = c(prior(normal(-1, 2), class = Intercept),
                     prior(normal(0, 2), class = b),
                     prior(exponential(1), class = sd),
                     prior(exponential(1), class = sigma)),
           seed = 14,
           control = list(adapt_delta = 0.8,
                          max_treedepth = 14)
)

m_2 <- brm(data = data, family = student,
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
                prior = c(prior(normal(-1, 2), class = Intercept),
                          prior(normal(0, 2), class = b),
                          prior(exponential(1), class = sd),
                          prior(exponential(1), class = sigma)),
                seed = 14,
                control = list(adapt_delta = 0.8,
                               max_treedepth = 14)
)

m_3 <- brm(data = data, family = student,
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
                        prior = c(prior(normal(-1, 2), class = Intercept),
                                  prior(normal(0, 2), class = b),
                                  prior(exponential(1), class = sd),
                                  prior(exponential(1), class = sigma)),
                        seed = 14,
                        control = list(adapt_delta = 0.8,
                                       max_treedepth = 14)
)

m_4 <- brm(data = data, family = student,
           dist| se(dist_se,  sigma = TRUE) ~ 1 +  
               ls + ml + neg_bin + other + zero_inflated + 
               no_region_time_effects + fixed_effects_no_time + random_effects + no_panel + duration +
               origin_constrained + destination_constrained + no_dom_international + impute_se + 
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
           prior = c(prior(normal(-1, 2), class = Intercept),
                     prior(normal(0, 2), class = b),
                     prior(exponential(1), class = sd),
                     prior(exponential(1), class = sigma)),
           seed = 14,
           control = list(adapt_delta = 0.8,
                          max_treedepth = 14)
)

m_4_a <- brm(data = data,
           dist ~ 1 +  
               ls + ml + neg_bin + other + zero_inflated + 
               no_region_time_effects + fixed_effects_no_time + random_effects + no_panel + duration +
               origin_constrained + destination_constrained + no_dom_international + impute_se + 
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
           prior = c(prior(normal(-1, 2), class = Intercept),
                     prior(normal(0, 2), class = b),
                     prior(exponential(1), class = sd),
                     prior(exponential(1), class = sigma)),
           seed = 14,
           control = list(adapt_delta = 0.8,
                          max_treedepth = 14)
)

m_4_b <- brm(data = data, 
             dist| se(dist_se,  sigma = TRUE) ~ 1 +  
                 ls + ml + neg_bin + other + zero_inflated + 
                 no_region_time_effects + fixed_effects_no_time + random_effects + no_panel + duration +
                 origin_constrained + destination_constrained + no_dom_international + impute_se + 
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
             prior = c(prior(normal(-1, 2), class = Intercept),
                       prior(normal(0, 2), class = b),
                       prior(exponential(1), class = sd),
                       prior(exponential(1), class = sigma)),
             seed = 14,
             control = list(adapt_delta = 0.8,
                            max_treedepth = 14)
)

model_weights(m_0, m_1, m_2, m_3, m_4, weights = "loo")

model_path <- here::here("./output/", "full_models.RData")
save(m_0, m_1, m_2, m_3, m_4, m_4_a, m_4_b, file = model_path)
