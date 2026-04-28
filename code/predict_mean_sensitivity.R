set.seed(14)

load("./data/data.Rda")
data_no_impute <- data %>% filter(impute_se == 0)

stderror <- function(x) sd(x)/sqrt(length(x))

load("./output/sensitivity_models.RData")

newdata_1 <- data_no_impute %>%
    mutate(
        ls = 0, 
        ml = 0, 
        neg_bin = 0,
        other = 0,
        zero_inflated = 0,
        no_region_time_effects = 0,
        fixed_effects_no_time = 0,
        no_dom_international = 0,
        random_effects = 0,
        no_panel = 0,
        duration = 0,
        origin_constrained = 0,
        destination_constrained = 0,
    )

newdata_2 <- newdata_1 %>%
    mutate(
        continental_o = 0,
        country_o = 0,
        continental_d = 0,
        country_d = 0,
        no_europe_o = 0,
        no_australia_o = 0,
        no_africa_o = 0,
        no_north_america_o = 0,
        no_south_america_o = 0,
        no_asia_o = 0,
        no_europe_d = 0,
        no_australia_d = 0,
        no_africa_d = 0,
        no_north_america_d = 0,
        no_south_america_d = 0,
        no_asia_d = 0
    )
newdata_3 <- newdata_2 %>%
    mutate(
        no_gdp_o = 0,
        no_gdppc_o = 0, 
        no_pop_o = 0, 
        no_gdp_d = 0, 
        no_gdppc_d = 0,
        no_pop_d = 0, 
        no_colony = 0,
        no_language = 0,
        no_border = 0, 
        no_exchange = 0,
        no_comcur = 0, 
        no_rta = 0, 
        no_price =0, 
        no_whs = 0, 
        no_island = 0,  
        no_climate = 0,
        no_sea = 0, 
        no_politics = 0, 
        no_culture = 0, 
        no_religion = 0, 
        no_trade = 0, 
        no_migration = 0,
        no_disease = 0 
    )

fit_1 <- predict(m_1_no_impute, newdata = newdata_1, robust = TRUE)
fit_2 <- predict(m_2_no_impute, newdata = newdata_2, robust = TRUE)
fit_3 <- predict(m_3_no_impute, newdata = newdata_3, robust = TRUE)
fit_4 <- predict(m_4_no_impute, newdata = newdata_3, robust = TRUE)

summary(fit_1)
summary(fit_2)
summary(fit_3)
summary(fit_4)

stderror(fit_1)
stderror(fit_2)
stderror(fit_3)
stderror(fit_4)

plot(density(fit_1[ ,1]))
plot(density(fit_2[ ,1]))
plot(density(fit_3[ ,1]))
plot(density(fit_4[ ,1]))