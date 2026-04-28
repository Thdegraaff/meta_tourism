set.seed(100)

library("here")
library("brms")
library("tidyverse")

data_path <- here::here("data", "data.Rda")
load(data_path)

stderror <- function(x) sd(x)/sqrt(length(x))

output_path <- here::here("output", "intercept_only.Rdata")
load(output_path)

fit_0 <- fitted(m_0)
fit_1 <- fitted(m_0_varying_effects)
fit_2 <- fitted(m_0_me)
fit_3 <- fitted(m_0_student)

summary(fit_0)
summary(fit_1)
summary(fit_2)
summary(fit_3)

############### Publication bias models

output_path <- here::here("output", "pubbias.Rdata")
load(output_path)

fit_0 <- fitted(m_0_student)
fit_1 <- fitted(m_0_student_a)
fit_2 <- fitted(m_0_student_b)
fit_3 <- fitted(m_0_student_c)

summary(fit_0)
summary(fit_1)
summary(fit_2)
summary(fit_3)


############## Full benchmark models

output_path <- here::here("output", "full_models.Rdata")
load(output_path)

newdata_1 <- data %>%
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
        impute_se = 0
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
        no_pop_d = 0
    )
newdata_4 <- newdata_3 %>%
    mutate(
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

fit_0 <- fitted(m_0, newdata = newdata_1)
fit_1 <- fitted(m_1, newdata = newdata_1)
fit_2 <- fitted(m_2, newdata = newdata_2)
fit_3 <- fitted(m_3, newdata = newdata_3)
fit_4 <- fitted(m_4, newdata = newdata_4)

summary(fit_0)
summary(fit_1)
summary(fit_2)
summary(fit_3)
summary(fit_4)

plot(density(fit_0[ ,1]))
plot(density(fit_1[ ,1]))
plot(density(fit_2[ ,1]))
plot(density(fit_3[ ,1]))
plot(density(fit_4[ ,1]))

############# For sensitivity models

output_path <- here::here("output", "sensitivity_models.Rdata")
load(output_path)

newsensdata_1 <- newdata_2 %>%
    mutate(
        no_gdp_o = 0,
        no_gdppc_o = 0, 
        no_pop_o = 0, 
        no_gdp_d = 0, 
        no_gdppc_d = 0,
        no_pop_d = 0
    )

newsensdata_2 <- newsensdata_1 %>%
    mutate(
        no_gdp_o = 0,
        no_gdppc_o = 0, 
        no_pop_o = 0, 
        no_gdp_d = 0, 
        no_gdppc_d = 0,
        no_pop_d = 0,
        no_whs = 0, 
        no_island = 0,  
        no_climate = 0,
        no_sea = 0, 
        no_disease = 0
    )

newsensdata_3 <- newsensdata_1 %>%
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
        no_politics = 0, 
        no_culture = 0, 
        no_religion = 0, 
        no_trade = 0, 
        no_migration = 0
    )

fitsens_1 <- predict(m_2_time, newdata = newdata_2, allow_new_levels = TRUE, robust = TRUE)
fitsens_2 <- predict(m_3_unilateral_a, newdata = newsensdata_1, allow_new_levels = TRUE, robust = TRUE)
fitsens_3 <- predict(m_4_unilateral_a, newdata = newsensdata_1, allow_new_levels = TRUE, robust = TRUE)
fitsens_4 <- predict(m_4_unilateral_b, newdata = newsensdata_2, allow_new_levels = TRUE, robust = TRUE)
fitsens_5 <- predict(m_4_bilateral, newdata = newsensdata_3, allow_new_levels = TRUE, robust = TRUE)

summary(fit_2)
summary(fitsens_1) # Model 2 + time
summary(fitsens_2) # Model 2 + gravity variables (GDP/GDPpc/Pop)
summary(fitsens_3) # Model 2 + gravity variables (GDP/GDPpc/Pop) + time
summary(fitsens_4) # Model 2 + gravity variables (GDP/GDPpc/Pop) + time + other unilateral variables
summary(fitsens_5) # Model 2 + gravity variables (GDP/GDPpc/Pop) + time + bilateral variables
summary(fit_4)

stderror(fitsens_1)
stderror(fitsens_2)
stderror(fitsens_3)
stderror(fitsens_4)
stderror(fitsens_5)

plot(density(fitsens_1[ ,1]))
plot(density(fitsens_2[ ,1]))
plot(density(fitsens_3[ ,1]))
plot(density(fitsens_4[ ,1]))
plot(density(fitsens_5[ ,1]))
