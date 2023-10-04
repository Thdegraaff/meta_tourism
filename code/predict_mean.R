set.seed(14)

load("./data/data.Rda")
stderror <- function(x) sd(x)/sqrt(length(x))

load("./output/intercept_only.RData")

fit_0 <- predict(m_0)
fit_1 <- predict(m_0_varying_effects)
fit_2 <- predict(m_0_me)
fit_3 <- predict(m_0_student)

summary(fit_0)
summary(fit_1)
summary(fit_2)
summary(fit_3)

stderror(fit_0)
stderror(fit_1)
stderror(fit_2)
stderror(fit_3)

load("./output/pubbias.RData")

fit_0 <- predict(m_0_student)
fit_1 <- predict(m_0_student_a)
fit_2 <- predict(m_0_student_b)
fit_3 <- predict(m_0_student_c)

summary(fit_0)
summary(fit_1)
summary(fit_2)
summary(fit_3)

stderror(fit_0)
stderror(fit_1)
stderror(fit_2)
stderror(fit_3)

load("./output/full_models.RData")

newdata_1 <- data %>%
    mutate(
        ls = 0, 
        ml = 0, 
        neg_bin = 0,
        other = 0,
        zero_inflated = 0,
        no_individual_effects = 0,
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
        no_asia = 0,
        no_europe_d = 0,
        no_australia_d = 0,
        no_africa_d = 0,
        no_north_america_d = 0,
        no_south_america_d = 0,
        no_asia = 0
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


fit_0 <- predict(m_0, newdata = newdata_1)
fit_1 <- predict(m_1, newdata = newdata_1)
fit_2 <- predict(m_2, newdata = newdata_2)
fit_3 <- predict(m_3, newdata = newdata_3)
fit_4 <- predict(m_4, newdata = newdata_3)

summary(fit_0)
summary(fit_1)
summary(fit_2)
summary(fit_3)
summary(fit_4)

stderror <- function(x) sd(x)/sqrt(length(x))

stderror(fit_0)
stderror(fit_1)
stderror(fit_2)
stderror(fit_3)
stderror(fit_4)

plot(density(fit_0[ ,1]))
plot(density(fit_1[ ,1]))
plot(density(fit_2[ ,1]))
plot(density(fit_3[ ,1]))
plot(density(fit_4[ ,1]))

load("./output/full_models.RData")

newdata_1 <- data %>%
    mutate(
        ls = 0, 
        ml = 0, 
        neg_bin = 0,
        other = 0,
        zero_inflated = 0,
        no_individual_effects = 0,
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
        no_asia = 0,
        no_europe_d = 0,
        no_australia_d = 0,
        no_africa_d = 0,
        no_north_america_d = 0,
        no_south_america_d = 0,
        no_asia = 0
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


fit_0 <- predict(m_0, newdata = newdata_1)
fit_1 <- predict(m_1, newdata = newdata_1)
fit_2 <- predict(m_2, newdata = newdata_2)
fit_3 <- predict(m_3, newdata = newdata_3)
fit_4 <- predict(m_4, newdata = newdata_3)

summary(fit_0)
summary(fit_1)
summary(fit_2)
summary(fit_3)
summary(fit_4)

stderror <- function(x) sd(x)/sqrt(length(x))

stderror(fit_0)
stderror(fit_1)
stderror(fit_2)
stderror(fit_3)
stderror(fit_4)

plot(density(fit_0[ ,1]))
plot(density(fit_1[ ,1]))
plot(density(fit_2[ ,1]))
plot(density(fit_3[ ,1]))
plot(density(fit_4[ ,1]))