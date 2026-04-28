set.seed(100)

library("hrbrthemes")
library("tidyverse")
library("here")
library("gridExtra")
library("brms")
library("tidybayes")


data_path <- here::here("data","data.Rda")
load(file = data_path)


#################### International tourism

data <- data %>% filter(
    continental_o == 1 | continental_d == 1 | intercontinental_o == 1 | intercontinental_d == 1
)

m_int <- brm(data = data, family = student,
           dist| se(dist_se,  sigma = TRUE) ~ 1 +  
               ls + ml + neg_bin + other + zero_inflated + 
               no_region_time_effects + fixed_effects_no_time + random_effects + no_panel + duration +
               origin_constrained + destination_constrained + no_dom_international + impute_se + 
               # continental_o + country_o + continental_d + country_d +
               # no_europe_o + no_australia_o + no_africa_o + no_north_america_o + no_south_america_o + no_asia_o +
               # no_europe_d + no_australia_d + no_africa_d + no_north_america_d + no_south_america_d + no_asia_d +
               # no_gdp_o + no_gdppc_o +no_pop_o + no_gdp_d + no_gdppc_d + no_pop_d + 
               # no_colony + no_language + no_border + no_exchange + no_comcur + no_rta + no_price + no_whs + no_island + 
               # no_climate + no_sea + no_politics + no_culture + no_religion + no_trade + no_migration + no_disease + 
               dist_se + 
               s(average_year) + 
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

newdata_1 <- data %>%
    mutate(
        ls = 0, 
        ml = 0, 
        neg_bin = 0,
        other = 0,
        zero_inflated = 0,
        no_region_time_effects = 0,
        fixed_effects_no_time = 0,
        random_effects = 0,
        no_panel = 0,
        duration = 0,
        origin_constrained = 0,
        destination_constrained = 0,
        no_dom_international = 0,
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

fit_int <- fitted(m_int, newdata = newdata_3, allow_new_levels = TRUE)
stderror <- function(x) sd(x)/sqrt(length(x))

summary(fit_int)
stderror(fit_int)

load(file = data_path)

data <- data %>% filter(
    continental_o == 1 | continental_d == 1 | intercontinental_o == 1 | intercontinental_d == 1
)

spline <- conditional_smooths(m_int)
effect_size <- -0.81

year_mid.df <- data.frame(spline$`mu: s(average_year)`)
year_mid.df$estimate__ <- year_mid.df$estimate__ + effect_size
year_mid.df$lower__ <- year_mid.df$lower__ + effect_size
year_mid.df$upper__ <- year_mid.df$upper__ + effect_size

### Time trend for international tourists
plot_int <- ggplot(year_mid.df, aes(average_year, estimate__)) + geom_line(color = "cornflowerblue", size = 1.25) +
    geom_ribbon(aes(ymin=lower__, ymax=upper__), linetype=2, alpha=0.20, fill = "cornflowerblue") + 
    ylab("Distance-decay effect") + xlab("Year of data (average)")  +
    geom_hline(yintercept = effect_size, linetype = 'dotted', color = "sienna", size = 0.75) +
    geom_point(data = data, aes(x = average_year, y = dist, alpha = 0.7, colour = "tomato"), size = 1) +
    theme_ipsum() +
    theme(legend.position = "none") +
    ggtitle("International tourism") + 
    ylim(-4, 1) +
    theme(plot.title = element_text(size=12), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))

#################### Domestic tourism

load(file = data_path)

data <- data %>% filter(
    continental_o == 0 & continental_d == 0 & intercontinental_o == 0 & intercontinental_d == 0
)

m_dom <- brm(data = data, family = student,
             dist| se(dist_se,  sigma = TRUE) ~ 1 +  
                 ls + ml + neg_bin + other + zero_inflated + 
                 no_region_time_effects + fixed_effects_no_time + random_effects + no_panel + duration +
                 origin_constrained + destination_constrained + no_dom_international + impute_se + 
                 # continental_o + country_o + continental_d + country_d +
                 # no_europe_o + no_australia_o + no_africa_o + no_north_america_o + no_south_america_o + no_asia_o +
                 # no_europe_d + no_australia_d + no_africa_d + no_north_america_d + no_south_america_d + no_asia_d +
                 # no_gdp_o + no_gdppc_o +no_pop_o + no_gdp_d + no_gdppc_d + no_pop_d + 
                 # no_colony + no_language + no_border + no_exchange + no_comcur + no_rta + no_price + no_whs + no_island + 
                 # no_climate + no_sea + no_politics + no_culture + no_religion + no_trade + no_migration + no_disease + 
                 dist_se + 
                 s(average_year) +
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

newdata_1 <- data %>%
    mutate(
        ls = 0, 
        ml = 0, 
        neg_bin = 0,
        other = 0,
        zero_inflated = 0,
        no_region_time_effects = 0,
        fixed_effects_no_time = 0,
        random_effects = 0,
        no_panel = 0,
        duration = 0,
        origin_constrained = 0,
        destination_constrained = 0,
        no_dom_international = 0,
        impute_se = 0
    )

newdata_2 <- newdata_1 %>%
    mutate(
        continental_o = 0,
        country_o = 1,
        continental_d = 0,
        country_d = 1,
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

fit_dom <- fitted(m_dom, newdata = newdata_3)
stderror <- function(x) sd(x)/sqrt(length(x))

summary(fit_dom)
stderror(fit_dom)

spline <- conditional_smooths(m_dom)
effect_size <- -0.81

year_mid.df <- data.frame(spline$`mu: s(average_year)`)
year_mid.df$estimate__ <- year_mid.df$estimate__ + effect_size
year_mid.df$lower__ <- year_mid.df$lower__ + effect_size
year_mid.df$upper__ <- year_mid.df$upper__ + effect_size

### Time trend for domestic tourists
plot_dom <- ggplot(year_mid.df, aes(average_year, estimate__)) + geom_line(color = "cornflowerblue", size = 1.25) +
    geom_ribbon(aes(ymin=lower__, ymax=upper__), linetype=2, alpha=0.20, fill = "cornflowerblue") + 
    ylab("Distance-decay effect") + xlab("Year of data (average)")  +
    geom_hline(yintercept = effect_size, linetype = 'dotted', color = "sienna", size = 0.75) +
    geom_point(data = data, aes(x = average_year, y = dist, alpha = 0.7, colour = "tomato"), size = 1) +
    theme_ipsum() +
    theme(legend.position = "none") +
    ggtitle("Domestic tourism") + 
    ylim(-4, 1) +
    theme(plot.title = element_text(size=12), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))

figure_path <- here::here("figures", "time_trends_int_dom.pdf")

cairo_pdf(file = figure_path, width = 7.5, height = 3)
grid.arrange(plot_int, plot_dom, ncol = 2)
dev.off()

