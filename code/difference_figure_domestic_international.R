library("hrbrthemes")
library("tidyverse")
library("here")
library("gridExtra")
library("brms")
library("tidybayes")

file_path <- here::here("data", "data.Rda")
load(file_path)
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

newdata_5 <- newdata_4 %>%
    mutate(
        country_o = 1,
        country_d = 1
    )

fit_international <- predict(m_4, newdata = newdata_4, allow_new_levels = TRUE, robust = TRUE)
fit_domestic <- predict(m_4, newdata = newdata_5, allow_new_levels = TRUE, robust = TRUE)

summary(fit_domestic)
summary(fit_international)


post <- as_draws_df(m_4)

post1 <- post %>% 
    mutate(`International` = b_Intercept + b_continental_o + b_continental_d,
           `Domestic` = b_Intercept + b_country_o + b_country_d, 
           `Difference International-Domestic` = `International` - `Domestic`) %>%
    select(`International`, `Domestic`, `Difference International-Domestic`) %>%
    pivot_longer(1:3) 


plot <- post1 %>% 
    mutate(name = fct_relevel(name, "International", "Difference International-Domestic", "Domestic")) %>%
    ggplot(aes(x = value, y = name, fill = name)) +
    stat_halfeye(point_interval = median_qi, .width = .95,
                 color = "sienna4" )  + 
    scale_fill_manual(values = c("salmon1", "steelblue1", "salmon2")) +
    theme_ipsum() +
    theme(legend.position = "none") +
    labs(subtitle = "",
         x = "Posterior mean distance decay", 
         y = NULL) +
    theme(plot.title = element_text(size=12), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
    geom_vline(xintercept = 0, linetype="dotted", color = "black", linewidth=0.5) +
    geom_vline(xintercept = 0.5, linetype="dotted", color = "black", linewidth=0.2) +
    geom_vline(xintercept = -0.5, linetype="dotted", color = "black", linewidth=0.2) + 
    geom_vline(xintercept = -1.5, linetype="dotted", color = "black", linewidth=0.2)  

plot

