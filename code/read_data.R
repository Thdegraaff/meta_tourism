data <- read_excel("./data/data_thomas.xlsx", sheet = "Data")

data <- data %>%
    filter(pub_year >= 2000) %>%
    mutate(
        duration = 1 - arrivals, 
        no_individual_effects = 1 - fixed_effects - random_effects,
        no_panel = 1 - panel, 
        no_gdp_d = 1 - gdp_d,
        no_gdppc_d = 1 - gdp_pp_d,
        no_pop_d = 1 - pop_d,
        no_gdp_o = 1 - gdp_o,
        no_gdppc_o = 1 - gdp_pp_o,
        no_pop_o = 1 - pop_o,
        country_o = 1 - intercontinental_o - continental_o,
        country_d = 1 - intercontinental_d - continental_d, 
        no_europe_o = 1 - europe_o,
        no_australia_o = 1 - australia_o,
        no_africa_o = 1 - africa_o,
        no_north_america_o = 1 - north_america_o,
        no_south_america_o = 1 - south_america_o,
        no_asia_o = 1 - asia_o,
        no_europe_d = 1 - europe_d,
        no_australia_d = 1 - australia_d,
        no_africa_d = 1 - africa_d,
        no_north_america_d = 1 - north_america_d,
        no_south_america_d = 1 - south_america_d,
        no_asia_d = 1 - asia_d,
        pr_ex = if_else( (exchange == 1 | price == 1 ), 1, 0),
        no_colony = 1 - colony, 
        no_language = 1 - language, 
        no_border = 1 - border, 
        no_exchange = 1 - exchange,
        no_comcur = 1 - comcur,
        no_rta = 1 - rta,
        no_price = 1 - price,
        no_whs = 1 - whs,
        no_island = 1 - island,
        no_climate = 1 - climate,
        no_sea = 1 - sea,
        no_politics = 1 - politics,
        no_culture = 1 - culture, 
        no_religion = 1 - religion,
        no_trade = 1 - trade,
        no_migration = 1 - migration,
        no_disease = 1 - disease
    )

save(data, file = "./data/data.Rda")