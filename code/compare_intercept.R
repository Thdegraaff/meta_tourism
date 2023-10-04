set.seed(14)

load("./data/data.Rda")

data$dist2 <- data$dist

m1 <- bf(dist| se(dist_se,  sigma = TRUE) ~ 1 +  
             ls + ml + neg_bin + other + zero_inflated + 
             no_individual_effects + random_effects + no_panel + duration +
             origin_constrained + destination_constrained + impute_se + 
             continental_o + country_o + continental_d + country_d +
             no_europe_o + no_australia_o + no_africa_o + no_north_america_o + no_south_america_o + no_asia_o +
             no_europe_d + no_australia_d + no_africa_d + no_north_america_d + no_south_america_d + no_asia_d +
             dist_se + s(dist_se, pc = 0) + 
             (1 | study_id)) + student()

m2 <- bf(dist2| se(dist_se,  sigma = TRUE) ~ 1 +  
            ls + ml + neg_bin + other + zero_inflated + 
            no_individual_effects + random_effects + no_panel + duration +
            origin_constrained + destination_constrained + impute_se + 
            continental_o + country_o + continental_d + country_d +
            no_europe_o + no_australia_o + no_africa_o + no_north_america_o + no_south_america_o + no_asia_o +
            no_europe_d + no_australia_d + no_africa_d + no_north_america_d + no_south_america_d + no_asia_d +
            no_gdp_o + no_gdppc_o + no_pop_o + no_gdp_d + no_gdppc_d + no_pop_d + 
            no_colony + no_language + no_border + no_exchange + no_comcur + no_rta + no_price + no_whs + no_island + 
            no_climate + no_sea + no_politics + no_culture + no_religion + no_trade + no_migration + no_disease + 
            dist_se + s(dist_se, pc = 0) + 
            (1 | study_id)) + student()

m_together <- brm(m1 + m2 + set_rescor(FALSE), 
                data = data, 
                iter = 5000, warmup = 2000, cores = 3, chains = 3,
                seed = 14,
                prior = c(prior(normal(-1, 0.25), class = Intercept),
                          prior(normal(0, 0.25), class = b)),
                control = list(adapt_delta = 0.8,
                               max_treedepth = 14)
)


post <- posterior_samples(m_together) %>%
    select(b_dist_Intercept, b_dist2_Intercept) 

count1 <- sum(post$b_dist_Intercept >= post$b_dist2_Intercept)/length(post$b_dist2_Intercept)
count2 <- sum(post$b_dist2_Intercept >= (rep(-1, length(post$b_dist2_Intercept) )) )/length(post$b_dist2_Intercept)

post <- post %>%
    rename(`Total effect` = b_dist_Intercept,
           `Direct effect` = b_dist2_Intercept) %>% 
    mutate(
        difference = `Total effect` - `Direct effect`
    ) %>%
    pivot_longer(cols = 1:3, names_to = "Distribution", values_to = "value")

p_diff <- ggplot(data = post, aes(x=value, group = Distribution, fill = Distribution)) +
    geom_density(alpha = 0.5) + 
    ylab("") + xlab("") + 
    geom_vline(xintercept =0, linetype = 'dotted', color = "sienna", size = 0.75) +
    theme_ipsum() +
    theme(legend.position = "top") +
    guides(y = "none") + 
    theme(plot.title = element_text(size =12), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
    
cairo_pdf("./figures/fig_difference.pdf", width = 10, heigh = 5)
p_diff
dev.off()
