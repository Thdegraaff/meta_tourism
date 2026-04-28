set.seed(100)

library("hrbrthemes")
library("tidyverse")
library("here")
library("gridExtra")
library("brms")
library("tidybayes")

###################################
# Code below is a bit clunky as loops over brms functions are 
# cumbersome
##################################


stderror <- function(x) sd(x)/sqrt(length(x))

data_path <- here::here("data","data.Rda")
load(file = data_path)

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


m_1 <- brm(data = data, family = student,
                  dist| se(dist_se,  sigma = TRUE) ~ 1 +  
                          ls + ml + neg_bin + other + zero_inflated + 
                          no_region_time_effects + fixed_effects_no_time + random_effects + no_panel + duration +
                          origin_constrained + destination_constrained + no_dom_international + impute_se + 
                          dist_se + 
                          (1 | study_id),
                      iter = 5000, warmup = 2000, cores = 3, chains = 3,
                      prior = c(prior(normal(-1, 0.25), class = Intercept),
                                prior(normal(0, 0.25), class = b),
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
               dist_se + 
               (1 | study_id),
           iter = 5000, warmup = 2000, cores = 3, chains = 3,
           prior = c(prior(normal(-1, 0.5), class = Intercept),
                     prior(normal(0, 0.5), class = b),
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

m_4 <- brm(data = data, family = student,
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

m_5 <- brm(data = data, family = student,
           dist| se(dist_se,  sigma = TRUE) ~ 1 +  
               ls + ml + neg_bin + other + zero_inflated + 
               no_region_time_effects + fixed_effects_no_time + random_effects + no_panel + duration +
               origin_constrained + destination_constrained + no_dom_international + impute_se + 
               dist_se + 
               (1 | study_id),
           iter = 5000, warmup = 2000, cores = 3, chains = 3,
           prior = c(prior(normal(-1, 3), class = Intercept),
                     prior(normal(0, 3), class = b),
                     prior(exponential(1), class = sd),
                     prior(exponential(1), class = sigma)),
           seed = 14,
           control = list(adapt_delta = 0.8,
                          max_treedepth = 14)
)

m_6 <- brm(data = data, family = student,
           dist| se(dist_se,  sigma = TRUE) ~ 1 +  
               ls + ml + neg_bin + other + zero_inflated + 
               no_region_time_effects + fixed_effects_no_time + random_effects + no_panel + duration +
               origin_constrained + destination_constrained + no_dom_international + impute_se + 
               dist_se + 
               (1 | study_id),
           iter = 5000, warmup = 2000, cores = 3, chains = 3,
           prior = c(prior(normal(-1, 4), class = Intercept),
                     prior(normal(0, 4), class = b),
                     prior(exponential(1), class = sd),
                     prior(exponential(1), class = sigma)),
           seed = 14,
           control = list(adapt_delta = 0.8,
                          max_treedepth = 14)
)

m_7 <- brm(data = data, family = student,
           dist| se(dist_se,  sigma = TRUE) ~ 1 +  
               ls + ml + neg_bin + other + zero_inflated + 
               no_region_time_effects + fixed_effects_no_time + random_effects + no_panel + duration +
               origin_constrained + destination_constrained + no_dom_international + impute_se + 
               dist_se + 
               (1 | study_id),
           iter = 5000, warmup = 2000, cores = 3, chains = 3,
           prior = c(prior(normal(-1, 10), class = Intercept),
                     prior(normal(0, 10), class = b),
                     prior(exponential(1), class = sd),
                     prior(exponential(1), class = sigma)),
           seed = 14,
           control = list(adapt_delta = 0.8,
                          max_treedepth = 14)
)
    
fit_1 <- fitted(m_1, newdata = newdata_1)
fit_2 <- fitted(m_2, newdata = newdata_1)
fit_3 <- fitted(m_3, newdata = newdata_1)
fit_4 <- fitted(m_4, newdata = newdata_1)
fit_5 <- fitted(m_5, newdata = newdata_1)
fit_6 <- fitted(m_6, newdata = newdata_1)
fit_7 <- fitted(m_7, newdata = newdata_1)

summary(fit_1)
summary(fit_2)
summary(fit_3)
summary(fit_4)
summary(fit_5)
summary(fit_6)
summary(fit_7)

pr <- c(0.25, 0.5, 1, 2, 3, 4, 10)
mean_fit <- c(mean(fit_1[,1]), mean(fit_2[,1]), mean(fit_3[,1]), mean(fit_4[,1]), mean(fit_5[,1]),
              mean(fit_6[,1]), mean(fit_7[,1]))
sd_fit <- c(mean(fit_1[,2]), mean(fit_2[,2]), mean(fit_3[,2]), mean(fit_4[,2]), mean(fit_5[,2]),
              mean(fit_6[,2]), mean(fit_7[,2]))
df <- tibble(pr = pr, fit = mean_fit, variation = sd_fit)

p <- ggplot(data = df, aes(x = pr, y = fit)) + 
    geom_line() + 
    geom_point() +
    ylab("Mean posterior distance decay effect for model (I)") + xlab("Prior on standard devation")  +
    theme_ipsum() +
    theme(legend.position = "none") +
    theme(plot.title = element_text(size=12), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))

fig_path <- here::here("figures", "prior_sim.pdf")

cairo_pdf(file = fig_path, width = 6, height = 4)
p
dev.off()
