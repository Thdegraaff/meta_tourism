set.seed(100)

library("hrbrthemes")
library("tidyverse")
library("here")
library("gridExtra")
library("brms")
library("tidybayes")

data_path <- here::here("data","data.Rda")
load(file = data_path)

m_flat <- brm(data = data, family = student,
           dist| se(dist_se,  sigma = TRUE) ~ 1 +  
               ls + ml + neg_bin + other + zero_inflated + 
               no_region_time_effects + fixed_effects_no_time + random_effects + no_panel + duration +
               origin_constrained + destination_constrained + no_dom_international + impute_se + 
               dist_se + 
               (1 | study_id),
           iter = 5000, warmup = 2000, cores = 3, chains = 3,
           seed = 14,
           control = list(adapt_delta = 0.8,
                          max_treedepth = 14)
)

data <- data |>
    group_by(study_id) |>
    mutate(number_inv = 1/n(),
           number = n())

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

m_1_weights <- brm(data = data, family = student,
                             dist| weights(number_inv) ~ 1 +  
                                 #ls + 
                                 ml + neg_bin + other + zero_inflated + 
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

data_early <- data |>
    filter(pub_year <= 2015)

data_late <- data |>
    filter(pub_year > 2015)

newdata_early<- data_early %>%
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

newdata_late <- data_late %>%
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

m_1_early <- brm(data = data_early, family = student,
           dist| se(dist_se,  sigma = TRUE) ~ 1 +  
               #ls + 
               ml + neg_bin + other + zero_inflated + 
               random_effects + no_panel + duration +
               origin_constrained + destination_constrained + impute_se + 
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

m_1_late <- brm(data = data_late, family = student,
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

data_sample <- data |>
    group_by(study_id) |>
    slice_sample(n = 1)

newdata_sample <- data_sample %>%
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

m_1_sample<- brm(data = data_sample, family = student,
                dist| se(dist_se,  sigma = TRUE) ~ 1 +  
                    ls + ml + neg_bin + other + zero_inflated + 
                    no_region_time_effects + fixed_effects_no_time + random_effects + no_panel + duration +
                    origin_constrained + destination_constrained + no_dom_international + impute_se + 
                    dist_se ,
                iter = 5000, warmup = 2000, cores = 3, chains = 3,
                prior = c(prior(normal(-1, 2), class = Intercept),
                          prior(normal(0, 2), class = b),
                          prior(exponential(1), class = sigma)),
                seed = 14,
                control = list(adapt_delta = 0.8,
                               max_treedepth = 14)
)

fit_flat <- fitted(m_flat, newdata = newdata_1)
fit_weights <- fitted(m_1_weights, newdata = newdata_1)
fit_early <- fitted(m_1_early, newdata = newdata_early)
fit_late <- fitted(m_1_late, newdata = newdata_late)
fit_sample <- fitted(m_1_sample, newdata = newdata_sample)

summary(fit_flat)
summary(fit_weights)
summary(fit_early)
summary(fit_late)
summary(fit_sample)

############## Split data in two groups to check influence of the prior

prior_check <- data |>
    mutate(large = (number > 10) ) |>
    group_by(large) |>
    summarize(
        mean_dist = mean(dist)
    )
prior_check
