set.seed(100)

library("brms")

data_path <- here::here("data", "data.Rda")
load(file = data_path)

m_0 <- brm(data = data, 
           dist ~ 1, 
           iter = 5000, warmup = 2000, cores = 3, chains = 3,
           prior = c(prior(normal(-1, 2), class = Intercept),
                     prior(exponential(1), class = sigma)),
           seed = 14,
           control = list(adapt_delta = 0.8,
                          max_treedepth = 14)
)

m_0_varying_effects <- brm(data = data, 
           dist ~ 1 + (1 | study_id), 
           iter = 5000, warmup = 2000, cores = 3, chains = 3,
           prior = c(prior(normal(-1, 2), class = Intercept),
                     prior(exponential(1), class = sd),
                     prior(exponential(1), class = sigma)),
           seed = 14,
           control = list(adapt_delta = 0.8,
                          max_treedepth = 14)
)

m_0_me <- brm(data = data, 
                 dist| se(dist_se,  sigma = TRUE)  ~ 1 + (1 | study_id), 
                 iter = 5000, warmup = 2000, cores = 3, chains = 3,
              prior = c(prior(normal(-1, 2), class = Intercept),
                        prior(exponential(1), class = sd),
                        prior(exponential(1), class = sigma)),
              seed = 14,
              control = list(adapt_delta = 0.8,
                             max_treedepth = 14)
)

m_0_student <- brm(data = data, family = student,
              dist| se(dist_se,  sigma = TRUE)  ~ 1 + (1 | study_id), 
              iter = 5000, warmup = 2000, cores = 3, chains = 3,
              prior = c(prior(normal(-1, 2), class = Intercept),
                        prior(exponential(1), class = sd),
                        prior(exponential(1), class = sigma)),
              seed = 14,
              control = list(adapt_delta = 0.8,
                             max_treedepth = 14)
)

m_0_student2 <- brm(data = data, family = student,
                   dist| se(dist_se,  sigma = TRUE)  ~ 1 + (1 | gr(study_id, dist = "student")), 
                   iter = 5000, warmup = 2000, cores = 3, chains = 3,
                   prior = c(prior(normal(-1, 2), class = Intercept),
                             prior(exponential(1), class = sd),
                             prior(exponential(1), class = sigma)),
                   seed = 14,
                   control = list(adapt_delta = 0.8,
                                  max_treedepth = 14)
)

fit_0 <- predict(m_0, robust = TRUE)
fit_1 <- predict(m_0_varying_effects, robust = TRUE)
fit_2 <- predict(m_0_me, robust = TRUE)
fit_3 <- predict(m_0_student, robust = TRUE)

summary(fit_0)
summary(fit_1)
summary(fit_2)
summary(fit_3)

stderror <- function(x) sd(x)/sqrt(length(x))

stderror(fit_0)
stderror(fit_1)
stderror(fit_2)
stderror(fit_3)

summary(m_0)
summary(m_0_varying_effects)
summary(m_0_me)
summary(m_0_student)

loo(m_0_student)
summary(m_0_student2)

model_weights(m_0, m_0_varying_effects, m_0_me, m_0_student, weights = "loo")

output_path <- here::here("./output/", "intercept_only.RData")
save(m_0, m_0_varying_effects, m_0_me, m_0_student, m_0_student2, file = output_path)
