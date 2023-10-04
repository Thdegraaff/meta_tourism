load("./data/data.Rda")

m_0_student <- brm(data = data, family = student,
              dist| se(dist_se,  sigma = TRUE)  ~ 1 + (1 | study_id), 
              iter = 5000, warmup = 2000, cores = 3, chains = 3,
              prior = c(prior(normal(-1, 0.25), class = Intercept),
                        prior(exponential(1), class = sd),
                        prior(exponential(1), class = sigma)),
              seed = 14,
              control = list(adapt_delta = 0.8,
                             max_treedepth = 14)
)

m_0_student_a <- brm(data = data, family = student,
                   dist| se(dist_se,  sigma = TRUE)  ~ 1 + (1 | study_id) + dist_se, 
                   iter = 5000, warmup = 2000, cores = 3, chains = 3,
                   prior = c(prior(normal(-1, 0.25), class = Intercept),
                             prior(exponential(1), class = sd),
                             prior(exponential(1), class = sigma)),
                   seed = 14,
                   control = list(adapt_delta = 0.8,
                                  max_treedepth = 14)
)

m_0_student_b <- brm(data = data, family = student,
                     dist| se(dist_se,  sigma = TRUE)  ~ 1 + (1 | study_id) + dist_se + s(dist_se, pc = 0), 
                     iter = 5000, warmup = 2000, cores = 3, chains = 3,
                     prior = c(prior(normal(-1, 0.25), class = Intercept),
                               prior(exponential(1), class = sd),
                               prior(exponential(1), class = sigma)),
                     seed = 14,
                     control = list(adapt_delta = 0.8,
                                    max_treedepth = 14)
)

data$N_inv <- 1/data$sample_size

cf_function <- brm(data = data, family = lognormal,
                   dist_se ~ N_inv + (1 | study_id), 
                   iter = 5000, warmup = 2000, cores = 3, chains = 3,
                   prior = c(prior(normal(-1, 0.25), class = Intercept),
                             prior(exponential(1), class = sd),
                             prior(exponential(1), class = sigma)),
                   seed = 14,
                   control = list(adapt_delta = 0.8,
                                  max_treedepth = 14)
)

predict_values <- predict(cf_function)
data$cf_residual <- data$dist_se - predict_values[ , 1]
data$me_error <- predict_values[ , 2]

m_0_student_c <- brm(data = data, family = student,
                     dist| se(dist_se,  sigma = TRUE)  ~ 1 + (1 | study_id) + dist_se + cf_residual, 
                     iter = 5000, warmup = 2000, cores = 3, chains = 3,
                     prior = c(prior(normal(-1, 0.25), class = Intercept),
                               prior(exponential(1), class = sd),
                               prior(exponential(1), class = sigma)),
                     seed = 14,
                     control = list(adapt_delta = 0.8,
                                    max_treedepth = 14)
)


summary(m_0_student)
summary(m_0_student_a)
summary(m_0_student_b)
summary(m_0_student_c)

model_weights(m_0_student, m_0_student_a, m_0_student_b, m_0_student_c, weights = "loo")

model_path <- here::here("./output/", "pubbias.RData")
save(m_0_student, m_0_student_a, m_0_student_b, m_0_student_c, file = model_path)
