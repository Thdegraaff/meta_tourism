load("./output/full_models.RData")

post <- as_draws_df(m_3)
cormat <- post %>% cor()
cormat <- round(cormat,2)
diag(cormat) <- 0
max_value <- apply(cormat, 2, max)
max_value

destination <- post  %>% select(b_no_europe_o:b_no_asia_o)
pairs(destination)

structural <- post %>% select(b_dist_se:nu)
pairs(structural)
