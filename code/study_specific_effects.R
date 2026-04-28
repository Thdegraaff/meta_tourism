## Libraries

library("here")
library("tidyverse")
library("viridis")
library("tidybayes")
library("brms")

## Load data

data_path <- here::here("data", "data.Rda")
load(data_path)
output_path <- here::here("output", "intercept_only.RData")
load(file = output_path)
output_path <- here::here("output", "pubbias.RData")
load(file = output_path)
output_path <- here::here("output", "full_models.RData")
load(file = output_path)

data_plot <- data %>%
    select(dist, dist_se, study_id) %>%
    group_by(study_id) %>%
    mutate(
        meta_mean = mean(dist),
        se_mean = mean(dist_se),
    )

color <- viridis_pal(option = "B")(7)[3]

n_study <- distinct(data, study_id) %>% count() %>% pull(n)
ranks <-
    tibble(Estimate = ranef(m_4)$study_id[, 1, "Intercept"],
           index    = 1:n_study) %>% 
    arrange(Estimate) %>% 
    mutate(rank = 1:n_study)

Intercept_vector <- c(fixef(m_0_varying_effects)[1 , 1], 
                      fixef(m_4_a)[1 , 1], 
                      fixef(m_4_b)[1 , 1], 
                      fixef(m_4)[1 , 1])
Intercept_vector <- rep(Intercept_vector, each = n_study)

d <-rbind(ranef(m_0_varying_effects)$study_id[ , , "Intercept"] + fixef(m_0_varying_effects)[1 , 1],
          ranef(m_4_a)$study_id[ , , "Intercept"] + fixef(m_4_a)[1 , 1],
          ranef(m_4_b)$study_id[ , , "Intercept"] + fixef(m_4_b)[1 , 1],
          ranef(m_4)$study_id[ , , "Intercept"] + fixef(m_4)[1 , 1]) %>%
    data.frame() %>%
    mutate(index = rep(1:n_study, times = 4),
           type  = rep(c("[I] Basic model with study specific effects" , 
                         "[II] plus additional controls for study attributes", 
                         "[III] plus errors-in-outcomes (measurement error)", 
                         "[IV] plus t-distribution response variable added"), each = n_study)) %>% 
    # add the ranks
    left_join(select(ranks, -Estimate), 
              by = "index") %>%
    cbind(Intercept_vector)

ggplot(data = d, aes(x = Estimate, xmin = Q2.5, xmax = Q97.5, y = rank)) +
    geom_pointrange(fatten = 1, color = color) +
    scale_x_continuous("", limits = c(-3, 1), breaks = c(-3, -2, -1, 0, 1) ) +
    scale_y_continuous(NULL, breaks = NULL) +
    geom_vline(data = d, aes(xintercept = Intercept_vector, color = "red"), size = 1) + 
    geom_vline(xintercept = -3, linetype="dotted", color = "black", size=0.2) +
    geom_vline(xintercept = -2, linetype="dotted", color = "black", size=0.2) +
    geom_vline(xintercept = -1, linetype="dotted", color = "black", size=0.2) +   
    geom_vline(xintercept =  0, linetype="dotted", color = "black", size=0.2) +
    geom_vline(xintercept =  1, linetype="dotted", color = "black", size=0.2) +      
    theme(panel.grid = element_blank()) +
    guides(color = "none") + 
    coord_flip() +
    theme(legend.position="none") +
    theme_bw() +
    facet_wrap(vars(type) , nrow = 4)

figure_path <- here::here("figures", "forestplot_studies.pdf")
ggsave(figure_path, width = 8, height = 10)
