library("hrbrthemes")
library("tidyverse")
library("here")
library("gridExtra")

data_path <- here::here("data", "data.Rda")
load(file = data_path)

data_plot <- data %>%
    #filter(impute_se == 0) %>%
    select(dist, dist_se, average_year, pub_year) %>%
    mutate(
        meta_mean = mean(dist),
        se_mean = sd(dist),
        lower = dist - 1.96 * dist_se, 
        upper = dist + 1.96 * dist_se,
        t_stat = dist/dist_se, 
        Color = ifelse(abs(dist)/dist_se > 1.96, "tomato", "cornflowerblue")
    ) %>%
    arrange(dist) %>%
    mutate(
        index = row_number()
    )

################################### Data description

m_mean <- mean(data_plot$dist)
m_sd <- sd(data_plot$dist)
m_ll <- mean(data_plot$dist) -  m_sd
m_ul <- mean(data_plot$dist) +  m_sd

hist_effect <- ggplot(data_plot, aes(dist)) + 
    geom_histogram(aes(y = ..density..), binwidth = 0.1, color = "white", fill = "cornflowerblue") +
    theme_ipsum() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    geom_vline(xintercept = m_mean, linetype = 'dashed', color = "black") +
    geom_vline(xintercept = m_ll, linetype = 'dotted', color = "black") +
    geom_vline(xintercept = m_ul, linetype = 'dotted', color = "black") +
    stat_function(fun = dnorm, args = list(mean = m_mean, sd = m_sd)) +
    xlab("Distance elasticity of tourims flows") 

dist_effect<- ggplot(data_plot, aes(x = index, y = dist))  + 
    geom_pointrange(aes(y = dist, ymin = lower, ymax = upper), size = 0.1, alpha = 0.5, color = "cornflowerblue") +
    theme_ipsum() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    xlab("") + ylab("Distance elasticity of tourism flows") +
    geom_hline(yintercept = m_mean, linetype = 'dashed', color = "black") +
    geom_hline(yintercept = m_ll, linetype = 'dotted', color = "black") +
    geom_hline(yintercept = m_ul, linetype = 'dotted', color = "black") 

fig_path <- here::here("figures", "effect_size.pdf")

cairo_pdf(file = fig_path, width = 5, height = 2)
grid.arrange(hist_effect, dist_effect, ncol = 2)
dev.off()

################################### Create time scatterplot

time_scatter <- ggplot(data_plot, aes(x = average_year, y = dist)) + 
    geom_jitter(color = "tomato", alpha = 0.7) + 
    geom_smooth(method='lm') + 
    xlab("Average year of data analysed") + ylab("Distance elasticity \n of tourism flows") +
    theme_ipsum() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))

pub_scatter <- ggplot(data_plot, aes(x = pub_year, y = dist)) + 
    geom_jitter(color = "tomato", alpha = 0.7)  + 
    geom_smooth(method='lm') + 
    xlab("Year of publication") + ylab("Distance elasticity \n of tourism flows") +
    theme_ipsum() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))

fig_path <- here::here("figures", "time_scatter.pdf")

cairo_pdf(file = fig_path, width = 10, height = 4)
grid.arrange(time_scatter, pub_scatter, ncol = 2)
dev.off()

################################### Create publication bias plots

data_plot_sel <- filter(data_plot, t_stat > -20)

t_hist <- ggplot(data_plot_sel, aes(t_stat)) + 
    geom_histogram(binwidth = 0.5, color = "white", fill = "cornflowerblue") +
    theme_ipsum() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    geom_vline(xintercept = -1.96, linetype = 'dotted', color = "black") +
    geom_text(aes(x=0, label="1.96", y=75), size = 3) +
    xlab("t-statistic") + ylab("Density")

se.seq = seq(0, max(data$dist_se), by = 0.001)
ll95 = mean(data$dist) - mean(data$dist) - (1.96 * se.seq)
ul95 = mean(data$dist) - mean(data$dist) + (1.96 * se.seq)

meanll95 = mean(data$dist) - (1.96 * sd(data$dist))
meanul95 = mean(data$dist) + (1.96 * sd(data$dist))

dfCI = data.frame(ll95, ul95, se.seq, estimate = mean(data$dist), meanll95, meanul95)

funnel_plot <- ggplot(data_plot, aes(x = dist_se, y = dist, color = Color)) + 
    geom_point(size = 0.5) + 
    scale_color_identity() + 
    xlab('Standard Error') + ylab('Distance elasticity') + 
    geom_line(aes(x = se.seq, y = ll95), linetype = 'dotted', data = dfCI, color = "black") +
    geom_line(aes(x = se.seq, y = ul95), linetype = 'dotted', data = dfCI, color = "black") +
    #geom_segment(aes(x = min(se.seq), y = meanll95, xend = max(se.seq), yend = meanll95), linetype='dotted', data=dfCI, color = "black") +
    #geom_segment(aes(x = min(se.seq), y = meanul95, xend = max(se.seq), yend = meanul95), linetype='dotted', data=dfCI, color = "black") +
    scale_y_continuous(breaks=seq(-3,2,1)) + 
    scale_x_continuous(breaks=seq(0,1.4,0.2)) + 
    coord_flip() + 
    theme_ipsum() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))

fig_path <- here::here("figures", "pubbias.pdf")

cairo_pdf(file = fig_path, width = 7.5, height = 3)
grid.arrange(t_hist, funnel_plot, ncol = 2)
dev.off()
