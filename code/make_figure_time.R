library("hrbrthemes")
library("tidyverse")
library("here")
library("gridExtra")
library("brms")


model_path_models <- here::here("./output/", "full_models.RData")
model_path_data <- here::here("./data/", "data.Rda")

load(file = model_path_models)
load(file = model_path_data)

######################### for data year

spline <- conditional_smooths(m_4)
effect_size <- fixef(m_4)[1,1]

year_mid.df <- data.frame(spline$`mu: s(average_year)`)
year_mid.df$estimate__ <- year_mid.df$estimate__ + effect_size
year_mid.df$lower__ <- year_mid.df$lower__ + effect_size
year_mid.df$upper__ <- year_mid.df$upper__ + effect_size

### Time trend for international tourists
plot_data <- ggplot(year_mid.df, aes(average_year, estimate__)) + geom_line(color = "cornflowerblue", size = 1.25) +
    geom_ribbon(aes(ymin=lower__, ymax=upper__), linetype=2, alpha=0.20, fill = "cornflowerblue") + 
    ylab("Distance decay effect") + xlab("Year of data (average)")  +
    geom_hline(yintercept = effect_size, linetype = 'dotted', color = "sienna", size = 0.75) +
    geom_point(data = data, aes(x = average_year, y = dist, alpha = 0.7, colour = "tomato"), size = 1) +
    theme_ipsum() +
    theme(legend.position = "none") +
    theme(plot.title = element_text(size=12), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))

######################### for publication year

spline <- conditional_smooths(m_4)
effect_size <- fixef(m_4)[1,1]

year_mid.df <- data.frame(spline$`mu: s(pub_year)`)
year_mid.df$estimate__ <- year_mid.df$estimate__ + effect_size
year_mid.df$lower__ <- year_mid.df$lower__ + effect_size
year_mid.df$upper__ <- year_mid.df$upper__ + effect_size

### Time trend for domestic tourists
plot_pub <- ggplot(year_mid.df, aes(pub_year, estimate__)) + geom_line(color = "cornflowerblue", size = 1.25) +
    geom_ribbon(aes(ymin=lower__, ymax=upper__), linetype=2, alpha=0.20, fill = "cornflowerblue") + 
    ylab("Distance decay effect") + xlab("Year of publication")  +
    geom_hline(yintercept = effect_size, linetype = 'dotted', color = "sienna", size = 0.75) +
    geom_point(data = data, aes(x = pub_year, y = dist, alpha = 0.7, colour = "tomato"), size = 1) +
    theme_ipsum() +
    theme(legend.position = "none") +
    theme(plot.title = element_text(size=12), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))

cairo_pdf(file="./figures/time_trends.pdf", width = 7.5, height = 3)
grid.arrange(plot_data, plot_pub, ncol = 2)
dev.off()

plot_pub_abstract <- ggplot(year_mid.df, aes(pub_year, estimate__)) + geom_line(color = "midnightblue", size = 1.25) +
    geom_ribbon(aes(ymin=lower__, ymax=upper__), linetype=2, alpha=0.20, fill = "midnightblue") + 
    ylab("Distance-decay effect") + xlab("Year of publication")  +
    geom_hline(yintercept = effect_size, linetype = 'dotted', color = "sienna", size = 0.75) +
    geom_point(data = data, aes(x = pub_year, y = dist, alpha = 0.3, colour = "tomato"), size = 1.3) +
    #theme_ipsum() +
    theme(legend.position = "none") +
    theme(plot.title = element_text(size=12), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    theme(rect = element_rect(fill = "transparent"))

ggsave(
    plot = plot_pub_abstract,
    file = "./figures/time_trends_publication.pdf",
    width = 4, 
    height = 3.5,
    bg = "transparent"
)

