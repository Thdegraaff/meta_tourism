model_path <- here::here("./output/", "full_models.RData")
load(file = model_path)


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
    ylab("Distance-decay effect") + xlab("Year of data (average)")  +
    geom_hline(yintercept = effect_size, linetype = 'dotted', color = "sienna", size = 0.75) +
    geom_point(data = data, aes(x = average_year, y = dist, alpha = 0.7, colour = "tomato"), size = 2) +
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
    ylab("Distance-decay effect") + xlab("Year of publication")  +
    geom_hline(yintercept = effect_size, linetype = 'dotted', color = "sienna", size = 0.75) +
    geom_point(data = data, aes(x = pub_year, y = dist, alpha = 0.7, colour = "tomato"), size = 2) +
    theme_ipsum() +
    theme(legend.position = "none") +
    theme(plot.title = element_text(size=12), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))

cairo_pdf(file="./figures/time_trends.pdf", width = 10, height = 4)
grid.arrange(plot_data, plot_pub, ncol = 2)
dev.off()
