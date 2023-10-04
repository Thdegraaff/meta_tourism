set.seed(14)
n = 500

x <- runif(n, 0, 2) - 1
y <- runif(n, 0, 2) - 1

d <- sqrt(x^2 + y^2)

df <- tibble(x, y, d) %>%
    filter(d <= 1)

p_plot <- ggplot(df,aes(x,y)) +
    geom_point(color = "cornflowerblue") +
    geom_point(aes(x=0, y=0), colour="red", size = 5) +
    theme_ipsum() + 
    geom_circle(aes(x0 = 0, y0 = 0, r = 1),inherit.aes=FALSE, size = 2) +
    geom_circle(aes(x0 = 0, y0 = 0, r = 0.1), lwd = 0.1, alpha = 0.1, lty = "dotted", inherit.aes=TRUE) +
    geom_circle(aes(x0 = 0, y0 = 0, r = 0.2), lwd = 0.3, alpha = 0.5, lty = "dotted", inherit.aes=FALSE) +
    geom_circle(aes(x0 = 0, y0 = 0, r = 0.3), lwd = 0.3, alpha = 0.5, lty = "dotted", inherit.aes=FALSE) +
    geom_circle(aes(x0 = 0, y0 = 0, r = 0.4), lwd = 0.3, alpha = 0.5, lty = "dotted", inherit.aes=FALSE) +
    geom_circle(aes(x0 = 0, y0 = 0, r = 0.5), lwd = 0.3, alpha = 0.5, lty = "dotted", inherit.aes=FALSE) +
    geom_circle(aes(x0 = 0, y0 = 0, r = 0.6), lwd = 0.3, alpha = 0.5, lty = "dotted", inherit.aes=FALSE) +
    geom_circle(aes(x0 = 0, y0 = 0, r = 0.7), lwd = 0.3, alpha = 0.5, lty = "dotted", inherit.aes=FALSE) +
    geom_circle(aes(x0 = 0, y0 = 0, r = 0.8), lwd = 0.3, alpha = 0.5, lty = "dotted", inherit.aes=FALSE) +
    geom_circle(aes(x0 = 0, y0 = 0, r = 0.9), lwd = 0.3, alpha = 0.5, lty = "dotted", inherit.aes=FALSE) +
    geom_text(aes(x = 0.1, y = 0, label = "1000"), vjust = 1, size = 1.75) +
    geom_text(aes(x = 0.2, y = 0, label = "2000"), vjust = 1, size = 1.75) +
    geom_text(aes(x = 0.3, y = 0, label = "3000"), vjust = 1, size = 1.75) +
    geom_text(aes(x = 0.4, y = 0, label = "4000"), vjust = 1, size = 1.75) +
    geom_text(aes(x = 0.5, y = 0, label = "5000"), vjust = 1, size = 1.75) +
    geom_text(aes(x = 0.6, y = 0, label = "6000"), vjust = 1, size = 1.75) +
    geom_text(aes(x = 0.7, y = 0, label = "7000"), vjust = 1, size = 1.75) +
    geom_text(aes(x = 0.8, y = 0, label = "8000"), vjust = 1, size = 1.75) +
    geom_text(aes(x = 0.9, y = 0, label = "9000"), vjust = 1, size = 1.75) +
    geom_text(aes(x = 1, y = 0, label = "10000"), vjust = 1, size = 1.75) +
    #facet_wrap(~ type_f) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank()
          )

set.seed(14)

n_tourists <- function(b, nobs, direct){
    x <- runif(nobs, 0, 2) - 1
    y <- runif(nobs, 0, 2) - 1
    d <- sqrt(x^2 + y^2)
    
    df <- tibble(x, y, d) %>%
        filter(d <= 1, d >= 0.5) %>%
        mutate(
            cat = as.numeric(cut_interval(d, 10)),
            ln_tourists = exp(2 + 0.4*log(1000000) + 0.4* log(1000000) + b * log(d*1000) + direct*log(5))
        ) %>% group_by(cat) %>%
        summarise(
            total_tourists = sum(ln_tourists)
        )
}

repl <- 1000
df_tourists <- data.frame(matrix(ncol = 10, nrow = repl))

beta <- -1.10
for (i in 1:repl){
    df_temp <- n_tourists(beta, n, 1)
    df_tourists[i,] <- t(df_temp$total_tourists)
}
df_total <- tibble(cat = df_temp$cat, mean1 = colMeans(df_tourists) )

beta <- -0.92
for (i in 1:repl){
    df_temp <- n_tourists(beta, n, 1)
    df_tourists[i,] <- t(df_temp$total_tourists)
}
df_total <- tibble(df_total, mean2 = colMeans(df_tourists) )

beta <- -0.68
for (i in 1:repl){
    df_temp <- n_tourists(beta, n, 0)
    df_tourists[i,] <- t(df_temp$total_tourists)
}
df_total <- tibble(df_total, mean4 = colMeans(df_tourists) )

df_t <- pivot_longer(df_total, cols = starts_with("mean"), names_to = "beta", values_to = "tourists")

model_colors <- c("steelblue", "tomato", "sienna")

p_sim <- ggplot(data = df_t, aes(x = cat, y = tourists, fill = beta) ) + 
    theme_ipsum() + 
    geom_col(position = "dodge") + 
    scale_color_manual(name = "Distance-decay", labels = c("-1.10", "-0.92", "-0.68"),values = model_colors) +
    scale_fill_manual(name = "Distance-decay", labels = c("-1.10", "-0.92", "-0.68"), values = model_colors) +
    theme(legend.position = "top") + 
    labs(x="Distance (category)", y="Number of tourists") + 
    scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9","10"))

cairo_pdf("./figures/dist_sim.pdf", width = 10, heigh = 5)
grid.arrange(p_plot, p_sim, ncol=2)
dev.off()
