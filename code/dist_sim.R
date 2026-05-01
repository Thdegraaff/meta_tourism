set.seed(14)

library("tidyverse")
library("readxl")
library("here")
library("hrbrthemes")
library("ggforce")
library("gridExtra")

n_sim = 500

x <- runif(n_sim, 0, 2) - 1
y <- runif(n_sim, 0, 2) - 1

d <- sqrt(x^2 + y^2)

df <- tibble(x, y, d) %>%
    filter(d <= 1)

p_sim_plot <- ggplot(df,aes(x,y)) +
    geom_point(color = "cornflowerblue") +
    geom_point(aes(x=0, y=0), colour="red", size = 5) +
    theme_ipsum() + 
    labs(title = "Distribution of possible destinations",
         subtitle = "In thousands of kilometers",
         caption = "Source: simulated data") +
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
    geom_text(aes(x = 0.1, y = 0, label = "2"), vjust = 1, size = 2.5) +
    geom_text(aes(x = 0.2, y = 0, label = "4"), vjust = 1, size = 2.5) +
    geom_text(aes(x = 0.3, y = 0, label = "6"), vjust = 1, size = 2.5) +
    geom_text(aes(x = 0.4, y = 0, label = "8"), vjust = 1, size = 2.5) +
    geom_text(aes(x = 0.5, y = 0, label = "10"), vjust = 1, size = 2.5) +
    geom_text(aes(x = 0.6, y = 0, label = "12"), vjust = 1, size = 2.5) +
    geom_text(aes(x = 0.7, y = 0, label = "14"), vjust = 1, size = 2.5) +
    geom_text(aes(x = 0.8, y = 0, label = "16"), vjust = 1, size = 2.5) +
    geom_text(aes(x = 0.9, y = 0, label = "18"), vjust = 1, size = 2.5) +
    geom_text(aes(x = 1, y = 0, label = "20"), vjust = 1, size = 2.5) +
    #facet_wrap(~ type_f) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank()
          )

set.seed(14)

make_tourists_plot <- function(data){
    model_colors <- c("steelblue", "tomato", "sienna")
    ggplot(data = data, aes(x = cat, y = tourists, fill = beta) ) + 
        theme_ipsum() + 
        geom_col(position = "dodge") + 
        scale_color_manual(name = "Distance-decay", labels = c("-1.09", "-0.82", "-0.54"),values = model_colors) +
        scale_fill_manual(name = "Distance-decay", labels = c("-1.09", "-0.82", "-0.54"), values = model_colors) +
        theme(legend.position = "top") + 
        labs(x="Distance categories (in thousands of kilometers)", 
             y="Share of total number of tourists",
             #title = "Tourists per distance category",
             #subtitle = "In shares of total amount of tourists", 
             caption = "Source: simulated data") +
        scale_x_continuous(breaks = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20))
        # scale_x_discrete(limits=c("1","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"),
                         # labels = everysecond(data$cat)) 
}

# data_path <- here::here("data", "Gravity_V202211.csv")
# gravity <- read_csv(data_path) %>%
#     select(iso3_o, year, pop_o) %>%
#     filter(year == 2019) %>%
#     group_by(iso3_o) %>%
#     summarise(
#         population = mean(pop_o)
#     )
# 
# save_path <- here::here("data", "data_gravity.Rda")
# save(gravity, file = save_path)

data_path <- here::here("data","data_gravity.Rda")
load(file = data_path)

data_path <- here::here("data", "dist_cepii.xls")
dist_data <- read_excel(data_path) %>%
    select(iso_o, iso_d, dist) %>%
    filter(dist >= 1, iso_o != iso_d) %>%
    mutate(
        cat = as.numeric(cut_interval(dist, 20)) 
    ) 

p_hist <- dist_data |>
    mutate(dist_1000 = dist / 1000) |>
    ggplot(aes(x = dist_1000)) + 
    geom_histogram(fill="cornflowerblue", col = "white", linewidth = 0.5, bins = 20) +
    theme_ipsum() +
    labs(x="Distance between pairs of countries (in thousands of kilometers)", 
         y="Number of country pairs",
         #title = "Real world distribution of distances",
         subtitle = "", 
         caption = "Source: CEPII") 

dist_data <- left_join(dist_data, gravity, by = c("iso_o" = "iso3_o"))
dist_data <- left_join(dist_data, gravity, by = c("iso_d" = "iso3_o"))
dist_data <- na.omit(dist_data)

n_tourists_theo <- function(b){
    r = 20000 * sqrt(runif(1000000))
    # see http://www.anderswallin.net/2009/05/uniform-random-points-in-a-circle-using-polar-coordinates/
    df <- tibble(dist = r) %>%
        filter(dist > 1000) %>%
        mutate(
            cat = as.numeric(cut_interval(dist, 20)), 
            tourists = exp(b * log(dist))
        ) %>% group_by(cat) %>%
        summarise(
            total_tourists = sum(tourists)
        ) %>%
        mutate(
            share_tourists = (total_tourists / sum(total_tourists) )
        )
}
    
n_tourists_real <- function(b, df_data){
    
    df <- df_data %>%
        # filter(iso_o == "CHN") %>%
        mutate(
            tourists = exp(log(population.x) + log(population.y) + b * log(dist))
        ) %>% group_by(cat) %>%
        summarise(
            total_tourists = sum(tourists)
        ) %>%
        mutate(
            share_tourists = (total_tourists / sum(total_tourists) )
        )
}

beta <- -1.09
df_temp <- n_tourists_real(beta, dist_data)
df_total <- tibble(cat = df_temp$cat, beta_low = df_temp$share_tourists) 

beta <- -0.82
df_temp <- n_tourists_real(beta, dist_data)
df_total <- tibble(df_total, beta_sec = df_temp$share_tourists) 

beta <- -0.54
df_temp <- n_tourists_real(beta, dist_data)
df_total <- tibble(df_total, beta_thi = df_temp$share_tourists) 

df_r <- pivot_longer(df_total, cols = starts_with("beta"), names_to = "beta", values_to = "tourists")

beta <- -1.09
df_temp <- n_tourists_theo(beta)
df_total <- tibble(cat = df_temp$cat, beta_low = df_temp$share_tourists) 

beta <- -0.82
df_temp <- n_tourists_theo(beta)
df_total <- tibble(df_total, beta_sec = df_temp$share_tourists) 

beta <- -0.54
df_temp <- n_tourists_theo(beta)
df_total <- tibble(df_total, beta_thi = df_temp$share_tourists) 

df_t <- pivot_longer(df_total, cols = starts_with("beta"), names_to = "beta", values_to = "tourists")

dist_data_china <- dist_data %>%
 filter(iso_o == "CHN")

beta <- -1.09
df_temp <- n_tourists_real(beta, dist_data_china)
df_total <- tibble(cat = df_temp$cat, beta_low = df_temp$share_tourists) 

beta <- -0.82
df_temp <- n_tourists_real(beta, dist_data_china)
df_total <- tibble(df_total, beta_sec = df_temp$share_tourists) 

beta <- -0.54
df_temp <- n_tourists_real(beta, dist_data_china)
df_total <- tibble(df_total, beta_thi = df_temp$share_tourists) 

df_china <- pivot_longer(df_total, cols = starts_with("beta"), names_to = "beta", values_to = "tourists")

dist_data_nzl <- dist_data %>%
    filter(iso_o == "NZL")

beta <- -1.09
df_temp <- n_tourists_real(beta, dist_data_nzl)
df_total <- tibble(cat = df_temp$cat, beta_low = df_temp$share_tourists) 

beta <- -0.82
df_temp <- n_tourists_real(beta, dist_data_nzl)
df_total <- tibble(df_total, beta_sec = df_temp$share_tourists) 

beta <- -0.54
df_temp <- n_tourists_real(beta, dist_data_nzl)
df_total <- tibble(df_total, beta_thi = df_temp$share_tourists) 

df_nzl <- pivot_longer(df_total, cols = starts_with("beta"), names_to = "beta", values_to = "tourists")

dist_data_nld <- dist_data %>%
    filter(iso_o == "NLD")

beta <- -1.09
df_temp <- n_tourists_real(beta, dist_data_nld)
df_total <- tibble(cat = df_temp$cat, beta_low = df_temp$share_tourists) 

beta <- -0.82
df_temp <- n_tourists_real(beta, dist_data_nld)
df_total <- tibble(df_total, beta_sec = df_temp$share_tourists) 

beta <- -0.54
df_temp <- n_tourists_real(beta, dist_data_nld)
df_total <- tibble(df_total, beta_thi = df_temp$share_tourists) 

df_nld <- pivot_longer(df_total, cols = starts_with("beta"), names_to = "beta", values_to = "tourists")

p_tourists_real <- make_tourists_plot(df_r)
p_tourists_theo <- make_tourists_plot(df_t)
p_tourists_china <- make_tourists_plot(df_china)
p_tourists_nzl <- make_tourists_plot(df_nzl)
p_tourists_nld <- make_tourists_plot(df_nld)

fig_path <- here::here("figures", "dist_sim_theo.pdf")
cairo_pdf(fig_path, width = 8, heigh = 5)
grid.arrange(p_sim_plot, p_tourists_theo, ncol=2)
dev.off()

fig_path <- here::here("figures", "dist_sim_real.pdf")    
cairo_pdf(fig_path, width = 8, heigh = 4)
grid.arrange(p_hist, p_tourists_real, ncol=2)
dev.off()

fig_path <- here::here("figures", "dist_sim_china_nzl_nld.pdf")
cairo_pdf(fig_path, width = 14, heigh = 5)
grid.arrange(p_tourists_china, p_tourists_nzl, p_tourists_nld, ncol=3)
dev.off()

######################## For graphical abstract

beta <- -0.82
df_temp <- n_tourists_theo(beta)

everysecond <- function(x){
    x <- sort(unique(x))
    x[seq(2, length(x), 2)] <- ""
    x
}

make_tourists_plot <- function(data){
    ggplot(data = data, aes(x = cat, y = share_tourists, fill = beta) ) + 
        #theme_ipsum() + 
        geom_col(fill = "darkseagreen4") + 
        labs(x="Distance (in thousands of kilometers)", 
             y="Share of total tourists") +
        scale_x_discrete(limits=c("1","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"),
                         labels = everysecond(data$cat)) +
        theme(legend.position = "none") +
        theme(plot.title = element_text(size=12), 
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black")) +
        theme(rect = element_rect(fill = "transparent"))
}

plot_abstract <- make_tourists_plot(df_temp)

fig_path <- here::here("figures", "theo_tourists_abstract.pdf")
ggsave(
    plot = plot_abstract,
    file = fig_path,
    width = 4, 
    height = 3.5,
    bg = "transparent"
)
