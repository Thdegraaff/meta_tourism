sysfonts::font_add_google("Roboto Condensed")
sysfonts::font_add("Roboto Condensed", regular = "RobotoCondensed-Regular.ttf")
showtext_auto()

set.seed(1974)

N <-  1000
beta <- 2
constant <- 10

gamma  <- -1
ln_distance <- runif(N, 4, 9)
island <- rbinom(N, 1, (ln_distance - 4)/5 )
border <- rbinom(N, 1, 0.5 * (1 - (ln_distance - 4)/5 ) )
ln_tourists_island <- constant + gamma * ln_distance + beta * island  + rnorm(N,0,1)
ln_tourists_border <- constant + gamma * ln_distance + beta * border  + rnorm(N,0,1)

data <- tibble(ln_tourists_border, ln_tourists_island, ln_distance, island, border)

m_noisland <- lm(ln_tourists_island~ ln_distance )
m_island <- lm(ln_tourists_island~ ln_distance + island)
m_noborder <- lm(ln_tourists_border~ ln_distance )
m_border <- lm(ln_tourists_border~ ln_distance + border)

pred_border <- expand.grid(ln_distance = seq(from = 4, to = 9, length.out = N), border = 0)
pred_noborder <- expand.grid(ln_distance = seq(from = 4, to = 9, length.out = N), border = 0)
pred_island <- expand.grid(ln_distance = seq(from = 4, to = 9, length.out = N), island = 0)
pred_noisland <- expand.grid(ln_distance = seq(from = 4, to = 9, length.out = N), border = 0)

border_out <- predict(object = m_border,
                    newdata = pred_border,
                    interval = "confidence")
pred_border <- cbind(pred_border, border_out)
noborder_out <- predict(object = m_noborder,
                      newdata = pred_border,
                      interval = "confidence")
pred_noborder <- cbind(pred_noborder, noborder_out)

island_out <- predict(object = m_island,
                      newdata = pred_island,
                      interval = "confidence")
pred_island <- cbind(pred_island, island_out)
noisland_out <- predict(object = m_noisland,
                        newdata = pred_island,
                        interval = "confidence")
pred_noisland <- cbind(pred_noisland, noisland_out)

model_colors <- c("tomato", "steelblue")

p_border <- ggplot(data, mapping = aes(x = ln_distance, y = ln_tourists_border)) + 
    geom_point(size = 1, alpha = 0.2, colour = "tomato") + 
    theme_ipsum() +
    geom_point(data = subset(data, border == "1"), mapping = aes(fill = "pink"), shape = 21, stroke = 0.2, alpha = 1, size = 1, colour = "steelblue") +
    geom_line(data = pred_noborder, aes(x = ln_distance, y = fit, color = "Not controlled for border"), size = 1) +
    geom_line(data = pred_border, aes(x = ln_distance, y = fit, color = "Controlled for border"), size = 1) +
    scale_color_manual(name = "Effect of distance", values = model_colors) +
    scale_fill_manual(name = "Effect of distance", values = "steelblue") +
    guides(fill = FALSE) +
    theme(legend.position = "top") + 
    labs(x="Distance (log)", y="Number of tourists (log)") + 
    theme(axis.text.x = element_blank(), axis.text.y = element_blank())

p_island <- ggplot(data, mapping = aes(x = ln_distance, y = ln_tourists_island)) + 
    geom_point(size = 1, alpha = 0.2, colour = "tomato") + 
    theme_ipsum() +
    geom_point(data = subset(data, island == "1"), mapping = aes(fill = "pink"), shape = 21, stroke = 0.2, alpha = 1, size = 1, colour = "steelblue") +
    geom_line(data = pred_noisland, aes(x = ln_distance, y = fit, color = "Not controlled for island"), size = 1) +
    geom_line(data = pred_island, aes(x = ln_distance, y = fit, color = "Controlled for island"), size = 1) +
    scale_color_manual(name = "Effect of distance", values = model_colors) +
    scale_fill_manual(name = "Effect of distance", values = "steelblue") +
    guides(fill = FALSE) +
    theme(legend.position = "top") + 
    labs(x="Distance (log)", y="Number of tourists (log)") + 
    theme(axis.text.x = element_blank(), axis.text.y = element_blank())

cairo_pdf("./figures/direct_indirect_sim.pdf",  width = 10, heigh = 4)
grid.arrange(p_border,p_island, ncol=2)
dev.off()

