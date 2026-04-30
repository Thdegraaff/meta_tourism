library("tidyverse")
library("here")

l1 <- -0.82
l2 <- -0.54

x <- seq(0, 2, 0.1)
y1 <- exp(x*l1)
y2 <- exp(x*l2)

figure_path <- here::here("figures", "dist_decay_example.pdf")

pdf(file = figure_path,   # The directory you want to save the file in
    width = 5, # The width of the plot in inches
    height = 5) # The height of the plot in inches

plot(x, y1, type = "l", axis = F, pch = 19, axes = FALSE,  
     col = "red", xlab = "", ylab = "",  ylim=c(0, 1))
# Add a second line
lines(x, y2, pch = 18, col = "blue", type = "l", lty = 2)
Axis(side = 1, labels = FALSE, tck = 0)
Axis(side = 2, labels = FALSE, tck = 0)
legend("bottomleft", legend=c("Total effect (-0.82)", "Direct effect (-0.54)"),
       col=c("red", "blue"), lty = 1:2, bty = "n")
title(ylab = "Size of tourist flow", xlab = "Distance", line=0.5, cex.lab=1.0 )

dev.off()
