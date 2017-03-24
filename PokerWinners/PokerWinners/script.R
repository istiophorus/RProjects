
library(ggplot2)

poker.results <- read.csv("results2.txt", header = TRUE, sep = ";")

x <- seq(1, 10, 1)

pdf("results2.pdf", width = 14, height = 14)

plot(x, as.numeric(poker.results[1,])[-1], type = "l", col = "red")
lines(x, as.numeric(poker.results[2,])[-1], col = "green")
lines(x, as.numeric(poker.results[3,])[-1], col = "blue")
lines(x, as.numeric(poker.results[4,])[-1], col = "gray")
lines(x, as.numeric(poker.results[5,])[-1], col = "black")
lines(x, as.numeric(poker.results[6,])[-1], col = "brown")
lines(x, as.numeric(poker.results[7,])[-1], col = "purple")
lines(x, as.numeric(poker.results[8,])[-1], col = "magenta")

dev.off()

