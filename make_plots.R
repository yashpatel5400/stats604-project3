library(grDevices)
permutations <- read.csv("permutations.csv", row.names = NULL)
png("plot1.png")
hist(permutations$avg_rgb_tomato)
dev.off()

png("plot2.png")
hist(permutations$avg_rgb_apple)
dev.off()

png("plot3.png")
hist(permutations$pct_brown_tomato)
dev.off()