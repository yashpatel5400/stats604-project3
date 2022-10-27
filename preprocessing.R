library(imager)
library(dplyr)

data_black <- data.frame("banana" = seq_len(36),
                   "trt" = rep(c("C", "A", "T"), each = 12))
data_yellow <- data.frame("banana" = seq_len(36),
                   "trt" = rep(c("C", "A", "T"), each = 12))
data_avg_r <- data.frame("banana" = seq_len(36),
                         "trt" = rep(c("C", "A", "T"), each = 12))

for (j in 1:5) {
  proportion_black = c()
  proportion_yellow = c()
  avg_r = c()
  for (i in 1:36) {
    # Take the pictures from the folder containing the banana's pics
    banana = load.image(paste("Day_", j, "/",i,".jpg",sep = ""))
    
    #plot(banana)
    #class(banana)
    #dim(banana)
    
    # Transform the bananapic in a data frame
    bdf_1 = as.data.frame(banana)
    
    # divide by 3 because there are 3 channels, each of them are repeated (RGB)
    total_area = dim(bdf_1)[1]/3
    white_area = dim(filter(bdf_1, value == 1))[1]/3
    banana_area = total_area - white_area
    
    # Transform the bananapic in a greyscale
    # sqrt because it helps dividing the black spots from the white.
    gbanana = sqrt(grayscale((banana)))
    #plot(gbanana)
    
    bdf_2 = as.data.frame(gbanana)
    # Select the most black spots
    black_spots = which(bdf_2$value<0.5)
    # Set to white all the other spots (only if you what to see the next plot
    #bdf_2[-black_spots,3]= 1
    
    black_area = length(black_spots)
    yellow_area = banana_area - length(black_spots)
    
    #as.cimg(bdf_2, dims = dim(gbanana)) %>% plot
    #aggregate(bdf, list(channel = bdf$cc),FUN = mean)
    
    proportion_black = c(proportion_black, black_area/banana_area)
    proportion_yellow = c(proportion_yellow, yellow_area/banana_area)
    
    # average of the R channel, excluding white area and black spots
    avg_r_channel <- mean(
      filter(bdf_1[bdf_1$cc == 1, ][bdf_2$value < 0.5, ], value < 1)$value)
    
    avg_r <- c(avg_r, avg_r_channel)
  }
  data_black <- cbind(data_black, proportion_black)
  data_yellow <- cbind(data_yellow, proportion_yellow)
  data_avg_r <- cbind(data_avg_r, avg_r)
}

colnames(data_black) <- c("banana", "trt", paste0("pct_brown_day_", seq(1, 5)))
colnames(data_yellow) <- c("banana", "trt", paste0("pct_yellow_day_", seq(1, 5)))
colnames(data_avg_r) <- c("banana", "trt", paste0("avg_rgb_day_", seq(1, 5)))

# read in squish results
# squish <- read.csv("processed_results/squish.csv", row.names = NULL)
# data <- cbind(data, pct_squished = squish$squish)
write.csv(data_black, "processed_results/aggregated_measurements_black.csv", row.names = FALSE)
write.csv(data_yellow, "processed_results/aggregated_measurements_yellow.csv", row.names = FALSE)
write.csv(data_avg_r, "processed_results/aggregated_measurements_rgb.csv", row.names = FALSE)

