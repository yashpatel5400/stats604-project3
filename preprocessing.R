library(imager)
library(dplyr)

data <- data.frame("banana" = seq_len(36),
                   "trt" = rep(c("C", "A", "T"), each = 12))
for (j in 1:5) {
  proportion_black = c()
  for (i in 1:36) {
    #Take the pictures from the folder containing the banana's pics
    if (j == 1) {
      banana = load.image(paste("Day_", j, "/",i,".jpg",sep = ""))
    } else {
      banana = load.image(paste("Day_", j, "/banana_",i,".jpeg",sep = ""))
    }
    
    #plot(banana)
    #class(banana)
    #dim(banana)
    
    # Transform the bananapic in a data frame
    bdf_1 = as.data.frame(banana)
    
    # divide by 3 because there are 3 channels, each of them are repeated (RGB)
    total_area = dim(bdf_1)[1]/3
    white_area = dim(filter(bdf_1, value == 1))[1]/3
    banana_area = total_area - white_area
    
    # sqrt because it helps in dividing the black sports from the white.
    # Transform the bananapic in a greyscale
    gbanana = sqrt(grayscale((banana)))
    #plot(gbanana)
    bdf_2 = as.data.frame(gbanana)
    # Select the most black spots
    black_spots = which(bdf_2$value<0.5)
    # set to white all the other spots (only if you what to see the next plot
    #bdf_2[-black_spots,3]= 1
    
    black_area = length(black_spots)
    
    #as.cimg(bdf_2, dims = dim(gbanana)) %>% plot
    #aggregate(bdf, list(channel = bdf$cc),FUN = mean)
    
    proportion_black = c(proportion_black, black_area/banana_area)
  }
  data <- cbind(data, proportion_black)
}

# data = as.data.frame(proportion_black)
# colnames(data) = c("proportion_black")
colnames(data) <- c("banana", "trt", paste0("pct_brown_day_", seq(1, 5)))
# write.csv(data,"Day_1.csv", row.names = FALSE)

# read in squish results
squish <- read.csv("processed_results/squish.csv", row.names = NULL)
data <- cbind(data, pct_squished = squish$squish)
write.csv(data, "processed_results/aggregated_measurements.csv", row.names = FALSE)
