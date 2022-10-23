# these lines can be replaced by daniele's code
set.seed(30001)
n_ctrl_bananas <- n_tomato_bananas <- n_apple_bananas <- 8
n_bananas <- n_ctrl_bananas + n_tomato_bananas + n_apple_bananas
test_data <- data.frame(
  "banana" = seq_len(n_bananas),
  "trt" = rep(c("C", "A", "T"), each = n_bananas / 3),
  "avg_rgb" = rnorm(n_bananas),
  "pct_brown" = runif(n_bananas),
  "pct_squished" = runif(n_bananas)
)

# these can be kept, but switch out test_data with the name of daniele's final dataframe
data_to_write <- test_data
write.csv(data_to_write, "aggregated_measurements.csv", row.names = FALSE)
