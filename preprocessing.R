# these lines can be replaced by daniele's code
set.seed(30001)
n_ctrl_bananas <- n_tomato_bananas <- n_apple_bananas <- 8
n_bananas <- n_ctrl_bananas + n_tomato_bananas + n_apple_bananas
test_data <- data.frame(
  "banana" = seq_len(n_bananas),
  "avg_rgb_tomato" = rnorm(n_bananas),
  "avg_rgb_apple" = rnorm(n_bananas),
  "pct_brown_tomato" = rbeta(n_bananas, 1, 1),
  "pct_brown_apple" = rbeta(n_bananas, 1, 1),
  "pct_squished_tomato" = rbeta(n_bananas, 1, 1),
  "pct_squished_apple" = rbeta(n_bananas, 1, 1)
)
write.csv(test_data, "aggregated_measurements.csv")
