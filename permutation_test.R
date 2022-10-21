# these lines can be replaced by xinhe's code
set.seed(30001)
n_ctrl_bananas <- n_tomato_bananas <- n_apple_bananas <- 8
n_permutations <- choose(n_ctrl_bananas + n_tomato_bananas, n_tomato_bananas)
test_data <- data.frame(
  "permutation" = seq_len(n_permutations),
  "avg_rgb_tomato" = rnorm(n_permutations),
  "avg_rgb_apple" = rnorm(n_permutations),
  "pct_brown_tomato" = rbeta(n_permutations, 1, 1),
  "pct_brown_apple" = rbeta(n_permutations, 1, 1),
  "pct_squished_tomato" = rbeta(n_permutations, 1, 1),
  "pct_squished_apple" = rbeta(n_permutations, 1, 1)
)

# these lines can be kept, but replace test_data with xinhe's final dataframe
data_to_write <- test_data
write.csv(test_data, "permutations.csv", row.names = FALSE)
