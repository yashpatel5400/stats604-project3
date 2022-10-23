# these lines can be replaced by xinhe's code
set.seed(30001)
n_ctrl_bananas <- n_tomato_bananas <- n_apple_bananas <- 8
n_bananas <- sum(c(n_ctrl_bananas, n_tomato_bananas, n_apple_bananas))
n_permutations <- choose(n_bananas, n_ctrl_bananas) * choose(n_bananas - n_ctrl_bananas, n_apple_bananas)
n_test_permutations <- 20000
assignments <- c(rep("C", n_ctrl_bananas), rep("T", n_tomato_bananas), rep("A", n_apple_bananas))
permutations <- data.frame(
  t(mapply(function(x) sample(assignments), seq_len(n_test_permutations))),
  row.names = NULL
)
colnames(permutations) <- paste0("banana_", seq_len(n_bananas))

test_data <- cbind(
  permutations,
  "avg_rgb_tomato" = rnorm(n_test_permutations),
  "avg_rgb_apple" = rnorm(n_test_permutations),
  "pct_brown_tomato" = rbeta(n_test_permutations, 1, 1),
  "pct_brown_apple" = rbeta(n_test_permutations, 1, 1),
  "pct_squished_tomato" = rbeta(n_test_permutations, 1, 1),
  "pct_squished_apple" = rbeta(n_test_permutations, 1, 1)
)

# these lines can be kept, but replace test_data with xinhe's final dataframe
data_to_write <- test_data
write.csv(data_to_write, "permutations.csv", row.names = FALSE)
