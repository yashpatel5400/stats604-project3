# these lines can be replaced by xinhe's code
set.seed(30001)

obs <- read.csv(aggregated_measurements.csv)

n_ctrl_bananas <- n_tomato_bananas <- n_apple_bananas <- 12
n_bananas <- sum(c(n_ctrl_bananas, n_tomato_bananas, n_apple_bananas))
# obs <- data.frame(
#  "banana" = seq_len(n_bananas),
#  "trt" = rep(c("C", "A", "T"), each = n_bananas / 3),
#  "avg_rgb" = rnorm(n_bananas),
#  "pct_brown" = runif(n_bananas),
#  "pct_squished" = runif(n_bananas)
#)
outcome_colnames <- c("avg_rgb", "pct_brown", "pct_squished")

n_permutations <- choose(n_bananas, n_ctrl_bananas) * 
  choose(n_bananas - n_ctrl_bananas, n_apple_bananas)
n_test_permutations <- 20000

test_data <- data.frame(
  "permutation" = seq_len(n_test_permutations),
  "avg_rgb_tomato" = rnorm(n_test_permutations),
  "avg_rgb_apple" = rnorm(n_test_permutations),
  "pct_brown_tomato" = rbeta(n_test_permutations, 1, 1),
  "pct_brown_apple" = rbeta(n_test_permutations, 1, 1),
  "pct_squished_tomato" = rbeta(n_test_permutations, 1, 1),
  "pct_squished_apple" = rbeta(n_test_permutations, 1, 1)
)

# Josh's code of generating permutations
assignments <- c(
  rep("C", n_ctrl_bananas), 
  rep("T", n_tomato_bananas), 
  rep("A", n_apple_bananas)
)
permutations <- data.frame(
  t(mapply(function(x) sample(assignments), seq_len(n_test_permutations))),
  row.names = NULL
)
colnames(permutations) <- paste0("banana_", seq_len(n_bananas)) 

# find the test statistic (difference-in-means) for all test permutations
for (i in 1:n_test_permutations){
  # compute the difference-in-means
  test_stats_t <- 
    colMeans(obs[permutations[i, ]=="T", outcome_colnames]) - 
    colMeans(obs[permutations[i, ]=="C", outcome_colnames])
  test_stats_a <- 
    colMeans(obs[permutations[i, ]=="A", outcome_colnames]) - 
    colMeans(obs[permutations[i, ]=="C", outcome_colnames])
  
  test_data[i, "avg_rgb_tomato"] <- test_stats_t["avg_rgb"]
  test_data[i, "pct_brown_tomato"] <- test_stats_t["pct_brown"]
  test_data[i, "pct_squished_tomato"] <- test_stats_t["pct_squished"]
  
  test_data[i, "avg_rgb_apple"] <- test_stats_a["avg_rgb"]
  test_data[i, "pct_brown_apple"] <- test_stats_a["pct_brown"]
  test_data[i, "pct_squished_apple"] <- test_stats_a["pct_squished"]
}

# these lines can be kept, but replace test_data with xinhe's final dataframe
data_to_write <- test_data
write.csv(test_data, "permutations.csv", row.names = FALSE)
