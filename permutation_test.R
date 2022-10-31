set.seed(30001)

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

outcome_brown <- read.csv("processed_results/aggregated_measurements_black.csv")
outcome_rgb <- read.csv("processed_results/aggregated_measurements_rgb.csv")
outcome_squish <- read.csv("processed_results/squish.csv")

obs <- data.frame(
  trt = rep(c("C", "A", "T"), each = 12),
  avg_rgb = outcome_rgb$avg_rgb_day_5 - outcome_rgb$avg_rgb_day_1,
  pct_brown = outcome_brown$pct_brown_day_5 - outcome_brown$pct_brown_day_1,
  pct_squished = outcome_squish$squish
)

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
  rep("A", n_apple_bananas), 
  rep("T", n_tomato_bananas)
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
data_to_write <- cbind(test_data, permutations)
#head(data_to_write)
write.csv(data_to_write, "permutations.csv", row.names = FALSE)


# p-values
# first compute the difference-in-means
dim_stats_t <- 
  colMeans(obs[obs$trt=="T", outcome_colnames]) - 
  colMeans(obs[obs$trt=="C", outcome_colnames])
dim_stats_a <- 
  colMeans(obs[obs$trt=="A", outcome_colnames]) - 
  colMeans(obs[obs$trt=="C", outcome_colnames])

# then compute the p-values
p1 <- sum(dim_stats_t["avg_rgb"] <= test_data$avg_rgb_tomato) / n_test_permutations
p2 <- sum(dim_stats_a["avg_rgb"] <= test_data$avg_rgb_apple) / n_test_permutations

p3 <- sum(dim_stats_t["pct_brown"] <= test_data$pct_brown_tomato) / n_test_permutations
p4 <- sum(dim_stats_a["pct_brown"] <= test_data$pct_brown_apple) / n_test_permutations

p5 <- sum(dim_stats_t["pct_squished"] <= test_data$pct_squished_tomato) / n_test_permutations
p6 <- sum(dim_stats_a["pct_squished"] <= test_data$pct_squished_apple) / n_test_permutations

p.values <- data.frame("rgb" = c(p1, p2),
                       "brown" = c(p3, p4),
                       "squished" = c(p5, p6))
rownames(p.values) <- c("tomato", "apple")
write.csv(p.values, "p-values.csv")
