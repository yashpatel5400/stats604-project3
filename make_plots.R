library(grDevices)
library(dplyr)
library(tidyr)
library(ggplot2)

permutations <- read.csv("permutations.csv", row.names = NULL)
measurements <- read.csv("aggregated_measurements.csv", row.names = NULL)
GREEN <- "#12b30c"
BROWN <- "#4a4d09"
YELLOW <- "#f2db2c"
PVAL_BLUE <- "#0726f0"
title_size <- 16
text_size <- 14
plot_theme <- theme(panel.background = element_blank(),
                    axis.text = element_text(size = text_size),
                    axis.title = element_text(size = title_size),
                    legend.text = element_text(size = text_size),
                    legend.title = element_text(size = title_size),
                    strip.text = element_text(size = text_size))

stat_cols <- colnames(permutations)[!grepl("banana", colnames(permutations))]
banana_cols <- colnames(permutations)[grepl("banana", colnames(permutations))]
n_bananas <- length(banana_cols)
true_test_statistics <- mapply(
  function(stat) {
    if (grepl("apple$", stat)) {
      trt <- "A"
      stat <- gsub("_apple", "", stat)
    } else if (grepl("tomato$", stat)) {
      trt <- "T"
      stat <- gsub("_tomato", "", stat)
    } else {
      stop("Did not encounter a valid treatment level")
    }
    mean(measurements[measurements$trt == trt, stat]) - mean(measurements[measurements$trt == "C", stat])
  },
  stat_cols,
  USE.NAMES = TRUE
)

# plot 1 data
pivoted_measurements <- tidyr::pivot_longer(measurements, -c("banana", "trt"))
pivoted_measurements <- pivoted_measurements %>%
  dplyr::mutate(value = ifelse(name == "avg_rgb", value, value * 100),
                name = gsub("pct", "%",
                            gsub("avg", "Average",
                                 gsub("rgb", "RGB",
                                      gsub("_", " ", name)))),
                Treatment = factor(trt,
                                   levels = c("C", "A", "T"),
                                   labels = c("Control", "Apple", "Tomato")))

# plot 2 data
assignment_counts <- sapply(permutations[, banana_cols], table)
count_plot_df <- cbind(
  data.frame("count" = matrix(assignment_counts)),
  "banana" = factor(rep(banana_cols, each = 3),
                    levels=paste0("banana_", seq_len(n_bananas)),
                    labels = seq_len(n_bananas)),
  "Treatment" = factor(rep(row.names(assignment_counts), n_bananas),
                       levels=c("C", "A", "T"),
                       labels=c("Control", "Apple", "Tomato"))
)

# plot 3a
make_all_bananas_comparison_plot <- function(perms, stat) {
  if (grepl("tomato$", stat)) {
    trt <- "T"
    clean_trt <- "Tomato"
  } else if (grepl("apple$", stat)) {
    trt <- "A"
    clean_trt <- "Apple"
  } else {
    trt <- "C"
    clean_trt <- "Control"
  }
  clean_stat <- paste(
    gsub(
      "pct", "%", gsub(
        "avg", "Average", gsub(
          "rgb", "RGB", gsub(" (tomato|apple)$", "", gsub("_", " ", stat))))),
    clean_trt, "vs. Control", collapse = " ")
  
  pivot_data <- Reduce(
    rbind,
    lapply(seq_len(n_bananas), 
          function(num) {
            banana_col <- paste0("banana_", num)
            msk <- perms[banana_col] == trt
            banana_perms <- perms[msk,
                                  c(banana_col, colnames(perms)
                                    [!grepl("banana", colnames(perms))])]
            colnames(banana_perms)[1] <- "trt"
            cbind("Banana" = num, banana_perms)
          })
  )
  pivot_data$Banana <- factor(pivot_data$Banana,
                              levels = seq_len(n_bananas))

  # running one-sided tests because trt should increase avg RGB and pct brown
  pval <- sum(perms[[stat]] >= true_test_statistics[stat]) / nrow(perms)
  pval_label_x_val <- true_test_statistics[stat]
  pval_label_y_val <- max(table(cut_width(perms[[stat]], width = 0.03)))
  ggplot(perms) +
    geom_histogram(aes(x = !!sym(stat)), fill = BROWN) +
    geom_histogram(data = pivot_data,
                   aes(x = !!sym(stat), fill = Banana),
                   position = "identity") +
    geom_vline(aes(xintercept=true_test_statistics[stat]), color = PVAL_BLUE,
               linetype="dashed") +
    scale_fill_manual(
      values = scales::seq_gradient_pal(YELLOW, GREEN)(seq(0, 1, length.out=n_bananas))) +
    labs(x = paste0(clean_stat," Test Statistic"),
         y = "N Permutations") +
    annotate("label", x = pval_label_x_val, y = pval_label_y_val,
             label = paste0("P-value: ", round(pval, 3)),
             color = PVAL_BLUE, size = 5) +
    plot_theme
}

# plots 3b func
make_banana_specific_comparison_plot <- function(perms, banana, stat) {
  banana_col <- paste0("banana_", banana)
  if (grepl("tomato$", stat)) {
    trt <- "T"
    clean_trt <- "Tomato"
  } else if (grepl("apple$", stat)) {
    trt <- "A"
    clean_trt <- "Apple"
  } else {
    trt <- "C"
    clean_trt <- "Control"
  }
  clean_stat <- paste(
    gsub(
      "pct", "%", gsub(
        "avg", "Average", gsub(
          "rgb", "RGB", gsub(" (tomato|apple)$", "", gsub("_", " ", stat))))),
    clean_trt, "vs. Control", collapse = " ")

  msk <- perms[banana_col] == trt
  legend_x_val <- min(quantile(perms[msk, stat], 0.001),
                      quantile(perms[!msk, stat], 0.001))
  legend_y_val_banana <- max(table(cut_width(perms[msk, stat], width = 0.2)))
  legend_y_val_all <- max(table(cut_width(perms[!msk, stat], width = 0.2)))

  ggplot(perms[!msk, ]) +
    geom_histogram(aes(x = !!sym(stat)), fill = BROWN, color = BROWN) +
    geom_histogram(data = perms[msk,],
                   aes(x = !!sym(stat)), fill = GREEN, color = GREEN) +
    annotate("label", x = legend_x_val, y = legend_y_val_banana,
             label = paste0("Banana ", banana, " == ", clean_trt, "\n Distribution"),
             color = GREEN, size = 6) +
    annotate("label", x = legend_x_val, y = legend_y_val_all,
             label = paste0("Banana ", banana, " != ", clean_trt, "\n Distribution"),
             color = BROWN, size = 6) +
    scale_y_continuous(
      labels = scales::label_number(accuracy = 1L, scale = 1 / 1e3, suffix = "k")) +
    labs(x = paste0(clean_stat," Test Statistic"),
         y = "N Permutations") +
    plot_theme
}

# make plots
pivoted_measurements %>%
  ggplot() +
  geom_point(aes(x = banana, y = value, color = Treatment)) +
  facet_wrap(~ name, ncol = 1, scale = "free_y") +
  scale_color_manual(values = c("Control" = GREEN, "Apple" = YELLOW, "Tomato" = BROWN)) +
  labs(x = "Banana", y = "Measurement Value") +
  plot_theme
ggsave("plot1.png")

ggplot(count_plot_df) +
  geom_col(aes(x=count, y=banana, fill=Treatment),
           position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = c("Control" = GREEN, "Apple" = YELLOW, "Tomato" = BROWN)) +
  labs(x="N Permutations", y="Banana") +
  plot_theme
ggsave("plot2.png")

for (r in seq_along(stat_cols)) {
  make_all_bananas_comparison_plot(permutations, stat_cols[r])
  ggsave(paste0("plot3", letters[r], ".png"))
}

