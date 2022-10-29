library(grDevices)
library(dplyr)
library(tidyr)
library(ggplot2)

permutations <- read.csv("permutations.csv", row.names = NULL)
data_black <- read.csv("processed_results/aggregated_measurements_black.csv", row.names = NULL)
data_yellow <- read.csv("processed_results/aggregated_measurements_yellow.csv", row.names = NULL)
data_avg_r <- read.csv("processed_results/aggregated_measurements_rgb.csv", row.names = NULL)
squish <- read.csv("processed_results/squish.csv", row.names = NULL)
squish$banana <- gsub("banana_", "", squish$X)
squish$pct_squished <- squish$squish
measurements <- merge(data_avg_r,
                      merge(data_black, data_yellow, by = c("banana", "trt")),
                      by = c("banana", "trt"))
measurements <- merge(measurements, squish[, c("banana", "pct_squished")])
measurements$avg_rgb <- measurements$avg_rgb_day_5 - measurements$avg_rgb_day_1
measurements$pct_brown <- measurements$pct_brown_day_5 - measurements$pct_brown_day_1

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
plot_folder <- "plots"

stat_cols <- colnames(permutations)[!grepl("(banana|permutation)", colnames(permutations))]
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
  dplyr::mutate(day = gsub("day", "Day",
                           gsub("_", " ",
                                gsub("(avg_rgb|pct_brown|pct_yellow)_", "", name))),
                value = ifelse(gsub("_day_\\d{1}", "", name) == "avg_rgb",
                                    value, value * 100),
                name = gsub("pct", "%",
                            gsub("avg", "Average",
                                 gsub("rgb", "RGB",
                                      gsub("_", " ",
                                           gsub("_day_\\d{1}", "", name))))),
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
    gsub("% squished", "Squish Ratio",
      gsub(
        "pct", "%", gsub(
          "avg", "Average", gsub(
            "rgb", "RGB", gsub(" (tomato|apple)$", "", gsub("_", " ", stat)))))),
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
  pval <- mean(perms[[stat]] >= true_test_statistics[stat])
  if (grepl("pct_brown", stat)) {
    pval_label_x_val <- true_test_statistics[stat] + sign(true_test_statistics[stat]) * 0.002
    cut_val <- 0.00025
  } else if (grepl("avg_rgb", stat)) {
    pval_label_x_val <- true_test_statistics[stat] + sign(true_test_statistics[stat]) * 0.01
    cut_val <- 0.0025
  } else {
    pval_label_x_val <- true_test_statistics[stat] + sign(true_test_statistics[stat]) * 0.1
    cut_val <- 0.025
  }
  
  pval_label_y_val <- max(table(cut_width(perms[[stat]], width = cut_val)))
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
  # if (grepl("tomato$", stat)) {
  #   trt <- "T"
  #   clean_trt <- "Tomato"
  # } else if (grepl("apple$", stat)) {
  #   trt <- "A"
  #   clean_trt <- "Apple"
  # } else {
  #   trt <- "C"
  #   clean_trt <- "Control"
  # }
  # clean_stat <- paste(
  #   gsub("% squished", "Squish Ratio",
  #        gsub(
  #          "pct", "%", gsub(
  #            "avg", "Average", gsub(
  #              "rgb", "RGB", gsub(" (tomato|apple)$", "", gsub("_", " ", stat)))))),
  #   clean_trt, "vs. Control", collapse = " ")
  clean_stat <- gsub("% squished", "Squish Ratio",
         gsub(
           "pct", "%", gsub(
             "avg", "Average", gsub(
               "rgb", "RGB", gsub(" (tomato|apple)$", "", gsub("_", " ", stat))))))

  ctrl_data <- data.frame(
    "trt" = c(perms[[banana_col]][perms[banana_col] == "C"],
              perms[[banana_col]][perms[banana_col] == "C"]),
    "perm_val" = c(perms[[paste0(stat, "_tomato")]][perms[banana_col] == "C"],
                   perms[[paste0(stat, "_apple")]][perms[banana_col] == "C"])
  )
  trt_data <- data.frame(
    "trt" = c(perms[[banana_col]][perms[banana_col] == "T"],
              perms[[banana_col]][perms[banana_col] == "A"]),
    "perm_val" = c(perms[[paste0(stat, "_tomato")]][perms[banana_col] == "T"],
                   perms[[paste0(stat, "_apple")]][perms[banana_col] == "A"])
  )
  legend_x_val <- min(quantile(ctrl_data[["perm_val"]], 0.005),
                      quantile(trt_data[["perm_val"]], 0.005))
  if (grepl("pct_brown", stat)) {
    cut_val <- 0.00025
  } else if (grepl("avg_rgb", stat)) {
    cut_val <- 0.0025
  } else {
    cut_val <- 0.025
  }
  
  legend_y_val_banana <- max(table(cut_width(ctrl_data[["perm_val"]], width = cut_val)))
  legend_y_val_all <- max(table(cut_width(trt_data[["perm_val"]], width = cut_val))) - 400
  fill_col <- scales::seq_gradient_pal(YELLOW, GREEN)(seq(0, 1, length.out=n_bananas))[banana]
  
  ggplot(ctrl_data) +
    geom_histogram(data = trt_data,
                   aes(x = perm_val), fill = fill_col, color = fill_col) +
    geom_histogram(aes(x = perm_val), fill = BROWN, color = BROWN) +
    annotate("label", x = legend_x_val, y = legend_y_val_banana,
             label = paste0("Banana ", banana, " Does Not Have\nControl Label Distribution"),
             color = fill_col, size = 6) +
    annotate("label", x = legend_x_val, y = legend_y_val_all,
             label = paste0("Banana ", banana, " Has Control\nLabel Distribution"),
             color = BROWN, size = 6) +
    labs(x = paste0(clean_stat," Test Statistic"),
         y = "N Permutations") +
    plot_theme
}

# make plots
pivoted_measurements %>%
  dplyr::filter(name != "% yellow") %>%
  ggplot() +
  geom_point(aes(x = banana, y = value, color = Treatment),
             size = 2) +
  facet_grid(name~day, scale = "free_y") +
  scale_color_manual(values = c("Control" = GREEN, "Apple" = YELLOW, "Tomato" = BROWN)) +
  labs(x = "Banana", y = "Measurement Value") +
  plot_theme
ggsave(paste(plot_folder, "plot1.png", sep="/"))

measurements %>%
  dplyr::mutate(Treatment = factor(trt,
                                   levels = c("C", "A", "T"),
                                   labels = c("Control", "Apple", "Tomato"))) %>%
  ggplot() +
  geom_point(aes(x = banana, y = pct_squished, color = Treatment),
             size = 3) +
  scale_color_manual(values = c("Control" = GREEN, "Apple" = YELLOW, "Tomato" = BROWN)) +
  labs(x = "Banana", y = "Ratio of Area After to Area Before Squish") +
  plot_theme
ggsave(paste(plot_folder, "plot2.png", sep="/"))

ggplot(count_plot_df) +
  geom_col(aes(x=count, y=banana, fill=Treatment),
           position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = c("Control" = GREEN, "Apple" = YELLOW, "Tomato" = BROWN)) +
  labs(x="N Permutations", y="Banana") +
  plot_theme
ggsave(paste(plot_folder, "plot3.png", sep="/"))

for (r in seq_along(stat_cols)) {
  make_all_bananas_comparison_plot(permutations, stat_cols[r])
  ggsave(paste(plot_folder, paste0("plot4", letters[r], ".png"), sep="/"))
}

make_banana_specific_comparison_plot(permutations, 28, "pct_squished")
ggsave(paste(plot_folder, "plot5.png", sep="/"))
make_banana_specific_comparison_plot(permutations, 30, "pct_brown")
ggsave(paste(plot_folder, "plot6.png", sep="/"))
make_banana_specific_comparison_plot(permutations, 30, "avg_rgb")
ggsave(paste(plot_folder, "plot7.png", sep="/"))

