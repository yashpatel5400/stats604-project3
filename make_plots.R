library(grDevices)
library(dplyr)
library(tidyr)
library(ggplot2)

permutations <- read.csv("permutations.csv", row.names = NULL)
permutations_tomato <- read.csv("permutations_tomato.csv", row.names = NULL)
permutations_apple <- read.csv("permutations_apple.csv", row.names = NULL)
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
  dplyr::filter(grepl("_day_", name)) %>%
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
n_bananas <- 36
count_plot_df <- rbind(
  dplyr::bind_rows(
    Map(function(col) {
      tidyr::pivot_longer(permutations_apple %>%
                            dplyr::count(!!sym(col)),
                          !!sym(col),
                          names_to = "Banana",
                          values_to = "Treatment") %>%
        dplyr::mutate(Test = "Apple Permutation Test",
                      Banana = factor(gsub("banana_", "", Banana),
                                      levels = seq_len(n_bananas)),
                      Treatment = dplyr::case_when(
                        Treatment == "A" ~ "Apple",
                        Treatment == "T" ~ "Tomato",
                        TRUE ~ "Control"
                      ))
    },
    colnames(permutations_apple)[grepl("banana", colnames(permutations_apple))])),
  dplyr::bind_rows(
    Map(function(col) {
      tidyr::pivot_longer(permutations_tomato %>%
                            dplyr::count(!!sym(col)),
                          !!sym(col),
                          names_to = "Banana",
                          values_to = "Treatment") %>%
        dplyr::mutate(Test = "Tomato Permutation Test",
                      Banana = factor(gsub("banana_", "", Banana),
                                      levels = seq_len(n_bananas)),
                      Treatment = dplyr::case_when(
                        Treatment == "A" ~ "Apple",
                        Treatment == "T" ~ "Tomato",
                        TRUE ~ "Control"
                      ))
    },
    colnames(permutations_tomato)[grepl("banana", colnames(permutations_tomato))]))
)
# 
# assignment_counts <- cbind(
#   sapply(permutations_tomato[, grepl("banana", colnames(permutations_tomato))],
#          table),
#   sapply(permutations_apple[, grepl("banana", colnames(permutations_apple))],
#          table)
# )
# count_plot_df <- cbind(
#   data.frame("count" = matrix(assignment_counts)),
  # "banana" = factor(rep(c(colnames(permutations_tomato)[grepl("banana", colnames(permutations_tomato))],
  #                         colnames(permutations_apple)[grepl("banana", colnames(permutations_apple))]),
  #                         each = 2),
  #                   levels=paste0("banana_", seq_len(n_bananas)),
  #                   labels = seq_len(n_bananas)),
#   "Treatment" = factor(rep(row.names(assignment_counts), n_bananas),
#                        levels=c("C", "A", "T"),
#                        labels=c("Control", "Apple", "Tomato"))
# )

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
  
  banana_cols <- colnames(perms)[grepl("banana", colnames(perms))]
  pivot_data <- Reduce(
    rbind,
    lapply(banana_cols, 
          function(banana_col) {
            msk <- perms[banana_col] == trt
            banana_perms <- perms[msk,
                                  c(banana_col, colnames(perms)
                                    [!grepl("banana", colnames(perms))])]
            colnames(banana_perms)[1] <- "trt"
            cbind("Banana" = as.numeric(gsub("banana_", "", banana_col)),
                  banana_perms)
          })
  )
  pivot_data$Banana <- factor(pivot_data$Banana,
                              levels = as.numeric(gsub("banana_", "", banana_cols)))

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
  # clean_stat <- gsub("% squished", "Squish Ratio",
  #        gsub(
  #          "pct", "%", gsub(
  #            "avg", "Average", gsub(
  #              "rgb", "RGB", gsub(" (tomato|apple)$", "", gsub("_", " ", stat))))))

  ctrl_data <- data.frame(
    "trt" = perms[[banana_col]][perms[banana_col] == "C"],
    "perm_val" = perms[[stat]][perms[banana_col] == "C"]
  )
  trt_data <- data.frame(
    "trt" = perms[[banana_col]][perms[banana_col] == trt],
    "perm_val" = perms[[stat]][perms[banana_col] == trt]
  )
  legend_x_val <- min(quantile(ctrl_data[["perm_val"]], 0.01),
                      quantile(trt_data[["perm_val"]], 0.01))
  if (grepl("pct_brown", stat)) {
    cut_val <- 0.00025
  } else if (grepl("avg_rgb", stat)) {
    cut_val <- 0.0015
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
ggsave(paste(plot_folder, "plot1.png", sep="/"), width = 20, height = 15, units = "cm")

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
ggsave(paste(plot_folder, "plot2.png", sep="/"), width = 20, height = 15, units = "cm")

ggplot(count_plot_df[count_plot_df$Test == "Apple Permutation Test",]) +
  geom_col(aes(x=n, y=Banana, fill=Treatment),
           position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = c("Control" = GREEN, "Apple" = YELLOW, "Tomato" = BROWN)) +
  labs(x="N Permutations", y="Banana") +
  plot_theme
ggsave(paste(plot_folder, "plot3a.png", sep="/"), width = 16, height = 18, units = "cm")

ggplot(count_plot_df[count_plot_df$Test == "Tomato Permutation Test",]) +
  geom_col(aes(x=n, y=Banana, fill=Treatment),
           position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = c("Control" = GREEN, "Apple" = YELLOW, "Tomato" = BROWN)) +
  labs(x="N Permutations", y="Banana") +
  plot_theme
ggsave(paste(plot_folder, "plot3b.png", sep="/"), width = 16, height = 18, units = "cm")

for (r in seq_along(stat_cols)) {
  stat_col <- stat_cols[r]
  if (grepl("apple", stat_col)) {
    make_all_bananas_comparison_plot(permutations_apple, stat_col)
  } else if (grepl("tomato", stat_col)) {
    make_all_bananas_comparison_plot(permutations_tomato, stat_col)
  } else {
    stop("Did not encounter a valid stat col")
  }
  ggsave(paste(plot_folder, paste0("plot4", letters[r], ".png"), sep="/"),
         width = 20, height = 15, units = "cm")
}

make_banana_specific_comparison_plot(permutations, 28, "pct_squished")
ggsave(paste(plot_folder, "plot5.png", sep="/"), width = 25, height = 15, units = "cm")
make_banana_specific_comparison_plot(permutations_tomato, 30, "pct_brown_tomato")
ggsave(paste(plot_folder, "plot6.png", sep="/"), width = 25, height = 15, units = "cm")
make_banana_specific_comparison_plot(permutations_apple, 19, "avg_rgb_apple")
ggsave(paste(plot_folder, "plot7.png", sep="/"), width = 25, height = 15, units = "cm")

