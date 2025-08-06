#!/usr/bin/env Rscript

# This script loads data from benchmark_results.csv and generates a series
# of plots to visualize and compare the performance of different queue
# implementations and optimization modes.
#
#   Rscript analyze_benchmarks.r
#   Or from the project root: make graphs

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = "http://cran.us.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

# Ensure required packages are installed
install_if_missing("tidyverse")
install_if_missing("scales")

cat("--> All required R packages are installed.\n")

csv_file <- "benchmark_results.csv"
output_dir <- "graphs"

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
  cat(paste("--> Created directory:", output_dir, "\n"))
}

bench_data <- tryCatch({
  read_csv(csv_file, show_col_types = FALSE)
}, error = function(e) {
  cat(paste("Error: The file '", csv_file, "' was not found.\n", sep = ""))
  cat("Please run 'make benchmark-all CSV_LOG=benchmark_results.csv' first.\n")
  quit(status = 1)
})

cat("--> Successfully loaded benchmark_results.csv.\n")

prepared_data <- bench_data %>%
  # For competitors, the optimization mode doesn't apply. We'll label it "N/A".
  mutate(OptimizationMode = if_else(Library != "cl-freelock", "N/A", OptimizationMode)) %>%
  mutate(
    TotalThreads = Producers + Consumers,
    MillionsOpsPerSec = OpsPerSec / 1e6,
    GC_MB = GCBytes / 1e6, # Convert GC bytes to Megabytes
    PlotLabel = case_when(
      Implementation == "Lock-Free-Unbounded" ~ "cl-freelock (Unbounded)",
      Implementation == "Lock-Free-Bounded"  ~ "cl-freelock (Bounded)",
      Implementation == "Lock-Free-Batch"  ~ "cl-freelock (Batch)",
      Implementation == "Lock-Free-SPSC"  ~ "cl-freelock (SPSC)",
      Implementation == "Mutex-Protected-List" ~ "Lock-Based Queue",
      Implementation == "External-Queue" ~ "oconnore/queues",
      TRUE ~ Implementation
    )
  )

cat("--> Data prepared for plotting.\n")

plot_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 15)),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 12),
    # Needs a bottom margin so we can prevent the legend from being cut off.
    plot.margin = margin(t = 10, r = 10, b = 20, l = 10)
  )

mpmc_data <- prepared_data %>%
  filter(
    (grepl("Unbounded", PlotLabel) | grepl("Lock-Based", PlotLabel) | grepl("oconnore", PlotLabel)) &
    QueueType == "Balanced"
  )

plot_scalability <- ggplot(mpmc_data, aes(
    x = TotalThreads,
    y = MillionsOpsPerSec,
    color = PlotLabel,
    linetype = OptimizationMode,
    # We Group by the interaction of both variables to create distinct lines.
    group = interaction(PlotLabel, OptimizationMode)
  )) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = unique(mpmc_data$TotalThreads)) +
  scale_linetype_manual(values = c("Multi-Threaded" = "solid", "Single-Threaded" = "dashed", "N/A" = "solid")) +
  labs(
    title = "MPMC Queue Performance vs. Contention",
    subtitle = "Comparing Multi-Threaded (solid) vs. Single-Threaded (dashed) optimizations.",
    x = "Total Threads (Producers + Consumers)",
    y = "Throughput (Millions of Operations / Second)",
    color = "Implementation",
    linetype = "Optimization"
  ) +
  scale_color_brewer(palette = "Set2") +
  plot_theme +
  theme(legend.title = element_text(face = "bold")) +
  # Arrange the legend into two rows to give it more space
  guides(color = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2))

ggsave(file.path(output_dir, "01_mpmc_scalability_comparison.png"), plot_scalability, width = 12, height = 8, dpi = 300, bg = "white")
cat("--> Saved plot: 01_mpmc_scalability_comparison.png\n")

batch_data <- prepared_data %>%
  filter(Implementation %in% c("Lock-Free-Bounded", "Lock-Free-Batch")) %>%
  mutate(Mode = ifelse(grepl("Batch", Implementation), "Batch Mode (Size 64)", "Single-Item Mode"))

plot_batching <- ggplot(batch_data, aes(x = as.factor(TotalThreads), y = MillionsOpsPerSec, fill = OptimizationMode)) +
  geom_col(position = "dodge") +
  facet_wrap(~Mode, scales = "free_y") + # Separate plots for Batch vs Single, with free y-axis
  labs(
    title = "Bounded Queue: Batching & Optimization Mode",
    subtitle = "Comparing throughput for different modes and optimizations.",
    x = "Total Threads (Producers + Consumers)",
    y = "Throughput (Millions of Operations / Second)",
    fill = "Optimization"
  ) +
  scale_fill_brewer(palette = "Paired") +
  plot_theme +
  theme(legend.title = element_text(face = "bold"))

ggsave(file.path(output_dir, "02_bounded_queue_comparison.png"), plot_batching, width = 12, height = 8, dpi = 300, bg = "white")
cat("--> Saved plot: 02_bounded_queue_comparison.png\n")


spsc_comparison_data <- prepared_data %>%
  filter(
    (QueueType == "SPSC" | (Producers == 1 & Consumers == 1)) &
    !grepl("Batch", Implementation)
  ) %>%
  group_by(PlotLabel) %>%
  filter(MillionsOpsPerSec == max(MillionsOpsPerSec)) %>%
  ungroup() %>%
  mutate(PlotLabel = fct_reorder(PlotLabel, MillionsOpsPerSec))

plot_spsc <- ggplot(spsc_comparison_data, aes(x = PlotLabel, y = MillionsOpsPerSec, fill = PlotLabel)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1fM", MillionsOpsPerSec)), vjust = -0.5, size = 4) +
  coord_flip() +
  labs(
    title = "SPSC Performance",
    subtitle = "Comparing specialized SPSC vs. general queues in a 1P/1C scenario.",
    x = "Queue Implementation",
    y = "Throughput (Millions of Operations / Second)"
  ) +
  plot_theme +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggsave(file.path(output_dir, "03_spsc_specialization.png"), plot_spsc, width = 12, height = 8, dpi = 300, bg = "white")
cat("--> Saved plot: 03_spsc_specialization.png\n")

gc_data <- prepared_data %>%
  filter(Producers == 1, Consumers == 1, !grepl("Batch", Implementation)) %>%
  mutate(PlotLabel = fct_reorder(PlotLabel, GC_MB))

plot_gc <- ggplot(gc_data, aes(x = PlotLabel, y = GC_MB, fill = PlotLabel)) +
  geom_col() +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  geom_text(aes(label = sprintf("%.2f MB", GC_MB)), hjust = -0.1, size = 4) +
  coord_flip() +
  labs(
    title = "Garbage Collection Pressure (1P/1C)",
    subtitle = "Memory allocated to process 1 million items (logarithmic scale).",
    x = "Queue Implementation",
    y = "Memory Allocated (Megabytes)"
  ) +
  plot_theme +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

ggsave(file.path(output_dir, "04_gc_pressure_comparison.png"), plot_gc, width = 12, height = 8, dpi = 300, bg = "white")
cat("--> Saved plot: 04_gc_pressure_comparison.png\n")


cat("\nAll plots have been generated in the 'graphs' directory.\n")
