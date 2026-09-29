data <- readRDS(here("data", "preprocessed", "data.rds"))

# --- PREPARE DATA FOR PLOTTING --- # 

# Calculate mean and standard error of the mean (SEM) for each Object.Name
mean_data <- data %>%
  group_by(Object.Name, Environment) %>%
  summarise(
    mean_response = mean(Response, na.rm = TRUE),
    std_err = se(Response),
    .groups = 'drop'
  )

mean_data_uni <- mean_data %>% filter(`Object.Name` %in% emo_uni)
mean_data_bi <- mean_data %>% filter(`Object.Name` %in% emo_bi)

# mean response for each video
video_means <- data %>%
  group_by(Object.Name, Environment, Choice) %>%
  summarize(mean_response = mean(Response, na.rm = TRUE), .groups = "drop")

video_means_uni <- video_means %>% filter(`Object.Name` %in% emo_uni)
video_means_bi <- video_means %>% filter(`Object.Name` %in% emo_bi)

# --- PLOT EMOTION MEANS --- # 

plot_emotion_means <- function(df, plot_title = "") {
  
  # group by BOTH Object.Name AND environment
  summary_data <- df %>%
    group_by(Object.Name, Environment) %>%
    summarize(
      mean_val = mean(mean_response, na.rm = TRUE),
      sd_val   = sd(mean_response, na.rm = TRUE),
      n        = n(),
      se_val   = sd_val / sqrt(n),
      .groups  = "drop"
    )
  
  ggplot(summary_data, aes(x = reorder(Object.Name, mean_val), y = mean_val)) +
    geom_errorbar(
      aes(ymin = mean_val - se_val, ymax = mean_val + se_val),
      width = 0.25,
      color = "#4A6572",
      linewidth = 0.7
    ) +
    geom_point(color = "#1A365D", size = 3) +
    geom_text(
      aes(label = sprintf("%.2f", mean_val)),
      vjust = -0.7,
      size = 3.5,
      fontface = "bold"
    ) +
    facet_wrap(~ Environment) +
    coord_flip() +
    scale_y_continuous(limits = c(0, 9.5), breaks = 1:9) +
    labs(
      x = "",
      y = "Mean Response (± SE)",
      title = plot_title
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
      axis.line = element_blank(),
      panel.grid.major.x = element_line(color = "grey90", linewidth = 0.5, linetype = "dashed"),
      strip.background = element_rect(fill = "#EFEFEF", color = "black"),
      strip.text = element_text(face = "bold", size = 11),
      plot.title = element_text(face = "bold", size = 14)
    )
}

#plot_uni <- plot_emotion_means(video_means_uni, "")
#plot_uni <- plot_emotion_means(video_means_uni, "")
print(plot_emotion_means(video_means_uni, ""))
print(plot_emotion_means(video_means_bi, ""))


# --- PLOT VIDEO MEANS --- # 

create_boxplot <- function(df){
  
  boxplot <- ggplot(df, aes(x = Environment, y = mean_response, fill = Environment)) +
    geom_boxplot(
      width = 0.6,
      position = position_dodge(0.8),
      outlier.shape = NA,
      alpha = 0.5
    ) +
    geom_jitter(
      position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),
      color = "black", 
      shape = 21,      
      size = 2,
      alpha = 0.8
    ) +
    facet_wrap(~ Object.Name) +
    scale_y_continuous(limits = c(1, 9), breaks = 1:9) +
    scale_fill_brewer(palette = "Set2") +
    labs(x = "", y = "Mean Rating / Video", fill = "Environment") +
    theme_classic(base_size = 14, base_family = "sans") +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7), 
      axis.line = element_blank(),
      panel.grid.major.y = element_line(color = "grey85", linewidth = 0.5, linetype = "dashed"),
      legend.position = "bottom",
      # strip.background = element_blank(),
      strip.background = element_rect(fill = NA, color = "black", linewidth = 0.7), # Box around facet titles
      #    strip.text = element_text(size = 12, face = "bold", margin = margin(t = 4, b = 4))
    )
  
  return(boxplot)
}

boxplot_uni <- create_boxplot(video_means_uni)
print(boxplot_uni)

boxplot_bi <- create_boxplot(video_means_bi)
print(boxplot_bi)


##################################
# VALENCE AND AROUSAL OF STIMULI #
##################################

plot_data <- data %>%
  filter(Object.Name %in% c("Unpleasant-Pleasant", "Aroused-Calm")) %>%
  group_by(Choice, Environment, Object.Name) %>%
  summarise(Mean_Rating = mean(Response, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = Object.Name, values_from = Mean_Rating)

ggplot(plot_data, aes(x = `Unpleasant-Pleasant`, y = `Aroused-Calm`, color = Environment)) +
  # Add quadrant lines (the midpoint is 5)
  geom_vline(xintercept = 5, linetype = "dashed", color = "gray70") +
  geom_hline(yintercept = 5, linetype = "dashed", color = "gray70") +
  
  geom_point(size = 3.5, alpha = 0.8) +
  scale_color_brewer(palette = "Set2") +
  
  labs(
    title = "",
    subtitle = "",
    x = "Valence (Unpleasant ↔ Pleasant)",
    y = "Arousal (Aroused ↔ Calm)"
  ) +
  scale_x_continuous(limits = c(1, 9), breaks = 1:9) +
  scale_y_continuous(limits = c(1, 9), breaks = 1:9) +
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  )


# --- SAVE MEAN DATA --- #
saveRDS(mean_data, file = here("data", "preprocessed", "mean_emotion_data.rds"))
saveRDS(mean_data_uni, here("data", "preprocessed", "mean_emotion_uni.rds"))
saveRDS(mean_data_bi, here("data", "preprocessed", "mean_emotion_bi.rds"))
saveRDS(video_means, here("data", "preprocessed", "video_means.rds"))
saveRDS(video_means_uni, here("data", "preprocessed", "video_means_uni.rds"))
saveRDS(video_means_uni, here("data", "preprocessed", "video_means_bi.rds"))

rm(boxplot_bi, boxplot_uni, data, mean_data, mean_data_uni, mean_data_bi, plot_data, video_means, video_means_uni, video_means_bi)
