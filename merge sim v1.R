# merging simulation v1 

library(tidyverse)
library(patchwork)
library(ggridges)
library(viridis)


# fix colors

path <- "C:/Users/harra.AD/OneDrive - UW-Madison/Documents/PVs/cthc/sim results v2 1000 10pvs"
files <- list.files(path = path)
setwd(path)
all <- lapply(files, function(x) mget(load(x)))

# mean and sd of PV1 compared to true score

pv_all <- map(all, "mean_pv") %>%
  bind_rows(.id = "replication")

(pv_summary <- pv_all %>%
  pivot_longer(
    cols = ends_with("_mean"),
    names_to = "Model",
    values_to = "Mean"
  ) %>%
  mutate(Model = sub("_mean", "", Model)) %>%  
  group_by(Model, PV) %>%
  summarise(
    mean_of_means = mean(Mean, na.rm = TRUE),
    true_mean = mean(pv_all$true_mean),
    bias_perc = (mean_of_means - true_mean) / true_mean * 100,
    .groups = "drop"
  ) %>% arrange(desc(abs(bias_perc))))




pv_all_sd <- map(all, "sd_pv") %>%
  bind_rows(.id = "replication")

(pv_summary_sd <- pv_all_sd %>%
    pivot_longer(
      cols = ends_with("sd"),
      names_to = "Model",
      values_to = "SD"
    ) %>%
    mutate(Model = sub("sd", "", Model)) %>%  
    group_by(Model, PV) %>%
    summarise(
      mean_of_means = mean(SD, na.rm = TRUE),
      true_mean = mean(pv_all_sd$true_sd),
      bias_perc = (mean_of_means - true_mean) / true_mean * 100,
      .groups = "drop"
    ) %>% arrange(desc(abs(bias_perc))))




pv_long <- pv_all %>%
  pivot_longer(
    cols = c(BMA_mean, BART_mean, Default_mean, Ridge_mean, Horse_mean, OG_mean, true_mean),
    names_to = "Model",
    values_to = "Score"
  ) %>%
  mutate(Model = recode(Model,
                        BMA_mean     = "BMA",
                        BART_mean    = "BART",
                        Default_mean = "Default",
                        Ridge_mean   = "Ridge",
                        Horse_mean   = "Reg Horse",
                        OG_mean      = "Replication",
                        true_mean    = "True Score"),
         Model = factor(Model, levels = c("True Score", "Replication", 
                                          "BMA", "BART", "Default", 
                                          "Ridge", "Reg Horse"))) %>%
mutate(PV = factor(PV, levels = paste0("PV", 1:10)))


# distributions overlayed, faceted by PV 
ggplot(pv_long, aes(x = Score, color = Model)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~PV, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution of PVs by Model")

# boxplots faceted by PV 
ggplot(pv_long, aes(x = Model, y = Score, fill = Model)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.7) +
  facet_wrap(~PV, scales = "free_y") +
  scale_color_viridis(discrete = T, direction = -1) +
  scale_fill_viridis(discrete = T, direction = -1) + 
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Distribution of PVs by Model",
    x = "Model",
    y = "Plausible Value Mean"
  )

# overall distribution via boxplot - this one is slow
(box_mean <-ggplot(pv_long, aes(x = Model, y = Score, fill = Model)) +
  geom_boxplot(outlier.size = 0.5) +
  #geom_jitter(width = 0.2, alpha = 0.4) +
    scale_color_viridis(discrete = T, direction = -1) +
    scale_fill_viridis(discrete = T, direction = -1) + 
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(
    title = "Distribution of PV Means by Model",
    x = "Model",
    y = "Mean"
  ))



# ridge plot facted by PV 
ggplot(pv_long, aes(x = Score, y = Model, fill = Model)) +
  geom_density_ridges(alpha = 0.7, scale = 1.2, rel_min_height = 0.01) +
  facet_wrap(~PV, scales = "free_y") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Distribution of PVs by Model",
    x = "Plausible Value Mean",
    y = "Model"
  ) +
  theme(legend.position = "none")

#ggsave("C:/Users/harra.AD/OneDrive - UW-Madison/Documents/PVs/ridges_10pv_sd.png", bg = "white", width = 20, height = 15)


# ridge plot combined
(ridge_mean <- ggplot(pv_long, aes(x = Score, y = Model, fill = Model)) +
  geom_density_ridges(alpha = 0.7, scale = 1.2, rel_min_height = 0.01) +
  scale_fill_viridis(discrete = T, direction = -1) + 
  theme_minimal(base_size = 12) +
  labs(
    title = "Distribution of PV Means by Model",
    x = "Mean",
    y = "Model"
  ) +
  theme(legend.position = "none"))

ggsave("C:/Users/harra.AD/OneDrive - UW-Madison/Documents/PVs/PV_ridge_mean.png", 
       bg = "white", height= 8, width = 8)



pv_long_sd <- pv_all_sd %>%
  pivot_longer(
    cols = c(BMA_sd, BART_sd, Default_sd, Ridge_sd, Horse_sd, OG_sd, true_sd),
    names_to = "Model",
    values_to = "Score"
  ) %>%
  mutate(Model = recode(Model,
                        BMA_sd     = "BMA",
                        BART_sd    = "BART",
                        Default_sd = "Default",
                        Ridge_sd   = "Ridge",
                        Horse_sd   = "Reg Horse",
                        OG_sd      = "Replication",
                        true_sd    = "True Score"),
         Model = factor(Model, levels = c("True Score", "Replication", 
                                          "BMA", "BART", "Default", 
                                          "Ridge", "Reg Horse"))) %>%
  mutate(PV = factor(PV, levels = paste0("PV", 1:10)))



# distributions overlayed, faceted by PV 
ggplot(pv_long_sd, aes(x = Score, color = Model)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~PV, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution of PVs SD by Model")

# boxplots faceted by PV 
ggplot(pv_long_sd, aes(x = Model, y = Score, fill = Model)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.7) +
  facet_wrap(~PV, scales = "free_y") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Distribution of PVs SD by Model",
    x = "Model",
    y = "Plausible Value SD"
  )

# overall distribution via boxplot - this one is slow 
(box_sd <- ggplot(pv_long_sd, aes(x = Model, y = Score, fill = Model)) +
  geom_boxplot(outlier.size = 0.5) +
  #geom_jitter(width = 0.2, alpha = 0.4) +
    scale_color_viridis(discrete = T, direction = -1) +
    scale_fill_viridis(discrete = T, direction = -1) + 
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(
    title = "Distribution of PV SD by Model",
    x = "Model",
    y = "Standard Deviation" 
  ))


box_mean  / box_sd 

ggsave("C:/Users/harra.AD/OneDrive - UW-Madison/Documents/PVs/PV_mean_sd_box.png", 
       bg = "white", height = 8, width = 8)

# ridge plot facted by PV 
ggplot(pv_long_sd, aes(x = Score, y = Model, fill = Model)) +
  geom_density_ridges(alpha = 0.7, scale = 1.2, rel_min_height = 0.01) +
  facet_wrap(~PV, scales = "free_y") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Distribution of PVs SD by Model",
    x = "Plausible Value SD",
    y = "Model"
  ) +
  theme(legend.position = "none")

#ggsave("C:/Users/harra.AD/OneDrive - UW-Madison/Documents/PVs/ridges_10pv_sd.png", bg = "white", width = 20, height = 15)


# ridge plot combined
(ridge_sd <- ggplot(pv_long_sd, aes(x = Score, y = Model, fill = Model)) +
  geom_density_ridges(alpha = 0.7, scale = 1.2, rel_min_height = 0.01) +
  theme_minimal(base_size = 12) +
  scale_fill_viridis(discrete = T, direction = -1) + 
  labs(
    title = "Distribution of PV SD by Model",
    x = "Standard Deviation",
    y = "Model"
  ) +
  theme(legend.position = "none"))

ridge_mean | ridge_sd

ggsave("C:/Users/harra.AD/OneDrive - UW-Madison/Documents/PVs/PV_mean_sd_box.png", 
       bg = "white", height = 8, width = 10)


(box_mean + theme(axis.title.x = element_blank(), 
                  axis.ticks.x = element_blank(),
                  axis.text.x = element_blank(),
                  legend.position = "none")) /
(box_sd + theme(legend.position = "none")) 
+ plot_layout(guides = "collect")

ggsave("C:/Users/harra.AD/OneDrive - UW-Madison/Documents/PVs/v2_meanSD_agg.png", bg = "white", width = 10, height = 7)


# kld 

kld_all <- map(all, "kld_df") %>%
  bind_rows(.id = "replication")

# pivot longer
kld_long <- kld_all %>%
  pivot_longer(
    cols = c(BMA_KLD, BART_KLD, Default_KLD, Ridge_KLD, Horse_KLD, OG_KLD),
    names_to = "Model",
    values_to = "KLD"
  ) %>%
  mutate(Model = recode(Model,
                        BMA_KLD     = "BMA",
                        BART_KLD    = "BART",
                        Default_KLD = "Default",
                        Ridge_KLD   = "Ridge",
                        Horse_KLD   = "Reg Horse",
                        OG_KLD      = "Replication"),
         Model = factor(Model, 
                        levels = c("Replication", "BMA", "BART", 
                                   "Default", "Ridge", "Reg Horse")),
         PV = factor(PV, levels = paste0("PV", 1:10)))  # fix PV ordering

# Boxplots across all PVs (combined)
(kld <- ggplot(kld_long, aes(x = Model, y = KLD, fill = Model)) +
  geom_boxplot(outlier.size = 0.5) +
  #geom_jitter(width = 0.1, alpha = 0.4) +
  #scale_color_viridis(discrete = T, direction = -1, end = .8) +
  scale_fill_viridis(discrete = T, direction = -1, end = .8) + 
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(
    title = "KLD by PV Distribution",
    x = "Model", y = "KLD"
  ))

ggsave("C:/Users/harra.AD/OneDrive - UW-Madison/Documents/PVs/kld_10pv_combined.png", bg = "white", width = 10, height = 8)


# Faceted boxplots by PV
ggplot(kld_long, aes(x = Model, y = KLD, fill = Model, color = Model)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.4) +
  facet_wrap(~PV, scales = "free_y") +
  scale_color_viridis(discrete = T, end = .8, direction = -1) +
  scale_fill_viridis(discrete = T, end = .8, direction = -1) + 
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Distribution of KLDs by Model",
    x = "Model", y = "KLD")

ggsave("C:/Users/harra.AD/OneDrive - UW-Madison/Documents/PVs/kld_10pv_combined.png", bg = "white", width = 20, height = 15)



# loo of secondary analyses 

loo_all <- map2_dfr(all, seq_along(all), function(replication_obj, rep_id) {
  loo_list <- replication_obj$loo_list
  
  map_dfr(seq_along(loo_list), function(i) {
    loo_obj <- loo_list[[i]]
    
    model_name <- if(!is.null(loo_obj$model_name)) loo_obj$model_name else paste0("Model_", i)
    
    tibble(
      model    = model_name,
      elpd_loo = loo_obj$estimates["elpd_loo","Estimate"],
      p_loo    = loo_obj$estimates["p_loo","Estimate"],
      looic    = loo_obj$estimates["looic","Estimate"]) }) %>% mutate(replication = rep_id)})

(loo_summary <- loo_all %>%
  group_by(model) %>%
  summarise(
    across(where(is.numeric),
           list(mean = ~mean(.x, na.rm = TRUE),
                sd   = ~sd(.x, na.rm = TRUE))), .groups = "drop"))

loo_summary_df <- data.frame(loo_ic = loo_summary$looic_mean, 
                             model = c("BART", "BMA", "Reg. Horse", "Ridge", "Default", "Replication", "True Score"))

loo_plot <- loo_all %>%
  mutate(model = recode(model,
                        "Model_1" = "BART",
                        "Model_2" = "BMA",
                        "Model_3" = "Horseshoe",
                        "Model_4" = "Ridge",
                        "Model_5" = "Default",
                        "Model_6" = "Replication", 
                        "Model_7" = "True Score")) %>%
  mutate(model = factor(model, levels =  c("True Score", "Replication", "BMA", "BART", "Default", "Ridge", "Horseshoe")))

ggplot(loo_plot, aes(x = model, y = looic,  fill = model)) +
  geom_boxplot(outlier.shape = NA) +
  #geom_jitter(width = 0.2, alpha = 0.5) +
  #scale_color_viridis(discrete = T, direction = -1) +
  scale_fill_viridis(discrete = T, direction = -1) + 
  labs(x = "Model", y = "LOO-IC") +
  ggtitle("LOO-IC for secondary analysis") + 
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("C:/Users/harra.AD/OneDrive - UW-Madison/Documents/PVs/v2_loo_10pv.png", bg = "white", width = 10, height = 7)
