# merging simulation v1 

library(tidyverse)
library(patchwork)
library(ggridges)
library(viridis)


# fix colors

path <- "C:/Users/harra.AD/OneDrive - UW-Madison/Documents/PVs/cthc/sim res BMA and BART"
files <- list.files(path = path)
setwd(path)
all <- lapply(files, function(x) mget(load(x)))

# # Bind all results together
# all_results <- map_dfr(all, ~ .x$mean_bart)
# 
# # Summarize across parameter combinations
# summary_results <- all_results %>%
#   group_by(k, ntree, power, base) %>%
#   summarise(
#     mean_of_means = mean(mean_value, na.rm = TRUE),
#     sd_of_means   = sd(mean_value, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# summary_results


# BART --------------------------------------------------------------------


all_bart <- map_dfr(seq_along(all), function(i) {
  all[[i]]$mean_bart %>%
    full_join(all[[i]]$sd_bart,  by = c("PV", "k", "ntree", "power", "base")) %>%
    full_join(all[[i]]$kld_bart, by = c("PV", "k", "ntree", "power", "base")) %>%
    mutate(run_id = i)
})


all_bart <- all_bart %>%
  mutate(condition = paste0("k=", k, ", ntree=", ntree, 
                            ", power=", power, ", base=", base))

(bart_mean <-ggplot(all_bart, aes(x = factor(power), y = mean_value, fill = factor(k))) +
    geom_boxplot(outlier.size = .5) +
    geom_hline(yintercept = 500, color = "black", linetype = "dashed") + 
    facet_grid(base ~ ntree, labeller = label_both) +
    scale_fill_viridis(discrete = T, end = .5, begin = .2) + 
    labs(y = "Mean", fill = "k", x = "power",
         title = "BART PV Distribution Mean") +
    theme_minimal(base_size = 12)) +
  theme(legend.position = "none")

#ggsave("C:/Users/harra.AD/OneDrive - UW-Madison/Documents/PVs/BART_conditions.png", bg = "white", width = 20, height = 20)



(bart_sd <-ggplot(all_bart, aes(x = factor(power), y = sd_value, fill = factor(k))) +
    geom_boxplot(outlier.size = .5) +
    geom_hline(yintercept = 100, color = "black", linetype = "dashed") + 
    facet_grid(base ~ ntree, labeller = label_both) +
    scale_fill_viridis(discrete = T, end = .5, begin = .2) + 
    labs(y = "Standard Deviation", fill = "k", x = "power",
         title = "BART PV Distribution SD") +
    theme_minimal(base_size = 12)) +
  theme(legend.position = "bottom")

((bart_mean  + theme(legend.position = "right")) / (bart_sd + theme(legend.position = "right"))) 

ggsave("C:/Users/harra.AD/OneDrive - UW-Madison/Documents/PVs/BART_meanSD.png", bg = "white", width = 10, height = 8)

(bart_kld <-ggplot(all_bart, aes(x = factor(power), y = kld, fill = factor(k))) +
    geom_boxplot(outlier.size = .5) +
    facet_grid(base ~ ntree, labeller = label_both) +
    scale_fill_viridis(discrete = T, end = .5, begin = .2) + 
    labs(y = "KLD", fill = "k", x = "power",
         title = "BART KLD of PV Distribution") +
    theme_minimal(base_size = 12))

ggsave("C:/Users/harra.AD/OneDrive - UW-Madison/Documents/PVs/BART_kld.png", bg = "white", width =  8, height = 6)


# BMA ---------------------------------------------------------------------


all_BMA <- map_dfr(seq_along(all), function(i) {
  all[[i]]$mean_BMA %>%
    full_join(all[[i]]$sd_BMA,  by = c("PV", "gprior", "mprior")) %>%
    full_join(all[[i]]$kld_BMA, by = c("PV", "gprior", "mprior")) %>%
    mutate(run_id = i)
})


all_BMA <- all_BMA %>%
  mutate(condition = paste0("gprior=", gprior, ", mprior=", mprior))


(BMA_mean1 <- ggplot(all_BMA, aes(x = gprior, y = mean_value, fill = factor(mprior))) +
    geom_boxplot(outlier.size = .5) +
    geom_hline(yintercept = 500, color = "black", linetype = "dashed") + 
    labs(x = "gprior", y = "Mean",
         title = "BMA PV Distribution Mean", fill = "mprior") +
    scale_fill_viridis(discrete = T, end = .7, begin = .1) + 
    theme_minimal(base_size = 12) +
    theme(legend.position = "right",
          axis.text.x = element_text(angle = 45, vjust = 0.5)))

(BMA_sd1 <- ggplot(all_BMA, aes(x = gprior, y = sd_value, fill = factor(mprior))) +
    geom_boxplot(outlier.size = .5) +
    geom_hline(yintercept = 100, color = "black", linetype = "dashed") + 
    labs(x = "gprior", y = "Standard Deviation",
         title = "BMA PV Distribution SD", fill = "mprior") +
    scale_fill_viridis(discrete = T, end = .7, begin = .1) + 
    theme_minimal(base_size = 12) +
    theme(legend.position = "right",
          axis.text.x = element_text(angle = 45, vjust = 0.5)))

((BMA_mean1 + theme(legend.position = "none")) / (BMA_sd1 )) & plot_layout(guides = "collect")

ggsave("C:/Users/harra.AD/OneDrive - UW-Madison/Documents/PVs/BMA_meanSD.png", bg = "white", width = 10, height = 8)

# (BMA_mean2 <- ggplot(all_BMA, aes(x = factor(gprior), y = mean_value, fill = factor(mprior))) +
#     geom_boxplot(alpha = 0.7) +
#     geom_hline(yintercept = 500, color = "black", linetype = "dashed") + 
#     facet_wrap(~ mprior, scales = "fixed") +   # one plot per gprior
#     labs(x = "gprior", y = "Mean of PVs",
#          title = "BMA Mean by Parameter Combination") +
#     scale_fill_viridis(discrete = TRUE, end = 0.7, begin = 0.1) +
#     theme_minimal(base_size = 12) +
#     theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
#           legend.position = "none",
#           strip.text = element_text(face = "bold", size = 10)))


#ggsave("C:/Users/harra.AD/OneDrive - UW-Madison/Documents/PVs/BMA_conditions.png", bg = "white", width = 20, height = 20)



(BMA_kld1 <- ggplot(all_BMA, aes(x = gprior, y = kld, fill = factor(mprior))) +
    geom_boxplot(outlier.size = .5) +
    labs(x = "gprior", y = "KLD",
         title = "BMA KLD of PV Distribution", fill = "mprior") +
    scale_fill_viridis(discrete = T, end = .7, begin = .1) + 
    theme_minimal(base_size = 12) +
    theme(legend.position = "right",
          axis.text.x = element_text(angle = 45, vjust = 0.5)))

ggsave("C:/Users/harra.AD/OneDrive - UW-Madison/Documents/PVs/BMA_kld.png", bg = "white", width = 7, height = 5)



# all_results <- map_dfr(seq_along(all), function(i) {
#   all[[i]]$mean_BMA %>% mutate(run_id = i, mean_true = all[[i]]$mean_true)  })
# 
# all_results <- all_results %>%
#   mutate(condition = paste0("grprior=", gprior, ", mprior=", mprior))
# 
# ggplot(all_results, aes(x = mean_value, color = PV, group = PV)) +
#   geom_density() +
#   geom_density(aes(x = mean_true), color = "black", linetype = "dashed") +
#   facet_wrap(~ condition, scales = "free_y", ncol = 3) +
#   labs(x = "Mean Value", y = "Count",
#        title = "PV Means by BMA Parameter Combination") +
#   theme_minimal(base_size = 12) + 
#   theme(legend.position = "none")
# 
# ggsave("C:/Users/harra.AD/OneDrive - UW-Madison/Documents/PVs/BMA_conditions.png", 
#        bg = "white", width = 20, height = 20)






(bma_kld1 <- ggplot(all_bma, aes(x = gprior, y = kld, fill = factor(mprior))) +
  geom_boxplot(alpha = 0.7) +
  labs(x = "gprior", y = "KLD",
       title = "BMA KLD by Parameter Combination") +
  scale_fill_viridis(discrete = T, end = .7, begin = .1) + 
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5)))

(bma_kld2 <- ggplot(all_bma, aes(x = factor(gprior), y = kld, fill = factor(mprior))) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ mprior, scales = "fixed") +   # one plot per gprior
  labs(x = "gprior", y = "KLD",
       title = "BMA KLD by Parameter Combination") +
  scale_fill_viridis(discrete = TRUE, end = 0.7, begin = 0.1) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = "none",
        strip.text = element_text(face = "bold", size = 10)))

bma_mean1 / bma_mean2 | bart_mean

ggsave("C:/Users/harra.AD/OneDrive - UW-Madison/Documents/PVs/mean_both.png", 
       bg = "white", width = 10, height = 7)

# ggsave("C:/Users/harra.AD/OneDrive - UW-Madison/Documents/PVs/BMA_KLD.png", 
#        bg = "white", width = 17, height = 17)

# OG Method ---------------------------------------------------------------



all_results <- map_dfr(seq_along(all), function(i) {
  all[[i]]$mean_OG %>% mutate(run_id = i, mean_true = all[[i]]$mean_true)  })


OG_mean <- ggplot(all_results, aes(x = mean_value, color = PV, group = PV)) +
  geom_density() +
  geom_density(aes(x = mean_true), color = "black", linetype = "dashed") +
  #facet_wrap(~ condition, scales = "free_y", ncol = 3) +
  labs(x = "Mean Value", y = "Count",
       title = "PV Means for Replication") +
  theme_minimal(base_size = 12) + 
  theme(legend.position = "none")




all_results <- map_dfr(seq_along(all), function(i) {
  all[[i]]$sd_OG %>% mutate(run_id = i, sd_true = all[[i]]$sd_true) })

OG_sd <- ggplot(all_results, aes(x = sd_value, color = PV, group = PV)) +
  geom_density() +
  geom_vline(xintercept = 100, color = "black", linetype = "dashed") + 
  #facet_wrap(~ condition, scales = "free_y", ncol = 3) +
  labs(x = "Mean Value", y = "Count",
       title = "Histograms of PV SDs by BMA Parameter Combination",
       subtitle = "Note: simulation error, didn't save true SD, should be ~ 100 ") +
  theme_minimal(base_size = 12) + 
  theme(legend.position = "none")

OG_mean / OG_sd

ggsave("C:/Users/harra.AD/OneDrive - UW-Madison/Documents/PVs/OG_combined.png", 
       bg = "white", width = 20, height = 22)



# Create a combined dataset
all_og_box <- all_og %>%
  mutate(gprior = "OG", 
         mprior = "OG", 
         param_combo = "OG")  # placeholder for x-axis

all_bma_combined <- all_bma %>%
  mutate(param_combo = paste0("gprior=", gprior, ", mprior=", mprior))

# Combine BMA + OG
plot_data <- bind_rows(
  all_bma_combined %>% select(param_combo, kld, mprior),
  all_og_box %>% select(param_combo, kld, mprior)
)

# Make x-axis factor so OG is at the end
plot_data <- plot_data %>%
  mutate(param_combo = factor(param_combo, levels = c(
    unique(all_bma_combined$param_combo), "OG"
  )))

# Plot
ggplot(plot_data, aes(x = param_combo, y = kld, fill = factor(mprior))) +
  geom_boxplot(alpha = 0.7) +
  labs(x = "Parameter Combination", y = "KLD",
       title = "BMA KLD Distributions with OG at the End") +
  scale_fill_viridis(discrete = TRUE, end = 0.7, begin = 0.1) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        strip.text = element_text(face = "bold", size = 10))
