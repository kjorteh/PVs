# merging simulation v1 

library(tidyverse)
library(patchwork)
library(ggridges)
library(viridis)


path <- "C:/Users/harra.AD/OneDrive - UW-Madison/Documents/PVs/cthc/sim res BMA and BART"
files <- list.files(path = path)
setwd(path)
all <- lapply(files, function(x) mget(load(x)))


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
