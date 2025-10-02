## Kjorte Harra
## BMA and BART parameter simulations v1

library(lsasim, quietly = T) 
library(TAM, quietly = T)
library(brms, quietly = T)
library(BMS, quietly = T)
library(dplyr, quietly = T)
library(tidyverse, quietly = T)
library(BART, quietly = T)
library(LaplacesDemon, quietly = T )


args <- commandArgs(trailingOnly = TRUE)
extract_int_arg <- function(arg_string, arg_name, default) {
  res <- 
    arg_string %>%
    stringr::str_extract(paste(arg_name, "[0-9]+")) %>%
    stringr::str_sub(stringr::str_length(arg_name) + 2, -1) %>%
    as.integer()
  if (is.na(res)) res <- default
  res}

arg_string <- paste(args, collapse = " ")
rep <- extract_int_arg(arg_string, "-rep", 2L)

if(length(args)==0){ print("No arguments supplied.") }

arguments = commandArgs(trailingOnly=TRUE)

set.seed(05022000)
seed.number <- round(runif(1000,10000000, 99999999),0)
my.seed <- seed.number[rep]
set.seed(my.seed)

n_pv <- 10

sim_params_bma <- expand.grid(
  gprior = c("UIP", "RIC", "HQ", "EBL"), 
  #"hyper=3", "hyper=4", "hyper=UIC", "hyper=RIC", "hyper=BRIC"),
  mprior = c("uniform", "fixed", "random"),
  stringsAsFactors = F)


sim_params_bart <- expand.grid(
  ntree = c(50, 200, 500), 
  k = c(1, 3),
  power = c(0.5, 1, 3),
  base = c(0.5, 0.9))



# Background Questionnaire  ---------------------------------------------------------------------

set.seed(my.seed)
bg_data <- questionnaire_gen( n_obs = 5000,
                              n_vars = 500, 
                              theta = T, verbose = F, full_output = T, 
                              c_mean = 0, c_sd = 1, 
                              cor_matrix = cor_gen(500))

# Item Parameter Generation ---------------------------------------------------------------------

set.seed(my.seed)
item_data <- item_gen(n_2pl = 300, 
                      a_bounds = c(0.5, 3), b_bounds = c(-2.5, 2.5)) 

# Item Response Generation  ---------------------------------------------------------------------

set.seed(my.seed)
blocks <- block_design(n_blocks = 20, item_parameters = item_data)
set.seed(my.seed)
booklets <- booklet_design(item_block_assignment = blocks$block_assignment)
set.seed(my.seed)
book_admin <- booklet_sample(5000, book_item_design = booklets, )

set.seed(my.seed)
responses <- response_gen(subject = book_admin$subject, 
                          item = book_admin$item, theta = bg_data$bg$theta, 
                          a_par = item_data$a, b_par = item_data$b)

# PISA Replication  ---------------------------------------------------------------------

bg_numeric <- data.frame(lapply(bg_data$bg[,3:ncol(bg_data$bg)], function(x) {
  if (is.factor(x) || is.character(x)) { as.numeric(as.factor(x))} 
  else { x } }))

pca <- prcomp(bg_numeric)
pca_df <- as.data.frame(pca$x[, 1:which(cumsum(summary(pca)$importance[2, ]) >= 0.80)[1]])
resp_items <- responses[, 1:ncol(responses)]
resp_items <- as.data.frame(lapply(resp_items, function(x) as.numeric(as.character(x))))
(bad_items <- names(which(apply(resp_items, 2, function(x) max(x, na.rm = TRUE) == 0))))
responses_clean <- resp_items[, !(names(resp_items) %in% bad_items)]
irt <- tam.mml.2pl(resp = responses_clean[,1:ncol(responses_clean)-1], Y = pca_df) 

pvs <- tam.pv(irt, normal.approx = F, np.adj = 1)
pv_scores <- (pvs$pv[,2:11] * 100) + 500
pv_df <- data.frame(pv_scores)


# BMA ---------------------------------------------------------------------

irt1 <- tam.mml.2pl(resp = responses[,1:ncol(responses) - 1])
bg_numeric$theta_estimates <- irt1$person$EAP

pv_results <- list()
for (i in seq_len(nrow(sim_params_bma))){
  
  message("Running BMA with g = ", sim_params_bma$gprior[i], ", mprior = ", sim_params_bma$mprior[i])
  
  bma_fit <- bms(bg_numeric[,c(500, 1:499)], 
                 g = sim_params_bma$gprior[i],
                 mprior = sim_params_bma$mprior[i], 
                 user.int = F)
  
  pdens <- pred.density(bma_fit, newdata = bg_numeric[,1:499])
  dens_list <- pdens$densities()
  pv_matrix_bma<- replicate(10, {
    vapply(seq_along(dens_list), function(j) {
      d <- dens_list[[j]]
      probs <- d$y / sum(d$y)
      sample(d$x, size = 1, prob = probs)
    }, numeric(1))
  })
  
  pv_matrix_bma <- pv_matrix_bma * 100 + 500
  colnames(pv_matrix_bma) <- paste0("PV", 1:n_pv)
  pv_results[[paste0("g_", sim_params_bma$gprior[i], "_m_", sim_params_bma$mprior[i])]] <- pv_matrix_bma
  
}

pv_long_bma <- map2_dfr(
  pv_results,
  seq_along(pv_results),
  ~ as.data.frame(.x) %>%
    mutate(obs = row_number()) %>%          
    pivot_longer(
      cols = -obs,                          
      names_to = "PV", 
      values_to = "value"
    ) %>%
    mutate(
      gprior = sim_params_bma$gprior[.y],
      mprior  = sim_params_bma$mprior[.y]
    )
)


# BART ---------------------------------------------------------------------


K <- 10
n_total <- nrow(bg_numeric)
pv_results <- vector("list", nrow(sim_params_bart))
folds <- sample(rep(1:K, length.out = n_total))


for (p in seq_len(nrow(sim_params_bart))) {
  
  message("Running BART with ntree = ", sim_params_bart$ntree[p], ", k = ", sim_params_bart$k[p],
          " power = ", sim_params_bart$power[p], ", base = ", sim_params_bart$base[p])

  pv_matrix_bart <- matrix(NA, nrow = n_total, ncol = n_pv)
  
  for (fold in 1:K) {
    
    train_idx <- which(folds != fold)
    test_idx  <- which(folds == fold)
    
    x_train <- as.matrix(bg_numeric[train_idx, 1:499])
    y_train <- bg_numeric$theta_estimates[train_idx]
    
    x_test  <- as.matrix(bg_numeric[test_idx, 1:499])
    
    # Fit BART
    fit <- wbart(
      x.train = x_train,
      y.train = y_train,
      ntree = sim_params_bart$ntree[p],
      k = sim_params_bart$k[p],
      power = sim_params_bart$power[p], 
      base = sim_params_bart$base[p],
      ndpost = 1000,
      nskip = 100,
      #sparse = TRUE, 
    )
    
    preds <- predict(fit, newdata = x_test)
    
    for (d in 1:n_pv) {
      draw_ids <- sample(1:nrow(preds), size = length(test_idx), replace = TRUE)
      pv_matrix_bart[test_idx, d] <- preds[cbind(draw_ids, 1:length(test_idx))] * 100 + 500
    }
  }
    colnames(pv_matrix_bart) <- paste0("PV", 1:n_pv)
  pv_results[[p]] <- pv_matrix_bart
}


pv_long_bart <- map2_dfr(
  pv_results,
  seq_along(pv_results),
  ~ as.data.frame(.x) %>%
    mutate(obs = row_number()) %>%          
    pivot_longer(
      cols = -obs,                          
      names_to = "PV", 
      values_to = "value"
    ) %>%
    mutate(
      k     = sim_params_bart$k[.y],
      ntree = sim_params_bart$ntree[.y],
      power = sim_params_bart$power[.y],
      base  = sim_params_bart$base[.y]
    )
) %>%
  arrange(PV, obs)   


# Comparisons ---------------------------------------------------------------------

# replication

bg_numeric$true_score <- bg_data$bg$theta * 100 + 500

pv_df <- pv_df %>% mutate(obs = row_number())
pv_long_og <- pv_df %>%
  pivot_longer(
    cols = starts_with("PV"),
    names_to = "PV",
    names_pattern = "(PV\\d+)\\.Dim1",  
    values_to = "OG_PV"
  ) %>%
  arrange(PV, obs)  

mean_OG <- pv_long_og %>%
  group_by(PV) %>%
  summarise(mean_value = mean(OG_PV), .groups = "drop")

sd_OG <- pv_long_og %>%
  group_by(PV) %>%
  summarise(sd_value = sd(OG_PV), .groups = "drop")

kld_OG <- pv_long_og %>%
  group_by(PV) %>%
  summarise(kld = KLD(OG_PV, bg_numeric$true_score)$sum.KLD.px.py)


# 
# bg_numeric <- bg_numeric %>% 
#   mutate(obs = row_number(),
#          true_score = bg_data$bg$theta * 100 + 500) 
# 
# bg_numeric_10 <- bind_rows(replicate(10, bg_numeric, simplify = FALSE))
# 
# 
# (combined_pv <- pv_long_og %>%
#     full_join(pv_long_bma,  by = c("obs", "PV")) %>%
#     full_join(pv_long_bart, by = c("obs", "PV")) %>%
#     bind_cols(bg_numeric_10 %>% select(-obs)))


mean_BMA <- pv_long_bma %>%
  group_by(PV, gprior, mprior) %>%
  summarise(mean_value = mean(value), .groups = "drop")

sd_BMA <- pv_long_bma %>%
  group_by(PV, gprior, mprior) %>%
  summarise(sd_value = sd(value), .groups = "drop")

kld_BMA <- pv_long_bma %>%
  group_by(PV, gprior, mprior) %>%
  summarise(kld = KLD(value, bg_numeric$true_score)$sum.KLD.px.py)



mean_bart <- pv_long_bart %>%
  group_by(PV, k, ntree, power, base) %>%
  summarise(mean_value = mean(value), .groups = "drop")

sd_bart <- pv_long_bart %>%
  group_by(PV, k, ntree, power, base) %>%
  summarise(sd_value = sd(value), .groups = "drop")

kld_bart <- pv_long_bart %>%
  group_by(PV, k, ntree, power, base) %>%  
  summarise(kld = KLD(value, bg_numeric$true_score)$sum.KLD.px.py)



mean_true <- mean(bg_numeric$true_score)
sd_true <- sd(bg_numeric$true_score)





# fit_bart <- brm(BART_PV ~ q142 + q256 + q303 + q22 + q205 + q118 + q133 + q102 + q235, data = combined_pv)
# fit_bma <- brm(BMA_PV ~ q142 + q256 + q303 + q22 + q205 + q118 + q133 + q102 + q235, data = combined_pv)
# fit_og <- brm(OG_PV ~ q142 + q256 + q303 + q22 + q205 + q118 + q133 + q102 + q235, data = combined_pv)
# fit_true <- brm(true_score ~ q142 + q256 + q303 + q22 + q205 + q118 + q133 + q102 + q235, data = combined_pv)
# 
# loo_bart <- loo(fit_bart)
# loo_bma <- loo(fit_bma)
# loo_og <- loo(fit_og)
# loo_true <- loo(fit_true)
# 
# loo_list <- list(loo_bart, loo_bma, loo_og, loo_true)
# loo_compare_obj <- loo_compare(loo_bart, loo_bma, loo_og, loo_true)
# 
# coef_bart  <- as.data.frame(fixef(fit_bart)) %>% rownames_to_column("term") %>% mutate(model = "BART")
# coef_bma   <- as.data.frame(fixef(fit_bma))  %>% rownames_to_column("term") %>% mutate(model = "BMA")
# coef_og    <- as.data.frame(fixef(fit_og))   %>% rownames_to_column("term") %>% mutate(model = "Original PV")
# coef_true  <- as.data.frame(fixef(fit_true)) %>% rownames_to_column("term") %>% mutate(model = "True Score")
# 
# coef_all <- bind_rows(coef_bart, coef_bma, coef_og, coef_true)


# Save Results ---------------------------------------------------------------------

sesssion_info <- sessionInfo()
run_date <- date()

file_name <- paste("sim_BMA_BART_resv1_", rep, ".Rdata", sep = "")

save(my.seed, 
     mean_bart, mean_BMA, mean_OG, mean_true,
     sd_bart, sd_BMA, sd_OG, sd_true, 
     kld_bart, kld_BMA, kld_OG,
     file = file_name)
