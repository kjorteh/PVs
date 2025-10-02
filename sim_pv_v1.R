## PV Simulation Pilot Test v1
#### Kjorte Harra

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

# Background Questionnaire  ---------------------------------------------------------------------

set.seed(my.seed)
bg_data <- questionnaire_gen( n_obs = 5000,
                              n_vars = 400, 
                              theta = T, verbose = F, full_output = T, 
                              c_mean = 0, c_sd = 1, 
                              cor_matrix = cor_gen(400))

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


#true_score <- bg_data$bg$theta * 100 + 500

# BMA ---------------------------------------------------------------------

irt1 <- tam.mml.2pl(resp = responses[,1:ncol(responses) - 1])
bg_numeric$theta_estimates <- irt1$person$EAP

set.seed(my.seed)
bma_fit <- bms(bg_numeric[,c(400, 1:399)], user.int = F)
pdens <- pred.density(bma_fit, newdata = bg_numeric[,1:ncol(bg_numeric)-1])
dens_list <- pdens$densities()
pv_matrix_bma<- replicate(10, {
  vapply(seq_along(dens_list), function(j) {
    d <- dens_list[[j]]
    probs <- d$y / sum(d$y)
    sample(d$x, size = 1, prob = probs)
  }, numeric(1))
})

pv_matrix_bma <- pv_matrix_bma * 100 + 500
colnames(pv_matrix_bma) <- paste0("BMA_PV", 1:n_pv)


# BART ---------------------------------------------------------------------

K <- 10
n_total <- nrow(bg_numeric)

folds <- sample(rep(1:K, length.out = n_total))
pv_matrix_bart <- matrix(NA, nrow = 5000, ncol = n_pv)

for (fold in 1:K) {
  
  set.seed(my.seed)
  
  train_idx <- which(folds != fold)
  test_idx  <- which(folds == fold)
  
  x_train <- as.matrix(bg_numeric[train_idx, 1:399])
  y_train <- bg_numeric$theta_estimates[train_idx]
  
  x_test  <- as.matrix(bg_numeric[test_idx, 1:399])
  
  fit <- wbart(
    x.train = x_train, y.train = y_train,
    ntree = 500, k = 1, ndpost = 1000,
    nskip = 100, sparse = TRUE, )
  
  preds <- predict(fit, newdata = x_test) 
  
  for (d in 1:n_pv) {
    draw_ids <- sample(1:nrow(preds), size = length(test_idx), replace = TRUE)
    pv_matrix_bart[test_idx, d] <- preds[cbind(draw_ids, 1:length(test_idx))] * 100 + 500
  }
}

colnames(pv_matrix_bart) <- paste0("BART_PV", 1:n_pv)

# Sparsity ---------------------------------------------------------------------

bg_numeric_scaled <- bg_numeric[,1:400] %>%
  mutate(across(where(is.numeric), ~ scale(.) %>% as.numeric()))

fit_default_pv <- brm(
  formula = theta_estimates ~ .,  
  data = bg_numeric_scaled,
  chains = 4, iter = 2000, warmup = 1000, seed = my.seed)

post <- t(posterior_predict(fit_default_pv))
rand_cols <- sample(ncol(post), 10)

sparsity_pv_df <- data.frame((post[, rand_cols] * 100) + 500)
colnames(sparsity_pv_df)[1:10] <- paste0("Default_PV", 1:10)


fit_ridge_pv <- brm(
  formula = theta_estimates ~ .,  
  data = bg_numeric_scaled,
  prior = prior(normal(0,1)), 
  chains = 4, iter = 2000, warmup = 1000, seed = my.seed)

post <- t(posterior_predict(fit_ridge_pv))
rand_cols <- sample(ncol(post), 10)
sparsity_pv_df <- cbind(sparsity_pv_df,(post[,rand_cols]*100) + 500)
colnames(sparsity_pv_df)[11:20] <- paste0("Ridge_PV", 1:10)

fit_horse_pv <- brm(
  formula = theta_estimates ~ .,  
  data = bg_numeric_scaled,
  prior = prior(horseshoe()), 
  chains = 4, iter = 2000, warmup = 1000, seed = my.seed)

post <- t(posterior_predict(fit_horse_pv))
sparsity_pv_df <- cbind(sparsity_pv_df,(post[,rand_cols]*100) + 500)
colnames(sparsity_pv_df)[21:30] <- paste0("Horse_PV", 1:10)


# Comparisons ---------------------------------------------------------------------

# replication

pv_df <- pv_df %>% mutate(obs = row_number())
pv_long_og <- pv_df %>%
  pivot_longer(
    cols = starts_with("PV"),
    names_to = "PV",
    names_pattern = "(PV\\d+)\\.Dim1",  
    values_to = "OG_PV"
  ) %>%
  arrange(PV, obs)  


pv_df_bma <- as.data.frame(pv_matrix_bma) %>%
  mutate(obs = row_number())

pv_long_bma <- pv_df_bma %>%
  pivot_longer(
    cols = starts_with("BMA"),            
    names_to = "PV",
    names_pattern = "V(\\d+)",          
    values_to = "BMA_PV"
  ) %>%
  mutate(PV = paste0("PV", PV))  %>%
  arrange(PV, obs)

pv_long_bart <- data.frame(pv_matrix_bart) %>%
  mutate(obs = row_number()) %>%
  pivot_longer(cols = starts_with("BART"),
               names_to = "PV",
               names_pattern = "V(\\d+)",  
               values_to = "BART_PV") %>%
  mutate(PV = paste0("PV", PV))  %>%
  arrange(PV, obs)


sparsity_pv_long <- sparsity_pv_df %>%
  mutate(obs = row_number()) %>%
  pivot_longer(
    cols = -obs,  
    names_to = c("Model", "PV"),
    names_pattern = "(.*)_PV(\\d+)",
    values_to = "Sparsity_PV"
  ) %>%
  mutate(PV = paste0("PV", PV)) %>%
  pivot_wider(
    names_from = Model,
    values_from = Sparsity_PV
  ) %>%
  arrange(PV, obs)


bg_numeric <- bg_numeric %>% 
  mutate(obs = row_number(),
         true_score = bg_data$bg$theta * 100 + 500) 

bg_numeric_10 <- bind_rows(replicate(10, bg_numeric, simplify = FALSE))


#pv_long_og, pv_long_bma, pv_long_bart, sparsity_pv_long, bg_numeric_10


# calculate bias here bc saving all this is way too much information

(combined_pv <- pv_long_og %>%
  full_join(pv_long_bma,  by = c("obs", "PV")) %>%
  full_join(pv_long_bart, by = c("obs", "PV")) %>%
  full_join(sparsity_pv_long, by = c("obs", "PV")) %>%
  bind_cols(bg_numeric_10 %>% select(-obs)))


mean_pv <- combined_pv %>%
  group_by(PV) %>%
  summarise(
    BMA_mean = mean(BMA_PV),
    BART_mean  = mean(BART_PV),
    Default_mean = mean(Default),
    Ridge_mean = mean(Ridge),
    Horse_mean = mean(Horse),
    OG_mean  = mean(OG_PV),
    true_mean = mean(true_score),
    .groups = "drop")

sd_pv <- combined_pv %>%
  group_by(PV) %>%
  summarise(
    BMA_sd = sd(BMA_PV),
    BART_sd  = sd(BART_PV),
    Default_sd = sd(Default),
    Ridge_sd = sd(Ridge),
    Horse_sd = sd(Horse),
    OG_sd  = sd(OG_PV),
    true_sd = sd(true_score),
    .groups = "drop")


kld_df <- combined_pv %>%
  group_by(PV) %>%
  summarise(
    BMA_KLD         = KLD(BMA_PV, true_score)$sum.KLD.px.py,
    BART_KLD        = KLD(BART_PV, true_score)$sum.KLD.px.py,
    Default_KLD     = KLD(Default, true_score)$sum.KLD.px.py,
    Ridge_KLD       = KLD(Ridge, true_score)$sum.KLD.px.py,
    Horse_KLD       = KLD(Horse, true_score)$sum.KLD.px.py,
    OG_KLD          = KLD(OG_PV, true_score)$sum.KLD.px.py,
    .groups = "drop")


fit_bart <- brm(BART_PV ~ q142 + q256 + q303 + q22 + q205 + q118 + q133 + q102 + q235, data = combined_pv)
fit_bma <- brm(BMA_PV ~ q142 + q256 + q303 + q22 + q205 + q118 + q133 + q102 + q235, data = combined_pv)
fit_ridge <- brm(Ridge ~ q142 + q256 + q303 + q22 + q205 + q118 + q133 + q102 + q235, data = combined_pv)
fit_horse <- brm(Horse ~ q142 + q256 + q303 + q22 + q205 + q118 + q133 + q102 + q235, data = combined_pv)
fit_default <- brm(Default ~ q142 + q256 + q303 + q22 + q205 + q118 + q133 + q102 + q235, data = combined_pv)
fit_og <- brm(OG_PV ~ q142 + q256 + q303 + q22 + q205 + q118 + q133 + q102 + q235, data = combined_pv)
fit_true <- brm(true_score ~ q142 + q256 + q303 + q22 + q205 + q118 + q133 + q102 + q235, data = combined_pv)

loo_bart <- loo(fit_bart)
loo_bma <- loo(fit_bma)
loo_horse <- loo(fit_horse)
loo_ridge <- loo(fit_ridge)
loo_default <- loo(fit_default)
loo_og <- loo(fit_og)
loo_true <- loo(fit_true)

loo_list <- list(loo_bart, loo_bma, loo_horse, loo_ridge, loo_default, loo_og, loo_true)
loo_compare_obj <- loo_compare(loo_bart, loo_bma, loo_horse, loo_ridge, loo_default, loo_og, loo_true)

coef_bart  <- as.data.frame(fixef(fit_bart)) %>% rownames_to_column("term") %>% mutate(model = "BART")
coef_bma   <- as.data.frame(fixef(fit_bma))  %>% rownames_to_column("term") %>% mutate(model = "BMA")
coef_horse   <- as.data.frame(fixef(fit_horse))  %>% rownames_to_column("term") %>% mutate(model = "Reg. Horse")
coef_ridge   <- as.data.frame(fixef(fit_ridge))  %>% rownames_to_column("term") %>% mutate(model = "Ridge")
coef_default  <- as.data.frame(fixef(fit_default))  %>% rownames_to_column("term") %>% mutate(model = "Direct Est")
coef_og    <- as.data.frame(fixef(fit_og))   %>% rownames_to_column("term") %>% mutate(model = "Original PV")
coef_true  <- as.data.frame(fixef(fit_true)) %>% rownames_to_column("term") %>% mutate(model = "True Score")

coef_all <- bind_rows(coef_bart, coef_bma, coef_horse, coef_ridge, coef_default ,coef_og, coef_true)

# add secondary analysis with PV as independent var? 

# Save Results ---------------------------------------------------------------------

sesssion_info <- sessionInfo()
run_date <- date()

file_name <- paste("sim_resv1_", rep, ".Rdata", sep = "")

save(my.seed, 
     mean_pv, sd_pv,
     #fit_default_pv, fit_ridge_pv, fit_horse_pv,
     kld_df, 
     loo_list, loo_compare_obj, coef_all,
     file = file_name)
