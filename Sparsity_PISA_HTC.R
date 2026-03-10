# HTC Sparsity PISA
## KJORTE HARRA
args <- commandArgs(trailingOnly = TRUE)
cnt <- args[1]

library(cmdstanr)
local_path <- list.dirs(getwd(), recursive = FALSE)
cmdstan_dir <- local_path[grep("cmdstan", local_path)][1]

if (!is.na(cmdstan_dir)) {
  set_cmdstan_path(cmdstan_dir)
  cat("CmdStan path set to:", cmdstan_path(), "\n")} else {
  stop("Could not find the cmdstan folder")}

library(brms)
library(dplyr)
library(tidyr)
library(TAM)
library(haven)
library(mice)
library(caret)
library(ggplot2)
library(posterior)
library(patchwork)

base_dir <- getwd()
load("pisa_testing_anchor_subset.RData")
rm(df_small, resp, resp_new)

process_country_pvs <- function(cnt, base_dir, plot = TRUE){
  
  tryCatch({
    
    message(sprintf("Processing %s", cnt))
    qqq_path <- paste0("data_CNT_", cnt, "_QQQ.sav")
    cog_path <- paste0("data_CNT_", cnt, "_COG.sav")
    
    df <- haven::read_sav(qqq_path)
    df_resp <- haven::read_sav(cog_path)
    
    # ----------------------------
    # Background preprocessing
    # ----------------------------
    
    df_background <- df[, -c(1:22, 1063:1278)]
    
    df_test <- df_background %>%
      mutate(across(where(is.labelled),
                    ~ if(is.numeric(.)) as.numeric(zap_labels(.))
                    else as.numeric(as_factor(.))))
    
    df_test[df_test == 9999999] <- NA
    df_test <- df_test %>% select(where(~ mean(is.na(.)) < .80))
    
    split_indices <- split(seq_len(ncol(df_test)),
                           cut(seq_len(ncol(df_test)), 5, labels = FALSE))
    
    df_blocks <- lapply(split_indices, function(cols) df_test[, cols])
    
    completed_blocks <- lapply(df_blocks, function(x) {complete(mice(x, method = "pmm", m = 1, print = FALSE), "long")})
    combined_completed <- do.call(cbind, completed_blocks) %>% select(-matches("^\\.imp$|-?\\.id$"))
    combined_completed_no_na <- combined_completed[, colSums(is.na(combined_completed)) == 0]
    
    rm(df_background, df_test, df_blocks, completed_blocks, combined_completed)
    gc()
    
    # ----------------------------
    # Response items
    # ----------------------------
    
    resp_df <- df_resp %>%
      select(matches("^CR.*S$") & !matches("VS$")) %>%
      select(where(~ !all(is.na(.))))
    
    resp <- resp_df %>%
      mutate(across(everything(), as.numeric)) %>%
      select(where(~ max(., na.rm = TRUE) > 0))
    
    # ----------------------------
    # Item mapping
    # ----------------------------
    
    orig_items <- irt$item$item
    new_items  <- colnames(resp)
    
    item_map <- match(orig_items, new_items)
    
    Bf <- irt$B.fixed.estimated
    Bf[,1] <- item_map[Bf[,1]]
    Bf <- Bf[!is.na(Bf[,1]), , drop = FALSE]
    
    Xf <- irt$xsi.fixed.estimated
    Xf[,1] <- item_map[Xf[,1]]
    Xf <- Xf[!is.na(Xf[,1]), , drop = FALSE]
    
    # ----------------------------
    # PCA
    # ----------------------------
    
    pca <- prcomp(combined_completed_no_na)
    ncomp <- which(cumsum(summary(pca)$importance[2,]) >= 0.90)[1]
    pca_df <- as.data.frame(pca$x[, 1:ncomp])
    
    rm(pca)
    gc()
    
    # ----------------------------
    # TAM model
    # ----------------------------
    
    irt_tam <- tam.mml.2pl(
      resp = resp,
      B.fixed = Bf,
      xsi.fixed = Xf,
      Y = pca_df,
      pweights = df$W_FSTUWT,
      pid = df$CNTSTUID,
      variance.fixed = matrix(c(1,1,1), ncol = 3)
    )
    
    pvs <- tam.pv(irt_tam, normal.approx = TRUE)
    
    pvs_df <- as.data.frame(pvs$pv)
    colnames(pvs_df) <- paste0("PV", seq_len(ncol(pvs_df)), "_TAM")
    
    irt_test <- tam.mml.2pl(
      resp = resp,
      B.fixed = Bf,
      xsi.fixed = Xf,
      pweights = df$W_FSTUWT,
      pid = df$CNTSTUID,
      variance.fixed = matrix(c(1,1,1), ncol = 3)
    )
    
    mu <- irt_test$person$EAP
    sd <- irt_test$person$SD.EAP
    theta_draw <- rnorm(length(mu), mu, sd)
    combined_completed_no_na$theta_estimates <- theta_draw
    
    # ----------------------------
    # Predictor matrix
    # ----------------------------
    
    bg_numeric_scaled <- combined_completed_no_na %>%
      select(-theta_estimates) %>%
      select(where(~ is.numeric(.) && sd(.) != 0)) %>%
      mutate(across(everything(), ~ as.numeric(scale(.))))
    
    if(ncol(bg_numeric_scaled) > 1){
      cor_mat <- cor(bg_numeric_scaled, use = "pairwise.complete.obs")
      high_corr <- which(abs(cor_mat) > .999, arr.ind = TRUE)
      high_corr <- high_corr[high_corr[,1] < high_corr[,2], , drop = FALSE]
      
      if(nrow(high_corr) > 0){
        drop_idx <- unique(high_corr[,2])
        bg_numeric_scaled <- bg_numeric_scaled[, -drop_idx, drop = FALSE] } }
    
    bg_numeric_scaled$theta_estimates <- theta_draw 
    colnames(bg_numeric_scaled) <- sub("^\\d+\\.", "", colnames(bg_numeric_scaled))
    
    # ----------------------------
    # Sparsity Models
    # ----------------------------
    
    fit_R2D2 <- brm(
      theta_estimates ~ .,
      data = bg_numeric_scaled,
      prior = prior(R2D2()),
      backend = "cmdstanr",
      threads = threading(1),
      control = list(adapt_delta = 0.99),
      chains = 4,
      iter = 4000,
      warmup = 2000,
      seed = 502
    )
    
    r2d2_rhat <- mean(summary(fit_R2D2)$fixed[,"Rhat"])
    
    pred <- posterior_predict(fit_R2D2, ndraws = 200)
    rand_rows <- sample(nrow(pred), 10)
    sparsity_pv_df <- data.frame((t(pred[rand_rows, ]) * 100) + 400)
    colnames(sparsity_pv_df)[1:10] <- paste0("PVREAD_R2D2", 1:10)
    
    ppc3 <- pp_check(fit_R2D2) +  ggtitle("PPC: R2D2 Prior")
    
    rm(fit_R2D2,pred)
    gc()
    
    # Regularized Horseshoe
    
    reghorse <- prior(horseshoe(par_ratio=.05,scale_slab=1,df_slab=4),class="b")
    
    fit_horse <- brm(
      theta_estimates ~ .,
      data = bg_numeric_scaled,
      prior = reghorse,
      backend = "cmdstanr",
      threads = threading(1),
      control = list(adapt_delta = 0.99),
      chains = 4,
      iter = 4000,
      warmup = 2000,
      seed = 502
    )
    
    horse_rhat <- mean(summary(fit_horse)$fixed[,"Rhat"])
    pred <- posterior_predict(fit_horse, ndraws = 200)
    rand_rows <- sample(nrow(pred), 10)
    sparsity_pv_df <- cbind(sparsity_pv_df, (t(pred[rand_rows,]) * 100) + 400)
    colnames(sparsity_pv_df)[11:20] <- paste0("PVREAD_REGHORSE", 11:20)
    
    ppc1 <- pp_check(fit_horse) + ggtitle("PPC: Reg. Horseshoe Prior")
    
    rm(fit_horse,pred)
    gc()
    
    # Ridge
    
    fit_ridge <- brm(
      theta_estimates ~ .,
      data = bg_numeric_scaled,
      prior = prior(normal(0,1)),
      backend = "cmdstanr",
      threads = threading(2),
      chains = 2,
      iter = 2000,
      warmup = 1000,
      seed = 502
    )
    
    ridge_rhat <- mean(summary(fit_ridge)$fixed[,"Rhat"])
    pred <- posterior_predict(fit_ridge, ndraws = 200)
    rand_rows <- sample(nrow(pred), 10)
    sparsity_pv_df <- cbind(sparsity_pv_df, (t(pred[rand_rows,]) * 100) + 400)
    colnames(sparsity_pv_df)[21:30] <- paste0("PVREAD_RIDGE", 21:30)
    sparsity_pv_df <- cbind(sparsity_pv_df, pvs_df)
    
    ppc2 <- pp_check(fit_ridge) + ggtitle("PPC: Ridge Prior")
    
    rm(fit_ridge,pred)
    gc()
    
    # ----------------------------
    # Save
    # ----------------------------
    
    
    sesssion_info <- sessionInfo()
    run_date <- date()
    
    file_name <- paste("pv_spars_pisa_", cnt, ".Rdata", sep = "")
    
    save(sparsity_pv_df, pvs_df, r2d2_rhat, ridge_rhat,horse_rhat,
         file = file_name)
    
    return(TRUE)
    
  }, error=function(e){
    
    message("FAILED: ",cnt," -> ",e$message)    
    return(FALSE)
    
  })
  
}

process_country_pvs(cnt, base_dir)