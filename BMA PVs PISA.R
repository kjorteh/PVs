# BMA 
## KJORTE HARRA

library(haven, quietly = T)
library(dplyr, quietly = T)
library(tidyverse, quietly = T)
library(TAM, quietly = T)
library(mice, quietly = T)
library(ggplot2, quietly = T)
library(patchwork, quietly = T)
library(future.apply, quietly = T)
library(BMS, quietly = T)
library(caret, quietly = T)


process_country_pvs <- function(cnt, base_dir, irt, plot = TRUE) {
  
  tryCatch({
    
    # ---------- File paths ----------
    qqq_path <- file.path(base_dir, "QQQ", paste0("data_CNT_", cnt, "_QQQ.sav"))
    cog_path <- file.path(base_dir, "COG", paste0("data_CNT_", cnt, "_COG.sav"))
    df <- read_sav(qqq_path)
    df_resp <- read_sav(cog_path)
    
    # ---------- Background / imputation ----------
    df_background <- df[, -c(1:22, 1063:1278)]
    df_test <- df_background %>%
      mutate(across(where(is.labelled), ~ if(is.numeric(.)) as.numeric(zap_labels(.)) else as.numeric(as_factor(.))))
    df_test[df_test == 9999999] <- NA
    df_test <- df_test %>% select(where(~ mean(is.na(.)) < 0.80))
    split_indices <- split(seq_len(ncol(df_test)), cut(seq_len(ncol(df_test)), 5, labels = FALSE))
    df_blocks <- lapply(split_indices, function(cols) df_test[, cols])
    completed_blocks <- lapply(df_blocks, function(x) complete(mice(x, method = "pmm", m = 1), "long"))
    combined_completed <- do.call(cbind, completed_blocks) %>% select(-matches("^\\.imp$|-?\\.id$"))
    combined_completed_no_na <- combined_completed[, colSums(is.na(combined_completed)) == 0]
    
    rm(df_background, df_test, df_blocks, completed_blocks, combined_completed)
    gc(FALSE)
    
    # ---------- Response items ----------
    resp_df <- df_resp %>% select(matches("^CR.*S$") & !matches("VS$")) %>% select(where(~ !all(is.na(.))))
    resp <- resp_df %>% select(where(~ max(., na.rm = TRUE) > 0))
    
    # ---------- Item mapping ----------
    orig_items <- irt$item$item
    new_items  <- colnames(resp)
    item_map <- match(orig_items, new_items)
    
    Bf <- irt$B.fixed.estimated
    Bf[,1] <- item_map[Bf[,1]]
    Bf <- Bf[!is.na(Bf[,1]), , drop = FALSE]
    
    Xf <- irt$xsi.fixed.estimated
    Xf[,1] <- item_map[Xf[,1]]
    Xf <- Xf[!is.na(Xf[,1]), , drop = FALSE]
    
    # ---------- PCA ----------
    pca <- prcomp(combined_completed_no_na)
    ncomp <- which(cumsum(summary(pca)$importance[2,]) >= 0.90)[1]
    pca_df <- as.data.frame(pca$x[, 1:ncomp])
    
    rm(pca); gc(FALSE)
    
    # ---------- TAM IRT ----------
    irt_tam <- tam.mml.2pl(resp = resp, B.fixed = Bf, xsi.fixed = Xf, Y = pca_df,
                           pweights = df$W_FSTUWT, pid = df$CNTSTUID,
                           variance.fixed = matrix(c(1,1,1), ncol = 3))
    
    pvs <- tam.pv(irt_tam, normal.approx = TRUE)
    
    # ---------- BMA IRT ----------
    irt_test <- tam.mml.2pl(resp = resp, B.fixed = Bf, xsi.fixed = Xf,
                            pweights = df$W_FSTUWT, pid = df$CNTSTUID,
                            variance.fixed = matrix(c(1,1,1), ncol = 3))
    
    ## ---- Draw theta  ----
    mu <- irt_test$person$EAP
    sd <- irt_test$person$SD.EAP
    combined_completed_no_na$theta_estimates <-
      rnorm(length(mu), mean = mu, sd = sd)
    
    ## ---- Build BMA Predictor Set ----
    build_X <- function(df) {
      theta <- df[, "theta_estimates", drop = FALSE]
      preds <- df[, setdiff(colnames(df), "theta_estimates"), drop = FALSE]
      preds <- preds[, apply(preds, 2, sd) > 0, drop = FALSE]
      list(theta = theta, preds = preds)}
    
    xp <- build_X(combined_completed_no_na)
    theta <- xp$theta
    P <- xp$preds
   
    if (ncol(P) > 1) {
      cor_mat <- cor(P, use = "pairwise.complete.obs")
      high_corr <- which(abs(cor_mat) > 0.999, arr.ind = TRUE)
      high_corr <- high_corr[high_corr[, 1] < high_corr[, 2], , drop = FALSE]
      
      if (nrow(high_corr) > 0) {
        drop_idx <- unique(high_corr[, 2])
        P <- P[, -drop_idx, drop = FALSE]}}
    
    if (ncol(P) == 0) {
      xp <- build_X(combined_completed_no_na)
      P <- xp$preds
      cor_mat <- cor(P, use = "pairwise.complete.obs")
      to_drop <- findCorrelation(cor_mat, cutoff = 0.9)
      
      if (length(to_drop) < ncol(P)) {
        P <- P[, -to_drop, drop = FALSE]} else {
        stop("caret::findCorrelation() would remove all predictors.")} }
    
    ## ---- BMA  ----
    X <- cbind(theta, P)
    
    bma_fit <- bms(X.data = X, g = "HQ", mprior = "uniform")
    pdens <- pred.density(bma_fit, newdata = X)
    dens_list <- pdens$densities()
    
    pv_matrix_bma <- replicate(10, {
      vapply(seq_along(dens_list), function(j) {
        d <- dens_list[[j]]
        probs <- d$y / sum(d$y)
        sample(d$x, size = 1, prob = probs)
      }, numeric(1)) })
    
    pv_df_bma <- as.data.frame(pv_matrix_bma * 100 + 400)  %>% mutate(obs = row_number())  
    
    pv_long_bma <- pv_df_bma %>% pivot_longer(cols = -obs, names_to = "PV", names_pattern = "V(\\d+)", values_to = "Score") %>%
      mutate(PV = paste0("PV", PV, "READ"), PV = factor(PV, levels = paste0("PV", 1:10, "READ")),   
             Source = "BMA", CNT = cnt) %>%
      arrange(PV, obs)  
    
    # ---------- PISA & TAM long ----------
    
    pisa_long <- df %>% mutate(obs = row_number()) %>% select(obs, matches("^PV.*READ$")) %>%
      pivot_longer(cols = -obs, names_to = "PV", values_to = "Score") %>%
      mutate(Source = "PISA", PV = factor(PV, levels = paste0("PV", 1:10, "READ")), CNT = cnt) %>%
      arrange(PV, obs)
    
    tam_long <- pvs$pv %>% select(-pid) %>% mutate(obs = row_number()) %>% select(obs, everything()) %>%
      pivot_longer(cols = -obs, names_to = "PV", values_to = "Score") %>%
      mutate(PV = sub("\\.Dim1$", "READ", PV), Score = Score * 100 + 400, Source = "TAM", 
             PV = factor(PV, levels = paste0("PV", 1:10, "READ")), CNT = cnt) %>%
      arrange(PV, obs)
    
    combined_data <- bind_rows(pisa_long, tam_long, pv_long_bma)
    
    # ---------- Optional density plot ----------
    if(plot){
      p <- ggplot(combined_data, aes(x = Score, color = Source)) +
        geom_density(linewidth = 1.1) +
        facet_wrap(~ PV, ncol = 5) +
        labs(title = paste0("PV Densities (Reading), PISA vs. BMA (", cnt, ")"),
             x = "Score", y = "Density") +
        labs(title = paste0("PV Densities: PISA 2022 ", cnt, " Reading"), x = "Score", y = "Density") +
        scale_color_viridis_d(option = "G", direction = -1, begin = .2, end = .8) +
        theme_minimal(base_size = 14) + 
        theme(legend.position = "bottom",  panel.grid.minor = element_blank())
      
      ggsave(filename = file.path("C:/Users/harra.AD/OneDrive - UW-Madison/Documents/PVs/pv_density_plots", paste0("PV_density_", cnt, ".png")),
             plot = p, width = 14, height = 8, dpi = 300, bg = "white")
      rm(p); gc(FALSE)
    }
    
    # ---------- Save PVs ----------
    pv_file <- file.path("C:/Users/harra.AD/OneDrive - UW-Madison/Documents/PVs/pvs_by_country_BMA", 
                         paste0("PV_", cnt, ".rds"))
    saveRDS(list(combined_data = combined_data, bma_fit = bma_fit), pv_file)
    
    # ---------- Cleanup ----------
    rm(df, df_resp, resp_df, resp, pvs, pv_df_bma, pv_long_bma, pisa_long, tam_long, combined_data,
       irt_tam, irt_test, mu, sd, X, bma_fit, pdens, dens_list, Bf, Xf, pca_df)
    gc(FALSE)
    
    invisible(pv_file)
    
    return(TRUE)
    
  }, error = function(e) {
    
    message("FAILED for ", cnt, ": ", e$message)

    writeLines(
      paste(Sys.time(), cnt, e$message),
      file.path(base_dir, "failed_countries.log"),
      sep = "\n",
      useBytes = TRUE
    )
    
    return(FALSE)
  })
}

load("C:/Users/harra.AD/OneDrive - UW-Madison/Documents/pisa testing anchor subset.RData")

qqq_dir <- "C:/Users/harra.AD/OneDrive - UW-Madison/Documents/PVs/QQQ"
cog_dir <- "C:/Users/harra.AD/OneDrive - UW-Madison/Documents/PVs/COG"

qqq_files <- list.files(qqq_dir, pattern = "^data_CNT_.*_QQQ\\.sav$")
cog_files <- list.files(cog_dir, pattern = "^data_CNT_.*_COG\\.sav$")

qqq_cnt <- sub("^data_CNT_(.*)_QQQ\\.sav$", "\\1", qqq_files)
cog_cnt <- sub("^data_CNT_(.*)_COG\\.sav$", "\\1", cog_files)

cnt_list <- intersect(qqq_cnt, cog_cnt)

# remove GTM KHM PRY VNM bc no reading item data
exclude <- c("GTM", "KHM", "PRY", "VNM")

cnt_list <- setdiff(cnt_list, c(exclude))

plan(multisession, workers = parallel::detectCores() - 1)

pv_files <- future_lapply(done, function(cnt) {
  message("Processing ", cnt)
  tryCatch({
    process_country_pvs(cnt, base_dir = "C:/Users/harra.AD/OneDrive - UW-Madison/Documents/PVs", irt = irt, plot = T)
  }, error = function(e){
    message("FAILED for ", cnt, ": ", e$message)
    return(NULL)
  })
}, future.seed = T)
