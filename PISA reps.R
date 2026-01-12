# PISA replication by Country 
## Kjorte Harra

library(haven, quietly = T)
library(dplyr, quietly = T)
library(tidyr, quietly = T)
library(TAM, quietly = T)
library(mice, quietly = T)
library(future.apply, quietly = T)


process_country <- function(cnt, base_dir, irt) {
  
  #message("Processing ", cnt)
  
  ## ---- Load data ----
  
  df <- read_sav(file.path(base_dir, "QQQ", paste0("data_CNT_", cnt, "_QQQ.sav")))
  
  ## ---- PISA PVs ----
  
  pisa_long <- df %>%
    select(starts_with("PV") & ends_with("READ")) %>%
    pivot_longer(
      cols = everything(),
      names_to = "PV",
      values_to = "Score") %>%
    mutate(
      Source = "PISA",
      CNT    = cnt,
      PV     = factor(PV, levels = paste0("PV", 1:10, "READ")))
  
  df_resp <- read_sav(file.path(base_dir, "COG", paste0("data_CNT_", cnt, "_COG.sav")))
  
  ## ---- Response data ----
  
  resp_df <- df_resp %>%
    select(matches("^CR.*S$") & !matches("VS$")) %>%
    select(where(~ !all(is.na(.))))
  
  resp <- resp_df %>% select(where(~ max(., na.rm = TRUE) > 0))
  
  if (ncol(resp) == 0) {
    message("No valid response data for ", cnt, ". Skipping this country.")
    return(NULL) }
  
  rm(df_resp); gc(FALSE)
  
  ## ---- Background variables ----
  df_test <- df[, -c(1:22, 1063:1278)] %>%
    mutate(across(where(is.labelled), ~ {
      if (is.numeric(.)) as.numeric(zap_labels(.))
      else as.numeric(as_factor(.))  }))
  
  df_test[df_test == 9999999] <- NA
  df_test <- df_test %>% select(where(~ mean(is.na(.)) < 0.80))
  
  ## ---- Imputation ----
  
  split_indices <- split(seq_len(ncol(df_test)), cut(seq_len(ncol(df_test)), 5, labels = FALSE))
  
  completed_blocks <- lapply(split_indices, function(i) {
    complete(mice(df_test[, i], method = "pmm", m = 1), "long")})
  
  combined_completed <- do.call(cbind, completed_blocks) %>% select(-matches("^\\.imp$|-?\\.id$"))
  
  na_counts <- colSums(is.na(combined_completed))
  na_counts[na_counts == 0]
  combined_completed_no_na <- combined_completed[, colSums(is.na(combined_completed)) == 0]
  
  rm(df_test, completed_blocks); gc(FALSE)
  
  ## ---- PCA ----
  
  pca <- prcomp(combined_completed_no_na)
  pca_df <- as.data.frame(pca$x[, 1:which(cumsum(summary(pca)$importance[2, ]) >= 0.90)[1]]) 
  rm(combined_completed, combined_completed_no_na); gc(FALSE)
  
  ## ---- Item mapping ----
  
  orig_items <- irt$item$item
  new_items  <- colnames(resp)
  item_map   <- match(orig_items, new_items)
  
  Bf <- irt$B.fixed.estimated
  Bf[, 1] <- item_map[Bf[, 1]]
  Bf <- Bf[!is.na(Bf[, 1]), , drop = FALSE]
  
  Xf <- irt$xsi.fixed.estimated
  Xf[, 1] <- item_map[Xf[, 1]]
  Xf <- Xf[!is.na(Xf[, 1]), , drop = FALSE]
  
  ## ---- TAM model ----
  
  irt_test <- tam.mml.2pl(
    resp      = resp,
    B.fixed   = Bf,
    xsi.fixed = Xf,
    Y         = pca_df,
    pweights  = df$W_FSTUWT,
    pid       = df$CNTSTUID)
  
  ## ---- Plausible values ----
  
  pvs <- tam.pv(irt_test, normal.approx = TRUE)
  
  ## ---- TAM PVs ----
  
  tam_long <- pvs$pv %>%
    select(-pid) %>%
    pivot_longer(
      cols = everything(),
      names_to = "PV",
      values_to = "Score") %>%
    mutate(
      PV     = sub("\\.Dim1$", "READ", PV),
      Score = Score * 100 + 500,
      Source = "TAM",
      CNT    = cnt,
      PV     = factor(PV, levels = paste0("PV", 1:10, "READ")))
  
  ## ---- Combine & save ----
  
  combined_data <- bind_rows(pisa_long, tam_long)
  
  out_file <- file.path("C:/Users/harra.AD/OneDrive - UW-Madison/Documents/PVs/pvs_by_country", paste0("PV_", cnt, ".rds"))
  saveRDS(combined_data, out_file)
  invisible(out_file)
}

#dir.create("C:/Users/harra.AD/OneDrive - UW-Madison/Documents/PVs/pvs_by_country", showWarnings = FALSE)
qqq_dir <- "C:/Users/harra.AD/OneDrive - UW-Madison/Documents/PVs/QQQ" 
cog_dir <- "C:/Users/harra.AD/OneDrive - UW-Madison/Documents/PVs/COG" 
qqq_files <- list.files(qqq_dir, pattern = "^data_CNT_.*_QQQ\\.sav$", full.names = FALSE) 
cog_files <- list.files(cog_dir, pattern = "^data_CNT_.*_COG\\.sav$", full.names = FALSE) 
qqq_cnt <- sub("^data_CNT_(.*)_QQQ\\.sav$", "\\1", qqq_files) 
cog_cnt <- sub("^data_CNT_(.*)_COG\\.sav$", "\\1", cog_files)

qqq_cnt <- sub("^data_CNT_(.*)_QQQ\\.sav$", "\\1", qqq_files)
cog_cnt <- sub("^data_CNT_(.*)_COG\\.sav$", "\\1", cog_files)

cnt_list <- intersect(qqq_cnt, cog_cnt)

plan(multisession, workers = parallel::detectCores())

(results <- future_lapply(
  cnt_list[c(80,1)], 
  function(cnt) {
    tryCatch({
      message("Processing ", cnt)
      process_country(cnt = cnt, base_dir = "C:/Users/harra.AD/OneDrive - UW-Madison/Documents/PVs", irt = irt)
    }, error = function(e) {
      message("FAILED: ", cnt, " — ", e$message)
      NULL}) }, future.seed = TRUE))
