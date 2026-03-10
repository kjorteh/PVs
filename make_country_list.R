# Generate country_list.txt for HTCondor jobs
# Kjorte Harra

# ---- Project directory ----
base_dir <- "/home/harra/pisa_spars"

qqq_dir <- file.path(base_dir, "QQQ")
cog_dir <- file.path(base_dir, "COG")
output_dir <- file.path(base_dir, "output")

# ---- Manual exclusions ----
exclude_countries <- c("VNM", "GTM", "KHM", "PRY")

# ---- Detect available files ----
qqq_files <- list.files(
  qqq_dir,
  pattern = "^data_CNT_.*_QQQ\\.sav$"
)

cog_files <- list.files(
  cog_dir,
  pattern = "^data_CNT_.*_COG\\.sav$"
)

# ---- Extract country codes ----
qqq_cnt <- sub("^data_CNT_(.*)_QQQ\\.sav$", "\\1", qqq_files)
cog_cnt <- sub("^data_CNT_(.*)_COG\\.sav$", "\\1", cog_files)

# ---- Countries with both datasets ----
cnt_list <- intersect(qqq_cnt, cog_cnt)

# ---- Remove excluded countries ----
cnt_list <- setdiff(cnt_list, exclude_countries)

# ---- Optional: skip completed countries ----
existing_outputs <- list.files(
  output_dir,
  pattern = "^pv_spars_pisa_.*\\.RData$"
)

completed_cnt <- sub("^pv_spars_pisa_(.*)\\.RData$", "\\1", existing_outputs)

cnt_list <- setdiff(cnt_list, completed_cnt)

# ---- Sort alphabetically ----
cnt_list <- sort(cnt_list)

# ---- Save list for Condor ----
outfile <- file.path(base_dir, "country_list.txt")

writeLines(cnt_list, outfile)

cat("\nCountry list created:\n")
print(cnt_list)

cat("\nTotal jobs to run:", length(cnt_list), "\n")
cat("Saved to:", outfile, "\n")