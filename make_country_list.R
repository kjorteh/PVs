# Generate country_list.txt for HTCondor jobs
# Kjorte Harra

base_dir <- "/home/harra/pisa_spars"
qqq_dir <- file.path(base_dir, "QQQ")
cog_dir <- file.path(base_dir, "COG")
output_dir <- file.path(base_dir, "output")
exclude_countries <- c("VNM", "GTM", "KHM", "PRY")

qqq_files <- list.files(
  qqq_dir,
  pattern = "^data_CNT_.*_QQQ\\.sav$"
)

cog_files <- list.files(
  cog_dir,
  pattern = "^data_CNT_.*_COG\\.sav$"
)

qqq_cnt <- sub("^data_CNT_(.*)_QQQ\\.sav$", "\\1", qqq_files)
cog_cnt <- sub("^data_CNT_(.*)_COG\\.sav$", "\\1", cog_files)

cnt_list <- intersect(qqq_cnt, cog_cnt)
cnt_list <- setdiff(cnt_list, exclude_countries)

existing_outputs <- list.files(
  output_dir,
  pattern = "^pv_spars_pisa_.*\\.RData$"
)

completed_cnt <- sub("^pv_spars_pisa_(.*)\\.RData$", "\\1", existing_outputs)
cnt_list <- setdiff(cnt_list, completed_cnt)
cnt_list <- sort(cnt_list)

outfile <- file.path(base_dir, "country_list.txt")
writeLines(cnt_list, outfile)
