import pandas as pd
import pyreadstat
import os
import time

input_file = r"C:\Users\harra.AD\OneDrive - UW-Madison\Documents\PVs\CY08MSP_STU_COG.SAV"
output_folder = r"C:\Users\harra.AD\OneDrive - UW-Madison\Documents\PVs\COG"
os.makedirs(output_folder, exist_ok=True)
start_time = time.time()

print("Reading SAV file...")
df, meta = pyreadstat.read_sav(input_file)
print(f"Finished reading file: {round(time.time() - start_time, 1)} seconds.")
print(f"Number of rows: {len(df)}, number of columns: {len(df.columns)}")

total_cnt = df["CNT"].nunique()
print(f"Now splitting by CNT... total unique CNTs: {total_cnt}")

for i, (cnt, subset) in enumerate(df.groupby("CNT"), start=1):
    filename = os.path.join(output_folder, f"data_CNT_{cnt}_COG.sav")

    pyreadstat.write_sav(
        subset,
        filename,
        column_labels=getattr(meta, "column_labels", None),  # variable labels
        variable_value_labels=getattr(meta, "variable_value_labels", None),
        variable_measure=getattr(meta, "variable_measure", None),
        variable_format=getattr(meta, "variable_formats", None),  # singular, maps formats
        missing_ranges=getattr(meta, "missing_ranges", None)
    )

    print(f"[{i}/{total_cnt}] Saved {filename} ({len(subset)} rows)")

total_time = round(time.time() - start_time, 1)
print(f"Total time: {total_time} seconds")

