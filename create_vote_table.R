library(data.table)
library(dplyr)

# Load the comparison datasets
fp_comparison <- fread('data/fp_comparison_sept_oct_2025.csv')
lla_comparison <- fread('data/lla_comparison_sept_oct_2025.csv')

# Merge both datasets
vote_table <- fp_comparison %>%
  select(seccion_id, seccion_nombre, 
         fp_sept_filtered = fp_votes_sept_filt,
         fp_sept_unfiltered = fp_votes_sept_unfilt,
         fp_oct = fp_votes_oct) %>%
  left_join(
    lla_comparison %>%
      select(seccion_id,
             lla_sept_filtered = lla_votes_sept_filt,
             lla_sept_unfiltered = lla_votes_sept_unfilt,
             lla_oct = lla_votes_oct),
    by = "seccion_id"
  ) %>%
  arrange(seccion_id)

# Calculate totals
totals <- data.frame(
  seccion_id = NA,
  seccion_nombre = "TOTAL",
  fp_sept_filtered = sum(vote_table$fp_sept_filtered, na.rm = TRUE),
  fp_sept_unfiltered = sum(vote_table$fp_sept_unfiltered, na.rm = TRUE),
  fp_oct = sum(vote_table$fp_oct, na.rm = TRUE),
  lla_sept_filtered = sum(vote_table$lla_sept_filtered, na.rm = TRUE),
  lla_sept_unfiltered = sum(vote_table$lla_sept_unfiltered, na.rm = TRUE),
  lla_oct = sum(vote_table$lla_oct, na.rm = TRUE)
)

# Combine with totals row
vote_table_with_totals <- bind_rows(vote_table, totals)

# Save to CSV
fwrite(vote_table_with_totals, "data/vote_summary_by_seccion.csv")

cat("Vote summary table created and saved to data/vote_summary_by_seccion.csv\n")
cat(sprintf("Total rows: %d secciones + 1 totals row\n", nrow(vote_table)))

# Display the first few rows and the totals
cat("\n=== First 10 secciones ===\n")
print(head(vote_table_with_totals, 10))
cat("\n=== Totals row ===\n")
print(tail(vote_table_with_totals, 1))
