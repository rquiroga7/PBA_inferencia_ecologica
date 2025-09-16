library(data.table)
library(dplyr)

# Load the same data and apply the same filters as in ecological_inference.R
cat("=== PARTY VOTE PERCENTAGES ANALYSIS ===\n")

# Read data
data_2023 <- fread("electores_circuitos_2023.csv")
data_2025 <- fread("electores_circuitos_2025.csv")

# Get parties (same logic as main script)
parties_2023 <- sort(unique(data_2023$nombreAgrupacion))
parties_2025 <- sort(unique(data_2025$nombreAgrupacion))

political_parties_2023 <- setdiff(parties_2023, c("VOTANTES"))
political_parties_2025 <- setdiff(parties_2025, c("VOTANTES"))

political_parties_2023 <- c(political_parties_2023, "NO_PADRON")
political_parties_2025 <- c(political_parties_2025, "NO_PADRON")

# Filter data to political parties only
data_2023_main <- data_2023[nombreAgrupacion %in% political_parties_2023]
data_2025_main <- data_2025[nombreAgrupacion %in% political_parties_2025]

# Province filtering (same as main script)
common_provinces <- intersect(unique(data_2023_main$secprov), unique(data_2025_main$secprov))
data_2023_main <- data_2023_main[secprov %in% common_provinces]
data_2025_main <- data_2025_main[secprov %in% common_provinces]

# Create circuit keys
data_2023_main[, circuit_key := paste(secprov, seccionId, circuitoId, sep = "_")]
data_2025_main[, circuit_key := paste(secprov, seccionId, circuitoId, sep = "_")]

# Get common circuits
circuits_2023 <- unique(data_2023_main$circuit_key)
circuits_2025 <- unique(data_2025_main$circuit_key)
common_circuits <- intersect(circuits_2023, circuits_2025)

# Filter to common circuits
data_2023_common <- data_2023_main[circuit_key %in% common_circuits]
data_2025_common <- data_2025_main[circuit_key %in% common_circuits]

cat("Total circuits after filtering:", length(common_circuits), "\n\n")

# Calculate aggregate vote totals BEFORE adding NO_PADRON
cat("=== ORIGINAL PARTY VOTES (before NO_PADRON adjustment) ===\n")

# 2023 totals (excluding NO_PADRON since it doesn't exist in original data)
political_parties_2023_orig <- setdiff(political_parties_2023, "NO_PADRON")
votes_2023_summary <- data_2023_common %>%
  filter(nombreAgrupacion %in% political_parties_2023_orig) %>%
  group_by(nombreAgrupacion) %>%
  summarise(total_votes = sum(votos, na.rm = TRUE)) %>%
  mutate(percentage = total_votes / sum(total_votes) * 100) %>%
  arrange(desc(total_votes))

cat("2023 Vote Distribution:\n")
for(i in 1:nrow(votes_2023_summary)) {
  cat(sprintf("  %-15s: %10.0f votes (%.1f%%)\n", 
              votes_2023_summary$nombreAgrupacion[i],
              votes_2023_summary$total_votes[i],
              votes_2023_summary$percentage[i]))
}
cat(sprintf("  %-15s: %10.0f total votes\n", "TOTAL", sum(votes_2023_summary$total_votes)))

# 2025 totals (excluding NO_PADRON since it doesn't exist in original data)
political_parties_2025_orig <- setdiff(political_parties_2025, "NO_PADRON")
votes_2025_summary <- data_2025_common %>%
  filter(nombreAgrupacion %in% political_parties_2025_orig) %>%
  group_by(nombreAgrupacion) %>%
  summarise(total_votes = sum(votos, na.rm = TRUE)) %>%
  mutate(percentage = total_votes / sum(total_votes) * 100) %>%
  arrange(desc(total_votes))

cat("\n2025 Vote Distribution:\n")
for(i in 1:nrow(votes_2025_summary)) {
  cat(sprintf("  %-15s: %10.0f votes (%.1f%%)\n", 
              votes_2025_summary$nombreAgrupacion[i],
              votes_2025_summary$total_votes[i],
              votes_2025_summary$percentage[i]))
}
cat(sprintf("  %-15s: %10.0f total votes\n", "TOTAL", sum(votes_2025_summary$total_votes)))

# Now apply the same filtering as in the main script to see what goes into the model
cat("\n=== APPLYING MAIN SCRIPT FILTERS ===\n")

# Recreate the wide format data (same as main script)
political_parties_2023_data <- setdiff(political_parties_2023, "NO_PADRON")
votes_2023_wide <- data_2023_common %>%
  filter(nombreAgrupacion %in% political_parties_2023_data) %>%
  select(circuit_key, nombreAgrupacion, votos, cantidadElectores) %>%
  tidyr::pivot_wider(
    names_from = nombreAgrupacion, 
    values_from = votos, 
    values_fill = 0,
    names_prefix = "v2023_"
  ) %>%
  left_join(
    data_2023_common %>% 
      filter(nombreAgrupacion == political_parties_2023_data[1]) %>% 
      select(circuit_key, cantidadElectores), 
    by = "circuit_key"
  )

votes_2025_wide <- data_2025_common %>%
  filter(nombreAgrupacion %in% political_parties_2025) %>%
  select(circuit_key, nombreAgrupacion, votos, cantidadElectores) %>%
  tidyr::pivot_wider(
    names_from = nombreAgrupacion, 
    values_from = votos, 
    values_fill = 0,
    names_prefix = "v2025_"
  ) %>%
  left_join(
    data_2025_common %>% 
      filter(nombreAgrupacion == political_parties_2025[1]) %>% 
      select(circuit_key, cantidadElectores_2025 = cantidadElectores), 
    by = "circuit_key"
  )

# Merge both years
electoral_data <- votes_2023_wide %>%
  inner_join(votes_2025_wide, by = "circuit_key") %>%
  as.data.table()

# Apply electores difference filter (10%)
electoral_data[, electores_diff_pct := abs(cantidadElectores_2025 - cantidadElectores) / cantidadElectores * 100]
electoral_data <- electoral_data[electores_diff_pct <= 10]

# Add NO_PADRON categories - represents people in electoral roll but not in voting data
political_parties_2023_actual <- setdiff(political_parties_2023, "NO_PADRON")
political_parties_2025_actual <- setdiff(political_parties_2025, "NO_PADRON")

# Calculate total votes per circuit (including NO_VOTANTES)
vote_cols_2023_actual <- paste0("v2023_", political_parties_2023_actual)
vote_cols_2025_actual <- paste0("v2025_", political_parties_2025_actual)

electoral_data[, total_actual_votes_2023 := rowSums(.SD, na.rm = TRUE), .SDcols = vote_cols_2023_actual]
electoral_data[, total_actual_votes_2025 := rowSums(.SD, na.rm = TRUE), .SDcols = vote_cols_2025_actual]

# NO_PADRON = people in electoral roll but not accounted for in voting data
electoral_data[, v2023_NO_PADRON := pmax(0, cantidadElectores - total_actual_votes_2023)]
electoral_data[, v2025_NO_PADRON := pmax(0, cantidadElectores_2025 - total_actual_votes_2025)]

# Calculate totals including NO_PADRON
vote_cols_2023 <- paste0("v2023_", political_parties_2023)
vote_cols_2025 <- paste0("v2025_", political_parties_2025)
electoral_data[, total_votes_2023 := rowSums(.SD, na.rm = TRUE), .SDcols = vote_cols_2023]
electoral_data[, total_votes_2025 := rowSums(.SD, na.rm = TRUE), .SDcols = vote_cols_2025]

# Remove circuits with zero votes and apply minimum vote filter (same as main script)
electoral_data <- electoral_data[total_votes_2023 > 0 & total_votes_2025 > 0]
electoral_data <- electoral_data[!is.na(total_votes_2023) & !is.na(total_votes_2025)]
electoral_data <- electoral_data[total_votes_2023 >= 50 & total_votes_2025 >= 50]

cat("Circuits after all filtering:", nrow(electoral_data), "\n")

# Calculate final aggregate percentages that go into the model
cat("\n=== FINAL DATA INPUT TO ECOLOGICAL INFERENCE MODEL ===\n")

# 2023 final totals (including NO_PADRON)
final_2023_totals <- electoral_data[, lapply(.SD, sum, na.rm = TRUE), .SDcols = vote_cols_2023]
total_2023_votes <- sum(final_2023_totals)

cat("2023 Final Vote Distribution (input to model):\n")
for(party in political_parties_2023) {
  vote_col <- paste0("v2023_", party)
  votes <- final_2023_totals[[vote_col]]
  pct <- votes / total_2023_votes * 100
  cat(sprintf("  %-15s: %10.0f votes (%.1f%%)\n", party, votes, pct))
}
cat(sprintf("  %-15s: %10.0f total votes\n", "TOTAL", total_2023_votes))

# 2025 final totals (including NO_PADRON)
final_2025_totals <- electoral_data[, lapply(.SD, sum, na.rm = TRUE), .SDcols = vote_cols_2025]
total_2025_votes <- sum(final_2025_totals)

cat("\n2025 Final Vote Distribution (input to model):\n")
for(party in political_parties_2025) {
  vote_col <- paste0("v2025_", party)
  votes <- final_2025_totals[[vote_col]]
  pct <- votes / total_2025_votes * 100
  cat(sprintf("  %-15s: %10.0f votes (%.1f%%)\n", party, votes, pct))
}
cat(sprintf("  %-15s: %10.0f total votes\n", "TOTAL", total_2025_votes))

# Compare changes
cat("\n=== PARTY VOTE SHARE CHANGES ===\n")
common_parties_orig <- intersect(political_parties_2023_orig, political_parties_2025_orig)
for(party in common_parties_orig) {
  vote_col_2023 <- paste0("v2023_", party)
  vote_col_2025 <- paste0("v2025_", party)
  
  pct_2023 <- final_2023_totals[[vote_col_2023]] / total_2023_votes * 100
  pct_2025 <- final_2025_totals[[vote_col_2025]] / total_2025_votes * 100
  change <- pct_2025 - pct_2023
  
  cat(sprintf("  %-15s: %.1f%% → %.1f%% (change: %+.1f pp)\n", 
              party, pct_2023, pct_2025, change))
}

# NO_PADRON impact
no_padron_2023 <- final_2023_totals[["v2023_NO_PADRON"]] / total_2023_votes * 100
no_padron_2025 <- final_2025_totals[["v2025_NO_PADRON"]] / total_2025_votes * 100
cat(sprintf("  %-15s: %.1f%% → %.1f%% (change: %+.1f pp)\n", 
            "NO_PADRON", no_padron_2023, no_padron_2025, no_padron_2025 - no_padron_2023))

cat("\n=== SUMMARY ===\n")
cat("These are the final vote percentages that are fed into the ei.MD.bayes model\n")
cat("after applying all filters (province matching, circuit overlap, electores difference ≤10%,\n")
cat("minimum 50 votes per circuit, and NO_PADRON adjustment for electoral roll differences).\n")