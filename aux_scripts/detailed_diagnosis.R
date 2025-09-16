library(data.table)
library(dplyr)

# File paths
file_2023 <- "electores_circuitos_2023.csv"
file_2025 <- "electores_circuitos_2025.csv"

cat("=== COMPREHENSIVE CIRCUIT FILTERING ANALYSIS ===\n")

# Read data
data_2023 <- fread(file_2023)
data_2025 <- fread(file_2025)

cat("Data loaded:\n")
cat("2023:", nrow(data_2023), "rows\n")
cat("2025:", nrow(data_2025), "rows\n")

# Check parties
parties_2023 <- sort(unique(data_2023$nombreAgrupacion))
parties_2025 <- sort(unique(data_2025$nombreAgrupacion))

cat("\nParties in 2023:", paste(parties_2023, collapse = ", "), "\n")
cat("Parties in 2025:", paste(parties_2025, collapse = ", "), "\n")

# Define political parties
political_parties_2023 <- setdiff(parties_2023, c("VOTANTES", "NO_VOTANTES"))
political_parties_2025 <- setdiff(parties_2025, c("VOTANTES", "NO_VOTANTES"))

cat("\nPolitical parties 2023:", paste(political_parties_2023, collapse = ", "), "\n")
cat("Political parties 2025:", paste(political_parties_2025, collapse = ", "), "\n")

# Filter to political parties and create circuit keys
data_2023_main <- data_2023[nombreAgrupacion %in% political_parties_2023]
data_2025_main <- data_2025[nombreAgrupacion %in% political_parties_2025]

data_2023_main[, circuit_key := paste(secprov, seccionId, circuitoId, sep = "_")]
data_2025_main[, circuit_key := paste(secprov, seccionId, circuitoId, sep = "_")]

# STEP 1: Count unique circuits
circuits_2023 <- unique(data_2023_main$circuit_key)
circuits_2025 <- unique(data_2025_main$circuit_key)
common_circuits <- intersect(circuits_2023, circuits_2025)

cat("\n=== STEP 1: CIRCUIT OVERLAP ANALYSIS ===\n")
cat("Unique circuits in 2023:", length(circuits_2023), "\n")
cat("Unique circuits in 2025:", length(circuits_2025), "\n")
cat("Common circuits:", length(common_circuits), "\n")
cat("Circuits only in 2023:", length(setdiff(circuits_2023, circuits_2025)), "\n")
cat("Circuits only in 2025:", length(setdiff(circuits_2025, circuits_2023)), "\n")
cat("Overlap percentage:", round(length(common_circuits) / max(length(circuits_2023), length(circuits_2025)) * 100, 1), "%\n")

# STEP 2: Check if we have complete data for common circuits
data_2023_common <- data_2023_main[circuit_key %in% common_circuits]
data_2025_common <- data_2025_main[circuit_key %in% common_circuits]

cat("\n=== STEP 2: DATA COMPLETENESS CHECK ===\n")
cat("2023 rows for common circuits:", nrow(data_2023_common), "\n")
cat("2025 rows for common circuits:", nrow(data_2025_common), "\n")

# Check for missing parties in circuits
circuits_with_all_parties_2023 <- data_2023_common[, .(n_parties = length(unique(nombreAgrupacion))), by = circuit_key]
circuits_with_all_parties_2025 <- data_2025_common[, .(n_parties = length(unique(nombreAgrupacion))), by = circuit_key]

expected_parties_2023 <- length(political_parties_2023)
expected_parties_2025 <- length(political_parties_2025)

complete_circuits_2023 <- circuits_with_all_parties_2023[n_parties == expected_parties_2023]$circuit_key
complete_circuits_2025 <- circuits_with_all_parties_2025[n_parties == expected_parties_2025]$circuit_key

cat("Circuits with all", expected_parties_2023, "parties in 2023:", length(complete_circuits_2023), "\n")
cat("Circuits with all", expected_parties_2025, "parties in 2025:", length(complete_circuits_2025), "\n")

complete_both <- intersect(complete_circuits_2023, complete_circuits_2025)
cat("Circuits complete in both years:", length(complete_both), "\n")

# STEP 3: Check cantidadElectores differences for complete circuits
if(length(complete_both) > 0) {
  cat("\n=== STEP 3: CANTIDADELECTORES ANALYSIS ===\n")
  
  # Get electores data for complete circuits
  electores_2023 <- data_2023_common[circuit_key %in% complete_both & nombreAgrupacion == political_parties_2023[1], 
                                     .(circuit_key, cantidadElectores_2023 = cantidadElectores)]
  electores_2025 <- data_2025_common[circuit_key %in% complete_both & nombreAgrupacion == political_parties_2025[1], 
                                     .(circuit_key, cantidadElectores_2025 = cantidadElectores)]
  
  electores_merged <- merge(electores_2023, electores_2025, by = "circuit_key")
  electores_merged[, diff_pct := abs(cantidadElectores_2025 - cantidadElectores_2023) / cantidadElectores_2023 * 100]
  
  cat("CantidadElectores difference statistics:\n")
  cat("Min difference:", round(min(electores_merged$diff_pct, na.rm = TRUE), 1), "%\n")
  cat("Max difference:", round(max(electores_merged$diff_pct, na.rm = TRUE), 1), "%\n")
  cat("Mean difference:", round(mean(electores_merged$diff_pct, na.rm = TRUE), 1), "%\n")
  cat("Median difference:", round(median(electores_merged$diff_pct, na.rm = TRUE), 1), "%\n")
  
  # Show distribution
  thresholds <- c(5, 10, 15, 20, 30, 50)
  for(threshold in thresholds) {
    remaining <- sum(electores_merged$diff_pct <= threshold, na.rm = TRUE)
    lost <- nrow(electores_merged) - remaining
    cat(sprintf("At %d%% threshold: %d circuits remain (%d lost, %.1f%% retention)\n", 
                threshold, remaining, lost, remaining/nrow(electores_merged)*100))
  }
  
  # Show some examples of circuits with high differences
  high_diff <- electores_merged[order(-diff_pct)][1:min(5, nrow(electores_merged))]
  cat("\nTop 5 circuits with highest electores differences:\n")
  print(high_diff[, .(circuit_key, cantidadElectores_2023, cantidadElectores_2025, diff_pct)])
  
} else {
  cat("\n=== ISSUE: NO COMPLETE CIRCUITS FOUND ===\n")
  cat("This explains why we have so few circuits for analysis!\n")
  
  # Let's check what's missing
  cat("\nChecking missing parties by circuit...\n")
  
  missing_2023 <- circuits_with_all_parties_2023[n_parties < expected_parties_2023]
  missing_2025 <- circuits_with_all_parties_2025[n_parties < expected_parties_2025]
  
  if(nrow(missing_2023) > 0) {
    cat("Sample circuits missing parties in 2023:\n")
    print(head(missing_2023, 5))
  }
  
  if(nrow(missing_2025) > 0) {
    cat("Sample circuits missing parties in 2025:\n")
    print(head(missing_2025, 5))
  }
}

# STEP 4: Vote totals analysis
cat("\n=== STEP 4: VOTE TOTALS ANALYSIS ===\n")

# Calculate vote totals by circuit for political parties only
vote_totals_2023 <- data_2023_common[, .(total_votes = sum(votos)), by = circuit_key]
vote_totals_2025 <- data_2025_common[, .(total_votes = sum(votos)), by = circuit_key]

cat("2023 vote totals - Min:", min(vote_totals_2023$total_votes), "Max:", max(vote_totals_2023$total_votes), "Mean:", round(mean(vote_totals_2023$total_votes)), "\n")
cat("2025 vote totals - Min:", min(vote_totals_2025$total_votes), "Max:", max(vote_totals_2025$total_votes), "Mean:", round(mean(vote_totals_2025$total_votes)), "\n")

# Check circuits with very low votes
low_vote_thresholds <- c(10, 25, 50, 100, 200)
for(threshold in low_vote_thresholds) {
  low_2023 <- sum(vote_totals_2023$total_votes < threshold)
  low_2025 <- sum(vote_totals_2025$total_votes < threshold)
  cat(sprintf("Circuits with <%d votes: 2023=%d, 2025=%d\n", threshold, low_2023, low_2025))
}

cat("\n=== SUMMARY ===\n")
cat("The main issue appears to be in the circuit overlap and data completeness phases.\n")
cat("We're losing most circuits before even getting to the filtering steps!\n")