# Debug script to understand why we get negative values
library(data.table)
library(dplyr)
library(jsonlite)

cat("Loading data to debug negative values...\n")

# Use same logic as ecological_inference.R
file_2023 <- "electores_circuitos_2023.csv"
file_2025 <- "electores_circuitos_2025.csv"

# Read data
data_2023 <- fread(file_2023)
data_2025 <- fread(file_2025)

cat("Data loaded:\n")
cat("2023:", nrow(data_2023), "rows\n")
cat("2025:", nrow(data_2025), "rows\n")

# Check unique agrupaciones in each year
parties_2023 <- sort(unique(data_2023$nombreAgrupacion))
parties_2025 <- sort(unique(data_2025$nombreAgrupacion))

cat("Parties in 2023:", paste(parties_2023, collapse = ", "), "\n")
cat("Parties in 2025:", paste(parties_2025, collapse = ", "), "\n")

# Define political parties for analysis (excluding only VOTANTES, but including NO_VOTANTES)
political_parties_2023 <- setdiff(parties_2023, c("VOTANTES"))
political_parties_2025 <- setdiff(parties_2025, c("VOTANTES"))

# Add NO_PADRON category for both years
political_parties_2023 <- c(political_parties_2023, "NO_PADRON")
political_parties_2025 <- c(political_parties_2025, "NO_PADRON")

cat("Political parties 2023:", paste(political_parties_2023, collapse = ", "), "\n")
cat("Political parties 2025:", paste(political_parties_2025, collapse = ", "), "\n")

# Filter data to political parties only
data_2023_main <- data_2023[nombreAgrupacion %in% political_parties_2023]
data_2025_main <- data_2025[nombreAgrupacion %in% political_parties_2025]

# Filter to common provinces only
common_provinces <- intersect(unique(data_2023_main$secprov), unique(data_2025_main$secprov))
data_2023_main <- data_2023_main[secprov %in% common_provinces]
data_2025_main <- data_2025_main[secprov %in% common_provinces]

# Get common circuits
circuits_2023 <- unique(data_2023_main$circuitoId)
circuits_2025 <- unique(data_2025_main$circuitoId)
common_circuits <- intersect(circuits_2023, circuits_2025)

cat("Common circuits found:", length(common_circuits), "\n")

# Take first 5 circuits for examples
sample_circuits <- head(common_circuits, 5)

cat("\n=== 5 EXAMPLE CIRCUITS FOR X MATRIX ===\n")

for(circuit in sample_circuits) {
  cat("\n--- Circuit:", circuit, "---\n")
  
  # Get 2023 data for this circuit
  circuit_2023 <- data_2023_main[circuitoId == circuit]
  circuit_2025 <- data_2025_main[circuitoId == circuit]
  
  if(nrow(circuit_2023) > 0 && nrow(circuit_2025) > 0) {
    cantidadElectores_2023 <- unique(circuit_2023$cantidadElectores)[1]
    cantidadElectores_2025 <- unique(circuit_2025$cantidadElectores)[1]
    
    # Use max as common base (like main script does)
    N <- max(cantidadElectores_2023, cantidadElectores_2025)
    
    cat("cantidadElectores 2023:", cantidadElectores_2023, "\n")
    cat("cantidadElectores 2025:", cantidadElectores_2025, "\n")
    cat("Common base N (max):", N, "\n")
    
    # Show X matrix values (2023)
    cat("X matrix values (2023):\n")
    actual_votes_2023 <- 0
    for(party in political_parties_2023[political_parties_2023 != "NO_PADRON"]) {
      votes <- circuit_2023[nombreAgrupacion == party]$votos[1]
      if(is.na(votes)) votes <- 0
      cat("  ", party, ":", votes, "\n")
      actual_votes_2023 <- actual_votes_2023 + votes
    }
    
    cat("  Total actual votes 2023:", actual_votes_2023, "\n")
    
    # Calculate NO_PADRON using common base N (this is where negatives can occur)
    no_padron_2023 <- N - actual_votes_2023
    cat("  NO_PADRON 2023 (using common base N):", no_padron_2023, "\n")
    
    # Check if NO_PADRON is negative
    if(no_padron_2023 < 0) {
      cat("  ⚠️ WARNING: NO_PADRON is negative! Actual votes (", actual_votes_2023, ") > common base N (", N, ")\n")
      cat("  This happens when 2023 has more votes than the max cantidadElectores\n")
    }
    
    # Show row sum using common base
    row_sum <- actual_votes_2023 + no_padron_2023
    cat("  X matrix row sum (with common base):", row_sum, "\n")
    cat("  Equals common base N?", row_sum == N, "\n")
    cat("  Difference from original cantidadElectores 2023:", N - cantidadElectores_2023, "\n")
  }
}