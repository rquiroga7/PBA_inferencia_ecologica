# Quick check for vote mismatches after fixing json_to_csv.R
library(data.table)
library(dplyr)

# Load the corrected data
data_2023 <- fread("electores_circuitos_2023.csv")
data_2025 <- fread("electores_circuitos_2025.csv")

cat("Data loaded:\n")
cat("2023:", nrow(data_2023), "rows\n")
cat("2025:", nrow(data_2025), "rows\n\n")

# Process 2025 data
votes_2025 <- dcast(data_2025, circuitoId + cantidadElectores ~ nombreAgrupacion, 
                   value.var = "votos", fill = 0)

# Calculate NO_PADRON for 2025
political_parties_2025 <- c("EN_BLANCO", "LLA_JxC", "NO_VOTANTES", "OTROS", "UxP")
vote_cols <- intersect(political_parties_2025, colnames(votes_2025))

votes_2025[, total_actual_votes := rowSums(.SD, na.rm = TRUE), .SDcols = vote_cols]
votes_2025[, NO_PADRON := cantidadElectores - total_actual_votes]
votes_2025[, check := total_actual_votes + NO_PADRON - cantidadElectores]

# Check for mismatches
tolerance <- 50
problematic <- votes_2025[abs(check) > tolerance]

cat("Circuits with vote mismatches >", tolerance, "votes:\n")
cat("Total problematic circuits:", nrow(problematic), "\n\n")

if(nrow(problematic) > 0) {
  cat("First 10 problematic circuits:\n")
  for(i in 1:min(10, nrow(problematic))) {
    cat("Circuit", problematic$circuitoId[i], 
        ": votes=", problematic$total_actual_votes[i],
        "+ NO_PADRON=", problematic$NO_PADRON[i],
        "vs cantidadElectores=", problematic$cantidadElectores[i],
        "diff=", problematic$check[i], "\n")
  }
} else {
  cat("SUCCESS: No major vote mismatches found!\n")
}

# Check small mismatches
small_mismatches <- votes_2025[abs(check) > 0 & abs(check) <= tolerance]
cat("\nCircuits with small mismatches (1-", tolerance, "votes):", nrow(small_mismatches), "\n")

# Summary stats
cat("\nSummary of differences:\n")
cat("Perfect matches (diff = 0):", sum(votes_2025$check == 0), "\n")
cat("Small differences (1-", tolerance, "):", nrow(small_mismatches), "\n") 
cat("Major differences (>", tolerance, "):", nrow(problematic), "\n")
cat("Total circuits:", nrow(votes_2025), "\n")